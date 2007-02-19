### PMML: Predictive Modelling Markup Language
##
## Part of the Rattle package for Data Mining
##
## Time-stamp: <2007-02-19 21:33:13 Graham>
##
## Copyright (c) 2007 Graham Williams, Togaware.com, GPL Version 2
##
## TODO
##
##	Extract the DataDictionary stuff to a separate function to
##	share between pmml.rpat and pmml.kmeans.

pmml <- function(model,
                 model.name="Rattle_Model",
                 app.name="Rattle/PMML",
                 description=NULL,
                 copyright=NULL, ...)
  UseMethod("pmml")

pmmlRootNode <- function()
{
  ## PMML
  
  PMML.VERSION <- "3.1"
  return(xmlNode("PMML",
                 attrs=c(version=PMML.VERSION,
                   xmlns="http://www.dmg.org/PMML-3_1", 
                   "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")))
}

pmmlHeader <- function(description, copyright, app.name)
{
  ## Header
  
  VERSION <- "1.0.5"

  if (is.null(copyright))
    header <- xmlNode("Header", attrs=c(description=description))
  else
    header <- xmlNode("Header",
                      attrs=c(copyright=copyright, description=description))

  ## Header -> Extension
  
  header <- append.XMLNode(header,
                           xmlNode("Extension", sprintf("%s", Sys.time()),
                                       attrs=c(description="timestamp")))
  
  header <- append.XMLNode(header, xmlNode("Extension",
                                           sprintf("%s", Sys.info()["user"]),
                                           attrs=c(description="username")))

  ## Header -> Application

  header <- append.XMLNode(header, xmlNode("Application",
                                           attrs=c(name=app.name,
                                             version=VERSION)))

  return(header)
}
  
pmmlDataDictionary <- function(field)
{
  ## field$name is a vector of strings, and includes target
  ## field$class is indexed by fields$names
  ## field$levels is indexed by fields$names
  
  number.of.fields <- length(field$name)

  ## DataDictionary
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    # Determine the operation type

    optype <- "UNKNOWN"
    datype <- "UNKNOWN"
    values <- NULL

    if (field$class[[field$name[i]]] == "numeric")
    {
      optype <- "continuous"
      datype <- "double"
    }
    else if (field$class[[field$name[i]]] == "factor")
    {
      optype <- "categorical"
      datype <- "string"
    }

    ## DataDictionary -> DataField
    
     data.fields[[i]] <- xmlNode("DataField", attrs=c(name=field$name[i],
                                                optype=optype,
                                                dataType=datype))

    ## DataDictionary -> DataField -> Value
    
    if (optype == "categorical")
      for (j in 1:length(field$levels[[field$name[i]]]))
        data.fields[[i]][[j]] <- xmlNode("Value",
                                         attrs=c(value=
                                           field$levels[[field$name[i]]][j]))
  }
  data.dictionary$children <- data.fields

  return(data.dictionary)
    
}

pmmlMiningSchema <- function(field, target=NULL)
{
  number.of.fields <- length(field$name)
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    if (is.null(target))
      usage <- "active"
    else
      usage <- ifelse(field$name[i] == target, "predicted", "active")
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field$name[i],
                                    usageType=usage))
  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}


########################################################################

pmml.rpart <- function(model,
                       model.name="RPart_Model",
                       app.name="Rattle/PMML",
                       description="RPart decision tree model",
                       copyright=NULL, ...)
{
  if (! inherits(model, "rpart")) stop("Not a legitimate rpart object")

  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)

  ## Collect the required information. We list all variables,
  ## irrespective of whether they appear in the final model. This
  ## seems to be the standard thing to do with PMML. It also adds
  ## extra information - i.e., the model did not need these extra
  ## variables!

  field <- NULL
  field$name <- as.character(model$terms@variables)[-1]
  number.of.fields <- length(field$name)
  field$class <- model$terms@dataClasses
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- model@ylevels
      else
        field$levels[[field$name[i]]] <- model@xlevels[[field$name[i]]]
  }
  
  ## PMML

  pmml <- pmmlRootNode()

  ## PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007 Graham.Williams@Togaware.com"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  ## PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  ## PMML -> TreeModel

  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="rpart",
                          splitCharacteristic="binarySplit"))
  
  
  ## PMML -> TreeModel -> MiningSchema
  
  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  ## PMML -> TreeModel -> Node

  depth <- rpart:::tree.depth(as.numeric(row.names(model$frame)))
  count <- model$frame$n
  score <- model@ylevels[model$frame$yval]
  label <- labels(model, pretty=0)

  field <- label[1]
  operator <- ""
  value <- "" #list("")
  for (i in 2:length(label))
  {
    field <-  c(field, strsplit(label[i], '>|<|=')[[1]][1])
    op <- substr(label[i], nchar(field[i])+1, nchar(field[i])+2)
    if (op == ">=")
    {
      operator <- c(operator, "greaterOrEqual")
      value <- c(value, substr(label[i], nchar(field[i])+3, nchar(label[i])))
    }
    else if (op == "< ")
    {
      operator <- c(operator, "lessThan")
      value <- c(value, substr(label[i], nchar(field[i])+3, nchar(label[i])))
    }
    else if (substr(op, 1, 1) == "=")
    {
      operator <- c(operator, "isIn")
      value <- c(value, substr(label[i], nchar(field[i])+2, nchar(label[i])))
    }
  }
  
  node <- genBinaryTreeNodes(depth, count, score, field, operator, value)

  ## tree.model[[2]] <- node
  tree.model <- append.XMLNode(tree.model, node)

  ## Add to the top level structure.
  
  ## pmml$children[[3]] <- tree.model
  pmml <- append.XMLNode(pmml, tree.model)

  return(pmml)
}

genBinaryTreeNodes <- function(depths, counts, scores, fields, ops, values)
{
  depth <- depths[1]
  count <- counts[1]
  score <- scores[1]
  field <- fields[1]
  op <- ops[1]
  value <- values[1]

  node <- xmlNode("Node", attrs=c(score=score, recordCount=count))
  if (field == "root")
    predicate <- xmlNode("True")
  else
  {
    # Create the SimplePredicate or SimpeSetPredicate node.

    if (op[1] %in% c("greaterOrEqual", "lessThan"))
      predicate <- xmlNode("SimplePredicate", attrs=c(field=field,
                                                operator=op, value=value))
    else if (op[1] == "isIn")
    {
      predicate <- xmlNode("SimpleSetPredicate", attrs=c(field=field,
                                                   operator=op))
      value <- strsplit(value[[1]], ",")[[1]]
      # Do we need quotes around the values?
      # vals <- paste('"', value, '"', collapse=" ", sep="")
      vals <- paste(value, collapse=" ", sep="")
      ##predicate[[1]] <- xmlNode("Array",
      ##                          vals,
      ##                          attrs=c(n=length(value), type="string"))
      predicate <- append.XMLNode(predicate,
                                  xmlNode("Array",
                                          vals,
                                          attrs=c(n=length(value),
                                            type="string")))
    }
  }
  if (length(depths) == 1)
  {
    left <- NULL
    right <- NULL
  }
  else
  {
    split.point <- which(depths[c(-1,-2)] == depths[2]) + 1 # Binary tree
    lb <- 2:split.point
    rb <- (split.point + 1):length(depths)
    left <- genBinaryTreeNodes(depths[lb], counts[lb], scores[lb],
                               fields[lb], ops[lb], values[lb])
    right <- genBinaryTreeNodes(depths[rb], counts[rb], scores[rb],
                                fields[rb], ops[rb], values[rb])
  }
  ## node[[1]] <- predicate
  node <- append.XMLNode(node, predicate)
  if (!is.null(left))
  {
    ##node[[2]] <- left
    ##node[[3]] <- right
    node <- append.XMLNode(node, left)
    node <- append.XMLNode(node, right)
  }
  return(node)
}

########################################################################

pmml.rpart.as.rules <- function(model,
                                model.name="RPart_Model",
                                app.name="RPart",
                                description="RPart model as rules",
                                copyright=NULL)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)
  
  if (! inherits(model, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information

  field <- NULL
  field$name <- as.character(model$terms@variables)[-1]
  number.of.fields <- length(field$name)
  field$class <- model$terms@dataClasses
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- model@ylevels
      else
        field$levels[[field$name[i]]] <- model@xlevels[[field$name[i]]]
  }

  ## PMML
  
  pmml <- pmmlRootNode()

  ## PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007 Graham.Williams@Togaware.com"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  ## PMML -> DataDictionary
  
  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  ## PMML -> RuleSetModel
  
  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          splitCharacteristic="binary",
                          algorithmName="rpart"))

  ## MiningSchema
  
  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  ## Add in actual tree nodes.

  rule.set <- xmlNode("RuleSet")
  rule.set <- append.XMLNode(rule.set,
                             xmlNode("RuleSelectionMethod",
                                     attrs=c(criterion="firstHit")))
  
  ## Visit each leaf node to generate a rule.

  ordered <- rev(sort(model$frame$yval2[,5], index=TRUE)$ix)
  names <- row.names(model$frame)
  next.child <- 2
  for (i in ordered)
  {
    if (model$frame[i,1] == "<leaf>")
    {
      simple.rule <- xmlNode("SimpleRule",
                             attrs=c(id=sprintf("R%03d", as.integer(names[i])),
                               recordCount=model$frame[i,]$n))
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      pth <- unlist(pth)[-1]
      if (length(pth) != 0)
      {
        predicate <- xmlNode("CompoundPredicate",
                             attrs=c(booleanOperator="and"))
        for (p in (1:length(pth)))
        {
          f <- unlist(strsplit(pth[p], "<|>=|="))[[1]]
          o <- ifelse(length(grep("<", pth[p]))>0, "lessThen",
               ifelse(length(grep(">=", pth[p]))>0, "greaterOrEqual",
               ifelse(length(grep("=", pth[p]))>0, "equal", "DONTKNOW")))
          v <- unlist(strsplit(pth[p], "<|>=|="))[[2]]
          predicate$children[[p]] <- xmlNode("SimplePredicate",
                                             attrs=c(field=f,
                                               operator=o,
                                               value=v))
        }
      }
      simple.rule$children[[1]] <- predicate
      rule.set$children[[next.child]] <- simple.rule
      next.child <- next.child + 1
    }
  }

  tree.model <- append.XMLNode(tree.model, rule.set)
  
  ## Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, tree.model)
  
  return(pmml)
}

########################################################################

pmml.kmeans <- function(model,
                        model.name="KMeans_Model",
                        app.name="Rattle/PMML",
                        description="KMeans cluster model",
                        copyright=NULL, ...)
{
  require(XML, quietly=TRUE)
  
  if (! inherits(model, "kmeans")) stop("Not a legitimate kmeans object")

  ## Collect the required information.

  field <- NULL
  field$name <-  colnames(model$centers)
  number.of.fields <- length(field$name)

  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name

  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  ## PMML

  pmml <- pmmlRootNode()

  ## PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007 Graham.Williams@Togaware.com"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  ## PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  ## PMML -> ClusteringModel

  cl.model <- xmlNode("ClusteringModel",
                      attrs=c(modelName=model.name,
                        functionName="clustering",
                        algorithmName="KMeans",
                        modelClass="centerBased",
                        numberOfClusters=number.of.clusters))

  ## PMML -> ClusteringModel -> MiningSchema

  cl.model <- append.XMLNode(cl.model, pmmlMiningSchema(field))

  ## PMML -> ClusteringModel -> ComparisonMeasure
  
  cl.model <- append.XMLNode(cl.model, xmlNode("ComparisonMeasure",
                                               attrs=c(kind="distance")))
  
  ## PMML -> ClusteringModel -> Cluster -> Array
  
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    cl.model <- append.XMLNode(cl.model,
                               xmlNode("Cluster",
                                       attrs=c(name=cluster.names[i],
                                         size=model$size[i]),
                                       xmlNode("Array",
                                               attrs=c(n=number.of.fields),
                                               paste(model$centers[i,],
                                                     collapse=" "))))
  }
  pmml <- append.XMLNode(pmml, cl.model)

  return(pmml)
}

########################################################################

pmml.rsf <- function(model,
                     model.name="rsfForest_Model",
                     app.name="Rattle/PMML",
                     description="Random Survival Forest Tree Model",
                     copyright=NULL, ...)
{
  ## Based on RANDOM SURVIVAL FOREST 2.0.0, Copyright 2006, Cleveland Clinic
  ## Original by Hemant Ishwaran and Udaya B. Kogalur
  ## UInified with the pmml package by Graham Williams
  
  if (sum(inherits(model, c("rsf", "forest"), TRUE) == c(1, 2)) != 2)
    stop("Not a legitimate (rsf, forest) object")

  require(XML, quietly=TRUE)
  require(randomSurvivalForest, quietly=TRUE)

  ## Collect the required information.

  field <- NULL

  field$name <- model$predictorNames
  if (is.null(field$name))
    stop("RSF predictorNames is NULL.  Please ensure the object is valid.")
  number.of.fields <- length(field$name)

  field$class <- rep("numeric", number.of.fields) # All fields are numeric? 
  names(field$class) <- field$name

  nativeArray <- model$nativeArray
  if (is.null(nativeArray))
    stop("RSF nativeArray content is NULL. Please ensure object is valid.")
    
  numTrees <- length(as.vector(unique(nativeArray$treeID))) # Trees in forest
  
  timeInterest = model$timeInterest
  if (is.null(timeInterest))
    stop("RSF timeInterest content is NULL. Please ensure object is valid.")

  formula = model$formula
  if (is.null(formula))
    stop("RSF formula is NULL.  Please ensure the object is valid.")

  bootstrapSeed = model$bootstrapSeed
  if (is.null(bootstrapSeed))
    stop("RSF bootstrapSeed content is NULL.  Please ensure object is valid.")

  ## PMML

  pmml <- pmmlRootNode()

  ## PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright 2006, Cleveland Clinic"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  ## PMML -> MiningBuildTask

  buildNode <- xmlNode("MiningBuildTask")

  ## PMML -> MiningBuildTask -> Extension
  
  extensionNode <- xmlNode("Extension")

  ## PMML -> MiningBuildTask -> Extension -> X-RSF-Formula

  extensionNode <- append.XMLNode(extensionNode,
                                  xmlNode("X-RSF-Formula",
                                          attrs=c(name=formula)))

  ## PMML -> MiningBuildTask -> Extension -> X-RSF-BootstrapSeeds -> Array
    
  extensionNode <- append.XMLNode(extensionNode, 
                                  xmlNode("X-RSF-BootstrapSeeds", 
                                          xmlNode("Array", 
                                                  attrs=c(type="integer",
                                                    n=length(bootstrapSeed)), 
                                                  paste(bootstrapSeed,
                                                        collapse="  \n  "))))

  ## PMML -> MiningBuildTask -> Extension -> TimesOfInterest

  extensionNode <- append.XMLNode(extensionNode, 
                                  xmlNode("X-RSF-TimesOfInterest", 
                                          xmlNode("Array", 
                                                  attrs=c(type="double",
                                                    n=length(timeInterest)), 
                                                  paste(timeInterest,
                                                        collapse="  \n  "))))
  
  ## Add into the PMML.

  pmml <- append.XMLNode(pmml, append.XMLNode(buildNode, extensionNode))
  
  ## PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))
  
  ## Create a dummy XML node object to insert into the recursive
  ## output object.

  internalNode <- xmlNode("Null")
  
  ## Define the variables for the offset and leaf count in the
  ## recursive output object.

  offset <- leafCount <- 1
  
  ## Create the recursive output object.  This would be unnecessary if
  ## it was possible to declare global variables in a package.

  recursiveOutput <- list(internalNode = internalNode,
                          offset = offset, leafCount = leafCount)
  
  ## Loop through all trees in the forest and extract the data.

  for (b in 1:numTrees)
  {
    treeModelNode <- xmlNode("TreeModel",
                             attrs=c(modelName=b, functionName="prediction",
                               algorithmName="rsf",
                               splitCharacteristic="binary"))

    ## PMML -> TreeModel [b] -> MiningSchema
    
    treeModelNode <- append.XMLNode(treeModelNode, pmmlMiningSchema(field))
    
    ## Global dependencies: (field$name, forest)
    
    ## Initialize the root node.  This differs from the rest of the
    ## internal nodes in the PMML structure.

    treeRoot <- xmlNode("Node", attrs=c(score=0, id=1))
    treeRoot <- append.XMLNode(treeRoot, xmlNode("True"))
    
    rootParmID <- nativeArray$parmID[recursiveOutput$offset] 
    rootSpltPT <- nativeArray$spltPT[recursiveOutput$offset]
    
    recursiveOutput$offset <- recursiveOutput$offset + 1
    recursiveOutput$leafCount <- 1
    
    ## Check that the current tree is not a stump (root node only with
    ## no branches)

    if (rootParmID != 0)
    {
      # The tree must be created in two phases.  First, the root left
      # daughter branches are created.  Second, the root right
      # daughter branches are created.  This is due to the root node
      # having a slightly different structure using the PMML protocol.
      # The root node actually has no split information.  The split
      # information is encoded into the daughter nodes.  Thus, instead
      # of making a check for the root node in the recursive routine,
      # we call the recursive routine twice.
      
      # Create the left daughter nodes.  Note that the object node
      # content is irrelevant as input.

      recursiveOutput$internalNode <- NULL
      recursiveOutput <- rsfMakeTree(recursiveOutput, nativeArray,
                                     field$name, b, -1, rootParmID,
                                     rootSpltPT)
      
      treeRoot <- append.XMLNode(treeRoot, recursiveOutput$internalNode)
      
      recursiveOutput$leafCount <- recursiveOutput$leafCount + 1
      
      # Create the right daughter nodes.  Note that the object node
      # content is irrelevant as input.

      recursiveOutput$internalNode <- NULL
      recursiveOutput <- rsfMakeTree(recursiveOutput, nativeArray,
                                     field$name, b, +1, rootParmID,
                                     rootSpltPT)
      
      treeRoot <- append.XMLNode(treeRoot, recursiveOutput$internalNode)
      
    }
    
    ## Add the current tree to the PMML data structure.

    treeModelNode <- append.XMLNode(treeModelNode, treeRoot)
    pmml <- append.XMLNode(pmml, treeModelNode)
  }
  
  return (pmml)
}

rsfMakeTree <- function(recursiveObject, nativeArray, predictorNames,
                        b, daughter, splitParameter, splitValue)
{
  # Node information encoded in a PMML TreeModel follows a slightly
  # different protocol than that encoded in our RSF matrix
  # representation.  Since the RSF representation is linear in
  # nature, each record containing node information must encode the
  # split information, particularly the split parameter and split
  # point, in the record itself.  In contrast, the PMML TreeModel
  # indicates a split by the presence of daughters in the node.  The
  # split parameter and split point are encoded by a SimplePredicate
  # tag in the daughters.  In creating a PMML tree from an RSF tree,
  # the recursive algorithm requires a "look back" to the previous
  # record in the RSF tree to determine the split parameter and
  # value.  This is accomplished via the parameters passed by the
  # parent call to this routine.
  
  # Weak consistency check to ensure that the iteration matches the
  # treeID in the nativeArray record.

  if(b != nativeArray$treeID[recursiveObject$offset])
    stop("Invalid nativeArray input record (treeID) at ",
         recursiveObject$offset, ".  Please contact Technical Support.")

  # Read the current nativeArray record, and determine whether this is
  # a terminal node.

  fwdSplitParameter <- nativeArray$parmID[recursiveObject$offset]
  fwdSplitValue <- nativeArray$spltPT[recursiveObject$offset]

  # Create the node that will be returned on this call.

  if (fwdSplitParameter == 0)
  {
    rsfNode <- xmlNode("Node",
                       attrs=c(score=1,
                         id=nativeArray$nodeID[recursiveObject$offset]))
    terminalFlag <- TRUE
  }
  else if (fwdSplitParameter != 0)
  {
    rsfNode <- xmlNode("Node",
                       attrs=c(score=0,
                         id=nativeArray$nodeID[recursiveObject$offset])) 
    terminalFlag <- FALSE
  }
   
  # Determine whether this the left of right daughter.

  if (daughter == -1)
    parseString <- "lessOrEqual"
  else if (daughter == +1)
    parseString <- "greaterThan"
  else
    # Ensure that the function call is coherent to aid in debugging.
    stop("Invalid parse direction encountered during recursion.",
         "Please contact Technical Support.")

  # Add the split information to this node via the look back.

  rsfNode <- append.XMLNode(rsfNode,
                            xmlNode("SimplePredicate",
                                  attrs=c(field=predictorNames[splitParameter],
                                    operator=parseString, value=splitValue)))

  # Increment the offset, always.

  recursiveObject$offset <- recursiveObject$offset + 1

  # Parse left and then right, if this is not a terminal node.

  if (terminalFlag == FALSE)
  {
    # Parse left: Do not increment the leafCount.  Internally
    # increment the offset, always.  Note that the object node content
    # is irrelevant as input.

    recursiveObject$internalNode <- NULL
    recursiveObject <- rsfMakeTree(recursiveObject, nativeArray,
                                   predictorNames, b, daughter = -1,
                                   fwdSplitParameter, fwdSplitValue)
    
    rsfNode <- append.XMLNode(rsfNode, recursiveObject$internalNode)
    
    # Parse right: Increment the leafCount.  Internally increment the
    # offset, always.  Note that the object node content is irrelevant
    # as input.
    
    recursiveObject$leafCount <- recursiveObject$leafCount + 1
    recursiveObject$internalNode <- NULL
    recursiveObject <- rsfMakeTree(recursiveObject, nativeArray,
                                   predictorNames, b, daughter = +1,
                                   fwdSplitParameter, fwdSplitValue)
    
    rsfNode <- append.XMLNode(rsfNode, recursiveObject$internalNode)
    
  }
  
  # Modify the recursive object with the new internal node structure.

  recursiveObject$internalNode <- rsfNode
  
  return (recursiveObject)
  
}


########################################################################

sql.pmml <- function(pmml)
{
  ## pmml <- xmlTreeParse("../TARGET.rpart.xml")
  ## pmml <- xmlTreeParse("../TARGET.rpart.xml", useInternalNodes=TRUE)

  root <- xmlRoot(pmml)
  children <- xmlChildren(root)

  if ("TreeModel" %in% names(children))
    sql <- generateTreeModelSQL(children$TreeModel)
  else if ("ClusteringModel" %in% names(children))
    sql <- generateClusteringModelSQL(children$TreeModel)
  else
    sql <- "SELECT * FROM *"

  return(sql)
}

generateTreeModelSQL <- function(tree)
{
  nodes <- xmlChildren(tree)$Node

  return("SELECT TREE")
}

generateClusteringModelSQL <- function(tree)
{
  return("SELECT CLUSTER NOT YET IMPLEMENTED")
}
