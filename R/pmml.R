### PMML: Predictive Modelling Markup Language
##
## Part of the Rattle package for Data Mining
##
## Time-stamp: <2007-02-10 16:27:09 Graham>
##
## Copyright (c) 2007 Graham Williams, Togaware.com, GPL Version 2
##
## TODO
##
##	Extract the DataDictionary stuff to a separate function to
##	share between pmml.rpat and pmml.kmeans.

PMML.VERSION <- "3.1"
VERSION <- "1.0.1"

pmml.rpart <- function(model,
                       model.name="RPart Model",
                       app.name="Rattle/PMML",
                       description="RPart decision tree model",
                       copyright=NULL)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)

  if (! inherits(model, "rpart")) stop("Not a legitimate rpart object")

  ## Collect the required information. We list all variables,
  ## irrespective of whether they appear in the final model. This
  ## seems to be the standard thing to do with PMML. It also adds
  ## extra information - i.e., the model did not need these extra
  ## variables!

  field.names <- as.character(model$terms@variables)[-1]
  target <- field.names[1]
  number.of.fields <- length(field.names)
  
  ## PMML
  
  pmml <- xmlNode("PMML",
                  attrs=c(version=PMML.VERSION,
                    xmlns="http://www.dmg.org/PMML-3_1", 
                    "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance"))

  ## PMML -> Header

  if (is.null(copyright))
    header <- xmlNode("Header", attrs=c(description=description))
  else
    header <- xmlNode("Header",
                      attrs=c(copyright=copyright, description=description))
  
  header[[1]] <- xmlNode("Application",
                         attrs=c(name=app.name,
                           version=VERSION))

  header[[2]] <- xmlNode("Annotation",
                         paste("Export of PMML from Rattle for",
                               "RPart models is experimental."))
  header[[2]][[2]] <- xmlNode("Extension", sprintf("%s", Sys.time()),
                              attrs=c(description="timestamp"))
  header[[2]][[3]] <- xmlNode("Extension", sprintf("%s", Sys.info()["user"]),
                              attrs=c(description="username"))
  
  pmml$children[[1]] <- header

  ## PMML -> DataDictionary
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    # Determine the operation type

    optype <- "UNKNOWN"
    values <- NULL

    if (model$terms@dataClasses[[field.names[i]]] == "numeric")
      optype <- "continuous"
    else if (model$terms@dataClasses[[field.names[i]]] == "factor")
      optype <- "categorical"

    ## PMML -> DataDictionary -> DataField
    
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i],
                                  optype=optype))

    ## PMML -> DataDictionary -> DataField -> Value
    
    if (optype == "categorical")
    {
      if (field.names[i] == target)
        clevels <- model@ylevels
      else
        clevels <- model@xlevels[[field.names[i]]]
      for (j in 1:length(clevels))
        data.fields[[i]][[j]] <- xmlNode("Value", attrs=c(value=clevels[j]))
    }
  }
  data.dictionary$children <- data.fields
  pmml$children[[2]] <- data.dictionary

  ## PMML -> TreeModel

  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="rpart",
                          splitCharacteristic="binarySplit"))
  
  
  ## PMML -> TreeModel -> MiningSchema
  
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    usage <- ifelse(field.names[i] == target, "predicted", "active") 
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field.names[i],
                                    usageType=usage))
  }

  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  tree.model[[1]] <- mining.schema

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

  tree.model[[2]] <- node

  ## Add to the top level structure.
  
  pmml$children[[3]] <- tree.model

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
      predicate[[1]] <- xmlNode("Array",
                                vals,
                                attrs=c(n=length(value), type="string"))
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
  node[[1]] <- predicate
  if (!is.null(left))
  {
    node[[2]] <- left
    node[[3]] <- right
  }
  return(node)
}

pmml.rpart.as.rules <- function(model, model.name="RPart Model", app.name="RPart",
                                copyright=NULL)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)
  
  if (! inherits(model, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information

  field.names <- as.character(model$frame$var) # GET UNIQUE LIST.....
  field.names <- setdiff(union(field.names, field.names), "<leaf>")
  number.of.fields <- length(field.names)
  tree.nodes <- rownames(model$frame)
  rule.paths <- path.rpart(model, node=c(tree.nodes), print.it=FALSE)
  
  ## Root node
  
  pmml <- xmlNode("PMML", attrs=c(version="3.1"))

  ## Header

  header <- xmlNode("Header",
                    attrs=c(copyright=copyright))
  header[[1]] <- xmlNode("Application",
                         attrs=c(name=app.name,
                           version=VERSION,
                           timestamp=sprintf("%s", Sys.time()),
                           username=sprintf("%s", Sys.info()["user"])))

  header[[2]] <- xmlNode("Annotation",
                         "Export of PMML for RPart models is experimental.")
  
  pmml$children[[1]] <- header
  
  ## DataDictionary child node
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    ## Determine the operation type

    optype <- "UNKNOWN"
    values <- NULL

    if (model$terms@dataClasses[[field.names[i]]] == "numeric")
      optype <- "continuous"
    else if (model$terms@dataClasses[[field.names[i]]] == "factor")
    {
      optype <- "categorical"
      for (j in 1:length(model@xlevels[[field.names[i]]]))
      {
        ## Build up the Values list of elements!
        values <- model@xlevels[[field.names[i]]][j]
      }
    }
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i],
                                  optype=optype))
  }
  data.dictionary$children <- data.fields
  pmml$children[[2]] <- data.dictionary

  ## Tree Node: Generate a rule set for now - simpler that a decision
  ## tree.
  
##   tree.model <- xmlNode("TreeModel",
##                         attrs=c(modelName=model.name,
##                           functionName="classification",
##                           splitCharacteristic="binary",
##                           algorithmName="rpart"))

  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          splitCharacteristic="binary",
                          algorithmName="rpart"))

  ## Mining Schema
  
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field.names[i],
                                    usageType="active"))
  }
  target <- attr(model$terms,"variables")[[2]]
  mining.fields[[i+1]] <- xmlNode("MiningField",
                                  attrs=c(name=target,
                                    usageType="predicted"))

  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  tree.model[[1]] <- mining.schema

  ## Add in actual tree nodes.

  rule.set <- xmlNode("RuleSet")
  rule.set$children[[1]] <- xmlNode("RuleSelectionMethod",
                                    attrs=c(criterion="firstHit"))
  
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

  tree.model[[2]] <- rule.set
  
  ## Add to the top level structure.
  
  pmml$children[[3]] <- tree.model
  
  return(pmml)
}

pmml.kmeans <- function(model,
                        model.name="KMeans Model",
                        app.name="Rattle/PMML",
                        description="KMeans cluster model",
                        copyright=NULL)
{
  require(XML, quietly=TRUE)
  
  if (! inherits(model, "kmeans")) stop("Not a legitimate kmeans object")

  ## Collect the required information.

  number.of.fields <- ncol(model$centers)
  field.names <-  colnames(model$centers)
  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  ## PMML

  pmml <- xmlNode("PMML",
                  attrs=c(version=PMML.VERSION,
                    xmlns="http://www.dmg.org/PMML-3_1", 
                    "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance"))

  ## PMML -> Header

  if (is.null(copyright))
    header <- xmlNode("Header", attrs=c(description=description))
  else
    header <- xmlNode("Header",
                      attrs=c(copyright=copyright, description=description))

  header[[1]] <- xmlNode("Application",
                         attrs=c(name=app.name,
                           version=VERSION))

  header[[2]] <- xmlNode("Annotation",
                         paste("Export of PMML from Rattle for",
                               "KMeans models is experimental."))
  header[[2]][[2]] <- xmlNode("Extension", sprintf("%s", Sys.time()),
                              attrs=c(description="timestamp"))
  header[[2]][[3]] <- xmlNode("Extension", sprintf("%s", Sys.info()["user"]),
                              attrs=c(description="username"))
  
  pmml$children[[1]] <- header

  ## PMML -> DataDictionary

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i]))
  }
  data.dictionary$children <- data.fields
  pmml$children[[2]] <- data.dictionary

  ## PMML -> ClusteringModel

  clustering.model <- xmlNode("ClusteringModel",
                              attrs=c(algorithmName="KMeans",
                                numberOfClusters=number.of.clusters))
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    clusters[[i]] <- xmlNode("Cluster",
                             attrs=c(name=cluster.names[i],
                               size=model$size[i]),
                             xmlNode("Array",
                                     attrs=c(n=number.of.fields),
                                     paste(model$centers[i,], collapse=" ")))
  }
  clustering.model$children <- clusters
  pmml$children[[3]] <- clustering.model
  #
  # All done
  #
  return(pmml)
}

