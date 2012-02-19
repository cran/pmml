# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2012-02-19 12:20:58 Graham Williams>
#
# Copyright (c) 2009 Togaware Pty Ltd
#
# This files is part of the Rattle suite for Data Mining in R.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

########################################################################

pmml.randomForest <- function(model,
                              model.name="randomForest_Model",
                              app.name="Rattle/PMML",
                              description="randomForest model",
                              copyright=NULL,
                              ...)

{
  if (! inherits(model, "randomForest"))
    stop("Not a legitimate randomForest object")

  require(XML, quietly=TRUE)
  require(randomForest, quietly=TRUE)

  # Collect the required information. We list all variables,
  # irrespective of whether they appear in the final model. This
  # seems to be the standard thing to do with PMML. It also adds
  # extra information - i.e., the model did not need these extra
  # variables!
  #
  # For a randomForest formula as currently used in Rattle, the
  # target is, for example, as.factor(Adjusted). Here, I need to
  # remove the as.factor(...). I wonder if I need to identify a
  # transformation in the PMML.

  field <- NULL
  tr.vars <- attr(model$terms, "dataClasses")
  var.names <- unlist(lapply(names(tr.vars),
                             function(x) gsub("as.factor\\((\\w*)\\)",
                                              "\\1", x, perl=TRUE)))

  field$name <- var.names
  number.of.fields <- length(field$name)
  target <- var.names[1]

  # The following is a bit sus and does not really get the corect type
  # of the as.factor modified fields!
  # Tridi 2/8/12: modified to get category names correctly
  field$class <- attr(model$terms, "dataClasses")
  names(field$class) <- var.names

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      if (field$name[i] == target){
        field$levels[[field$name[i]]] <- model$classes
      }
      else {
          cat <- model$forest$xlevels[i][[1]]
          field$levels[[field$name[i]]] <- cat
      }
    }
  }

  # PMML

  pmml <- pmmlRootNode("3.2")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  mmodel <- xmlNode("MiningModel",attrs=c(modelName=model.name,functionName=model$type))
  mmodel <- append.XMLNode(mmodel,pmmlMiningSchema(field, target))

  if(model$type == "regression") {
    segmentation <- xmlNode("Segmentation",attrs=c(multipleModelMethod="average"))
  } 
  if(model$type == "classification") {
    segmentation <- xmlNode("Segmentation",attrs=c(multipleModelMethod="majorityVote"))
  }

  numTrees <- model$ntree
  for(b in 1:numTrees)
  {
    segment <- xmlNode("Segment",attrs=c(id=b))
    tru <- xmlNode("True")
    segment <- append.XMLNode(segment, tru)

  # PMML -> TreeModel
    if(model$type == "regression") {
          tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          algorithmName="randomForest",
                          splitCharacteristic="binarySplit"))
    }
    if(model$type == "classification") {
  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="randomForest",
                          splitCharacteristic="binarySplit"))
    }

  # PMML -> TreeModel -> MiningSchema

  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  # PMML -> TreeModel -> Node
    if(model$type == "regression") {
      tree <- cbind(model$forest$leftDaughter[,b],
                    model$forest$rightDaughter[,b],
                    model$forest$bestvar[,b],
                    model$forest$xbestsplit[,b],
                    model$forest$nodestatus[,b],
                    model$forest$nodepred[,b])[1:model$forest$ndbigtree[b],]
    } else {
      tree <- cbind(model$forest$treemap[,,b],
                    model$forest$bestvar[,b],
                    model$forest$xbestsplit[,b],
                    model$forest$nodestatus[,b],
                    model$forest$nodepred[,b])[1:model$forest$ndbigtree[b],]
    }
     internalNode <- xmlNode("Null")
     recursiveOutput <- list(internalNode = internalNode)
     recursiveOutput$internalNode <- NULL

  # Basic algorithm: Given node, add left leaf and then right leaf. Recursive algorithm as 
  # at each leaf, again add left leaf and then right leaf.
     recursiveOutput <- getRFTreeNodes2(recursiveOutput, model, -1, tree, 1, 1)


  # Add to the top level structure.
     tree.model <- append.XMLNode(tree.model, recursiveOutput$internalNode)
     segment <- append.XMLNode(segment, tree.model)
     segmentation <- append.XMLNode(segmentation, segment)
  }
  mmodel <- append.XMLNode(mmodel, segmentation)
  pmml <- append.XMLNode(pmml, mmodel)
  
  return(pmml)
}

getRFTreeNodes2 <- function(recursiveObject, model, side, tinf, rowfrom, rownext)
{
  if(!((model$type == "regression") || (model$type == "classification")))
     print("Model type not supported")

 # Keep going over nodes; if leaf node, add score, else split and keep going
  if((rowfrom == 1) && (rownext == 1)) {
 # Add top node at first loop only
    rfNode <- xmlNode("Node",attrs=c(id="1"))
    nodeB <- xmlNode("True")
    rfNode <- append.XMLNode(rfNode,nodeB)
  } else {
      fname <- attributes(model$forest$xlevels[tinf[rowfrom,3]])[[1]]
 # Treat left and right leafs separately as their information is stored in separate column in tree
      if(side==-1){
        if(tinf[rownext,1] == 0) {
 # The score for classification must be translated from a number to the category name
          if(model$type == "regression") {
 # The score for regresion can just be read off.
            rfNode <- xmlNode("Node",attrs=c(id=tinf[rowfrom,1],score=tinf[rownext,6]))
          } else {
            rfNode <- xmlNode("Node",attrs=c(id=tinf[rowfrom,1],score=model$classes[tinf[rownext,6]]))
         }
        } else
        {
          rfNode <- xmlNode("Node",attrs=c(id=tinf[rowfrom,1]))
        } 
 # After the node, add the split info in pmml
 # --------------------------------------------------------------------------------------------- 
  # left side, regression model, terminal node 
          # is the field categorical
          if(is.numeric(model$forest$xlevels[tinf[rowfrom,3]][[1]][1])) {
	    numeric <- TRUE
          } else {
            numeric <- FALSE
          }
  # split if var is numeric
          if(numeric) { 
            splitNode <- xmlNode("SimplePredicate",attrs=c(field=fname,operator="lessOrEqual",value=tinf[rowfrom,4]))
          } else {
  # split if var is categorical
            binary <- sdecimal2binary(tinf[rowfrom,4])
            ssp <- xmlNode("SimpleSetPredicate",attrs=c(field=fname,booleanOperator="isIn"))
            num1 <- 0
            scat <- NULL
           holder <- array(0,dim=c(1,model$forest$ncat[fname][[1]]))
            for(k in 1:length(binary)){
              holder[k] = binary[k]
            }

 # for each category allowed, if value is 1 (from the binary conversion) then go left
            for(k in 1:model$forest$ncat[fname][[1]]) {
              if(holder[k]==1){
                num1 <- num1 + 1
                catname <- as.character(unlist(model$forest$xlevels[fname]))[k]
               catname <- model$forest$xlevels[fname][[1]][k]
                scat <- paste(scat," ",catname)
              }
            }
             
 # all the gsubs are to strip intermediate, leading and trailing spaces. 
            scat <- gsub("^[ ]*","",scat)
            ap <- xmlNode("Array",attrs=c(n=num1,type="string"),scat)
            ssp <- append.XMLNode(ssp,ap)
            splitNode <- ssp
          }
          rfNode <- append.XMLNode(rfNode,splitNode)
        } 
      else {
 # --------------------------------------------------------------------------------------------
  # right side, regression, terminal node
 # repeat all over for right side 
        if(tinf[rownext,1] == 0) {
          if(model$type == "regression") {
 # The only difference is where to read off the node info from the tree structure 
            rfNode <- xmlNode("Node",attrs=c(id=tinf[rowfrom,2],score=tinf[rownext,6]))
          } else {
            rfNode <- xmlNode("Node",attrs=c(id=tinf[rowfrom,2],score=model$classes[tinf[rownext,6]]))
         }
        }
        else
        {
          rfNode <- xmlNode("Node",attrs=c(id=tinf[rowfrom,2]))
        }
          # is the field categorical
	  if(is.numeric(model$forest$xlevels[tinf[rowfrom,3]][[1]][1])) { 
            numeric <- TRUE
          } else {
            numeric <- FALSE
          }
  # split if var is numeric
          if(numeric) {
            splitNode <- xmlNode("SimplePredicate",attrs=c(field=fname,operator="greaterThan",value=tinf[rowfrom,4]))
          } else {
  # split if var is categorical
            binary <- sdecimal2binary(tinf[rowfrom,4])
            ssp <- xmlNode("SimpleSetPredicate",attrs=c(field=fname,booleanOperator="isIn"))
            num1 <- 0
            scat <- NULL
            holder <- array(0,dim=c(1,model$forest$ncat[fname][[1]]))
            for(k in 1:length(binary)){
              holder[k] = binary[k]
            }
            for(k in 1:model$forest$ncat[fname][[1]]) {
              if(holder[k]==0){
                num1 <-  num1 + 1
                catname <- as.character(unlist(model$forest$xlevels[fname]))[k]
                scat <- paste(scat," ",catname)
              }
            }

            scat <- gsub("^[ ]*","",scat)
            ap <- xmlNode("Array",attrs=c(n=num1,type="string"),scat)
            ssp <- append.XMLNode(ssp,ap)
            splitNode <- ssp
          }
          rfNode <- append.XMLNode(rfNode,splitNode)
        } 
    } 

  if(tinf[rownext,5] == -1)
  {
    terminalFlag <- TRUE
  } else {
    terminalFlag <- FALSE
  }


  if (terminalFlag == TRUE) {
#    only the predicted value for this node is the output
  }
  if(terminalFlag == FALSE) 
  {
    recursiveObject$internalNode <- NULL
    recursiveObject <- getRFTreeNodes2(recursiveObject,model,-1,tinf,rownext,tinf[rownext,1])

    rfNode <- append.XMLNode(rfNode, recursiveObject$internalNode)

    recursiveObject$internalNode <- NULL
    recursiveObject <- getRFTreeNodes2(recursiveObject,model,1,tinf,rownext,tinf[rownext,2])

    rfNode <- append.XMLNode(rfNode, recursiveObject$internalNode)
  }

  recursiveObject$internalNode <- rfNode
  return(recursiveObject)
}

genBinaryRFTreeNodes <- function(model, n=1, root=1)
{
  cassign <- "<-"
  cif <- "if"
  cthen <- ""
  celse <- "else"
  cendif <- ""
  cin <- "%in%"

  # Model this on treeset.randomForest in Rattle.
  
  tree <- getTree(model, n)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  var.names <- names(tr.vars)

  node <- xmlNode("Node")
  result <- ""

  return(xmlNode("True"))
  
  if (tree[root, 'status'] == -1) # Terminal node
  {
    result <- sprintf("Result %s %s", cassign,
                      levels(model$y)[tree[root,'prediction']])
  }
  else
  {
    var.class <- tr.vars[tree[root, 'split var']]
    node.var <- var.names[tree[root,'split var']]
    if(var.class == "character" | var.class == "factor")
    {
      # Convert the binary split point to a 0/1 list for the levels.
      
      var.levels <- levels(eval(model$call$data)[[tree[root,'split var']]])
      bins <- sdecimal2binary(tree[root, 'split point'])
      bins <- c(bins, rep(0, length(var.levels)-length(bins)))
      node.value <- var.levels[bins==1]
      node.value <- sprintf('("%s")', paste(node.value, collapse='", "'))
      condition <- sprintf("%s %s %s%s", node.var, cin,
                           ifelse(format=="R", "c", ""), node.value)
    }
    else if (var.class == "integer" | var.class == "numeric")
    {
      # Assume spliting to the left means "<=", and right ">",
      # which is not what the man page for getTree claims!

      node.value <- tree[root, 'split point']
      condition <- sprintf("%s <= %s", node.var, node.value)

    }
    else
    {
      stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                   var.class))
    }
    

    condition <- sprintf("%s (%s)", cif, condition)
    
    lresult <- treeset.randomForest(model, n, tree[root,'left daughter'],
                                    format=format)
    if (cthen == "")
      lresult <- c(condition, lresult)
    else
      lresult <- c(condition, cthen, lresult)
    rresult <- treeset.randomForest(model, n, tree[root,'right daughter'],
                                    format=format)
    rresult <- c(celse, rresult)
    result <- c(lresult, rresult)
    if (cendif != "") result <- c(result, cendif)
  }
  return(result)
}

########################################################################
treeset.randomForest <- function(model, n=1, root=1, format="R")
{
  # Return a string listing the decision tree form of the chosen tree
  # from the random forest.
  tree <- getTree(model, n)
  if (format == "R")
  {
    cassign <- "<-"
    cif <- "if"
    cthen <- ""
    celse <- "else"
    cendif <- ""
    cin <- "%in%"
  }
  else if (format == "VB")
  {
    cassign <- "="
    cif <- "If"
    cthen <- "Then"
    celse <- "Else"
    cendif <- "End If"
    cin <- "In"
  }

  # Traverse the tree

  tr.vars <- attr(model$terms, "dataClasses")[-1]
  var.names <- names(tr.vars)
  result <- ""
  if (tree[root, 'status'] == -1) # Terminal node
  {
    result <- sprintf("Result %s %s", cassign,
                      levels(model$y)[tree[root,'prediction']])
  }
  else
  {
    var.class <- tr.vars[tree[root, 'split var']]
    node.var <- var.names[tree[root,'split var']]
    if(var.class == "character" | var.class == "factor")
    {
      # Convert the binary split point to a 0/1 list for the levels.
      var.levels <- levels(eval(model$call$data)[[tree[root,'split var']]])
      bins <- sdecimal2binary(tree[root, 'split point'])
      bins <- c(bins, rep(0, length(var.levels)-length(bins)))
      node.value <- var.levels[bins==1]
      node.value <- sprintf('("%s")', paste(node.value, collapse='", "'))
      condition <- sprintf("%s %s %s%s", node.var, cin,
                           ifelse(format=="R", "c", ""), node.value)
    }
    else if (var.class == "integer" | var.class == "numeric")
    {
      # Assume spliting to the left means "<=", and right ">",
      # which is not what the man page for getTree claims!

      node.value <- tree[root, 'split point']
      condition <- sprintf("%s <= %s", node.var, node.value)

    }
    else
    {
      stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                   var.class))
    }

    condition <- sprintf("%s (%s)", cif, condition)
    lresult <- treeset.randomForest(model, n, tree[root,'left daughter'],
                                    format=format)
    if (cthen == "")
      lresult <- c(condition, lresult)
    else
      lresult <- c(condition, cthen, lresult)
    rresult <- treeset.randomForest(model, n, tree[root,'right daughter'],
                                    format=format)
    rresult <- c(celse, rresult)
    result <- c(lresult, rresult)
    if (cendif != "") result <- c(result, cendif)
  }
  return(result)
}
