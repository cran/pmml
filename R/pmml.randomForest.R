# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-06-21 14:53:25 Graham Williams>
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
                              copyright=NULL, ...)

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

  field$class <- attr(model$terms, "dataClasses")
  names(field$class) <- var.names

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- model$classes
      else
        # How to get the levels for these variables. We don't have the
        # actual data available, and have not found the information in
        # the randomForest object yet. Should be able to get it from
        # the trees, but is there another way?
        field$levels[[field$name[i]]] <- c("NotYetAvailable")
        # model@xlevels[[field$name[i]]]
  }

  # PMML

  pmml <- pmmlRootNode("3.2")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> TreeModel

  # For now, get one tree and print that out. Then put this into a
  # loop over all of the trees in the forest.

  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="randomForest",
                          splitCharacteristic="binarySplit"))

  # PMML -> TreeModel -> MiningSchema

  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  # PMML -> TreeModel -> Node

##   depth <- rpart:::tree.depth(as.numeric(row.names(model$frame)))
##   count <- model$frame$n
##   score <- model@ylevels[model$frame$yval]
##   label <- labels(model, pretty=0)

##   field <- label[1]
##   operator <- ""
##   value <- "" #list("")
##   for (i in 2:length(label))
##   {
##     field <-  c(field, strsplit(label[i], '>|<|=')[[1]][1])
##     op <- substr(label[i], nchar(field[i])+1, nchar(field[i])+2)
##     if (op == ">=")
##     {
##       operator <- c(operator, "greaterOrEqual")
##       value <- c(value, substr(label[i], nchar(field[i])+3, nchar(label[i])))
##     }
##     else if (op == "< ")
##     {
##       operator <- c(operator, "lessThan")
##       value <- c(value, substr(label[i], nchar(field[i])+3, nchar(label[i])))
##     }
##     else if (substr(op, 1, 1) == "=")
##     {
##       operator <- c(operator, "isIn")
##       value <- c(value, substr(label[i], nchar(field[i])+2, nchar(label[i])))
##     }
##   }
  
  node <- genBinaryRFTreeNodes(model)

  tree.model <- append.XMLNode(tree.model, node)

  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, tree.model)

  return(pmml)
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
