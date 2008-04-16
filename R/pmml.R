# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-04-16 06:38:34 Graham Williams>
#
# Copyright (c) 2007-2008 Togaware, GPL Version 2
#
# TODO
#
#      Extract the DataDictionary stuff to a separate function to
#      share between pmml.rpart and pmml.kmeans.

########################################################################
# UTILITY FUNCTIONS

# 070929 these are potentially unintialised! Why? Where are they meant
# to gloablly come from? They are defined in random_forest.R but that
# is not part of the pmml package.

cassign <- "<-"
cif <- "if"
cthen <- ""
celse <- "else"
cendif <- ""
cin <- "%in%"

sdecimal2binary <- function(x)
{
  return(rev(sdecimal2binary.smallEndian(x)))
}

sdecimal2binary.smallEndian <- function(x)
{
  if (x==0) return(0)
  if (x<0) stop("Sorry, the input must be positive")
  dec <- x

  n <- floor(log(x)/log(2))
  bin <- c(1)
  dec <- dec - 2 ^ n

  while(n > 0)
  {
    if (dec >= 2 ^ (n-1)) {bin <- c(bin,1); dec <- dec - 2 ^ (n-1)}
    else bin <- c(bin,0)
    n <- n - 1
  }
  return(bin)
}

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

#######################################################################
# Main PMML functions

pmml <- function(model,
                 model.name="Rattle_Model",
                 app.name="Rattle/PMML",
                 description=NULL,
                 copyright=NULL, ...)
  UseMethod("pmml")

# Function pmmlRootNode

pmmlRootNode <- function()
{
  PMML.VERSION <- "3.1"
  return(xmlNode("PMML",
                 attrs=c(version=PMML.VERSION,
                   xmlns="http://www.dmg.org/PMML-3_1",
                   "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")))
}

# Function pmml3.2RootNode

pmml3.2RootNode <- function()
{
  PMML.VERSION <- "3.2"
  return(xmlNode("PMML",
                 attrs=c(version=PMML.VERSION,
                   xmlns="http://www.dmg.org/PMML-3_2",
                   "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance", 
                   "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-3_2",
                     "http://www.dmg.org/v3-2/pmml-3-2.xsd"))))
}

pmmlHeader <- function(description, copyright, app.name)
{
  # Header
  
  VERSION <- "1.1.6"
  #  "1.1.5" # Add pmml.nnet.
  # "1.1.4" # Add pmml.ksvm. Fix extensions. 
  # "1.1.3" # Fixes for new version of randomSurvivalForest.
  # "1.1.2" Expose pmml.lm in NAMESPACE - woops.
  # "1.1.1" Add pmml.lm

  if (is.null(copyright))
    header <- xmlNode("Header", attrs=c(description=description))
  else
    header <- xmlNode("Header",
                      attrs=c(copyright=copyright, description=description))

  # Header -> Extension
						   
  header <- append.XMLNode(header,
                           xmlNode("Extension",
                                   attrs=c(name="timestamp",
                                     value=sprintf("%s", Sys.time()),
                                     extender="Rattle")))
  
  header <- append.XMLNode(header, xmlNode("Extension",
                                           attrs=c(name="description",
                                             value=sprintf("%s",
                                               Sys.info()["user"]),
                                             extender="Rattle")))
  

  # Header -> Application

  header <- append.XMLNode(header, xmlNode("Application",
                                           attrs=c(name=app.name,
                                             version=VERSION)))

  return(header)
}

pmmlDataDictionary <- function(field)
{
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$names
  # field$levels is indexed by fields$names
  number.of.fields <- length(field$name)

  # DataDictionary

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=number.of.fields))
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

    # DataDictionary -> DataField

     data.fields[[i]] <- xmlNode("DataField", attrs=c(name=field$name[i],
                                                optype=optype,
                                                dataType=datype))

    # DataDictionary -> DataField -> Value

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
# LM
#
# Author: rguha@indiana.edu
# Date: 28 May 2007
#
# Modified on 01 Feb 2008 by Zementis, Inc. (info@zementis.com) to add 
# the capability to export binary logistic regression models using glm.

pmml.lm <- function(model,
                    model.name="Regression_model",
                    app.name="Rattle/PMML",
                    description="Regression Model",
                    copyright=NULL, ...)
{
  if (! inherits(model, "lm")) stop("Not a legitimate lm object")

  require(XML, quietly=TRUE)

  # Collect the required information. We list all variables,
  # irrespective of whether they appear in the final model. This
  # seems to be the standard thing to do with PMML. It also adds
  # extra information - i.e., the model did not need these extra
  # variables!

  terms <- attributes(model$terms)
  field <- NULL
  field$name <- names(terms$dataClasses)
  number.of.fields <- length(field$name)
  field$class <- terms$dataClasses
  target <- field$name[1]

  for (i in 1:number.of.fields)
    {
      # We don't need to bother with ylevels since lm doesn't do
      # factor predictions
      if (field$class[[field$name[i]]] == "factor")
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
    }

  # PMML

  pmml <- pmml3.2RootNode()

  # PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007 rguha@indiana.edu"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> RegressionModel

  # Added by Zementis so that code can also export binary logistic
  # regression glm models built with binomial(logit)

  if (as.character(model$call[[3]])[1] == "binomial")
  {
    lm.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          normalizationMethod="softmax",
                          targetFieldName=target)) 
  }
  else # The original code for linear regression models
  {
    lm.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          algorithmName="least squares",
                          targetFieldName=target))
  }

  # PMML -> RegressionModel -> MiningSchema

  lm.model <- append.XMLNode(lm.model, pmmlMiningSchema(field, target))

  # PMML -> RegressionModel -> RegressionTable

  coeff <- coefficients(model)
  coeffnames <- names(coeff)
  
  regTable <- xmlNode("RegressionTable",
                      attrs=c(intercept=as.numeric(coeff[1])))
  
  for (i in 1:length(field$name))
  {
    name <- field$name[[i]]
    if (name == target) next
    klass <- field$class[[name]]
    if (klass == 'numeric')
    {
      predictorNode <- xmlNode("NumericPredictor",
                               attrs=c(name=name,
                                 exponent="1",
                                 coefficient=as.numeric(coeff[which(coeffnames == name)])))
      regTable <- append.XMLNode(regTable, predictorNode)
    }
    else if (klass == 'factor')
    {
      levs <- model$xlevels[[name]]
      for (l in levs[-1])
      {
        tmp <- paste(name, l, sep='')
        predictorNode <- xmlNode("CategoricalPredictor",
                                 attrs=c(name=name,
                                   value=l,
                                   coefficient=as.numeric(coeff[ which(coeffnames == tmp) ])))
        regTable <- append.XMLNode(regTable, predictorNode)
      }
    }
  }
  
  lm.model <- append.XMLNode(lm.model, regTable)
  
  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, lm.model)
  
  return(pmml)
}

#######################################################################
# Neural Networks
#
# Author: Zementis, Inc. (www.zementis.com) E-mail: info@zementis.com
# Date: 6 Feb 2008
# Implements a PMML exporter for nnet objects (Neural Networks)
#

# Function pmml.nnet.DataDictionary

pmml.nnet.DataDictionary <- function(field)
{
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$names
  # field$levels is indexed by fields$names
  number.of.fields <- length(field$name)
  
  # DataDictionary
	
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    # Determine the operation type
    
    optype <- "UNKNOWN"
    datype <- "UNKNOWN"
    values <- NULL
    modified.target <- FALSE
    
    if (field$class[[field$name[i]]] == "numeric")
    {
      optype <- "continuous"
      datype <- "double"
    }
    else if (field$class[[field$name[i]]] == "factor")
    {
      optype <- "categorical"
      datype <- "string"
      
      temp = grep("as.factor", field$name[i], value = TRUE, fixed = TRUE)
      if (i == 1 && length(temp) > 0)
      {
        target <- field$name[i]
        tempName <- strsplit(field$name[i],"")
        endPos <- (length(tempName[[1]]) - 1)
        field$name[i] <- substring(target,11,endPos)
        modified.target <- TRUE
      }       
    }
    
    # DataDictionary -> DataField
    
    data.fields[[i]] <- xmlNode("DataField", attrs=c(name=field$name[i],
                                               optype=optype,
                                               dataType=datype))
    
    if (modified.target == TRUE) 
    {
      field$name[1] <- target
    }
    
    # DataDictionary -> DataField -> Value
    
    if (optype == "categorical")
      for (j in 1:length(field$levels[[field$name[i]]]))
        data.fields[[i]][[j]] <- xmlNode("Value",
                                         attrs=c(value=field$levels[[field$name[i]]][j]))
  }
  data.dictionary$children <- data.fields
  
  return(data.dictionary)
  
}

###################################################################
# Function pmml.nnet.MiningSchema

pmml.nnet.MiningSchema <- function(field, target=NULL)
{
  number.of.fields <- length(field$name)
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
    {	
			temp = grep("as.factor", field$name[i], value = TRUE, fixed = TRUE)
			if (i == 1 && length(temp) > 0)
			{
				targetTmp <- field$name[i]
				tempName <- strsplit(field$name[i],"")
				endPos <- (length(tempName[[1]]) - 1)
				field$name[i] <- substring(targetTmp,11,endPos)
				modified.target <- TRUE                      
			}      
    }
    
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

###################################################################
# Function pmml.nnet
#

pmml.nnet <- function(model,
                      model.name="NeuralNet_model",
                      app.name="Rattle/PMML",
                      description="Neural Network PMML Model",
                      copyright=NULL, 
                      ...)
{
  if (! inherits(model, "nnet")) stop("Not a legitimate nnet object")
  
  require(XML, quietly=TRUE)
  
  ###################################################################
  # Collect the required information. We list all variables,
  # irrespective of whether they appear in the final model. This
  # seems to be the standard thing to do with PMML. It also adds
  # extra information - i.e., the model did not need these extra
  # variables!
  
  number.of.neural.layers <- length(model$n) - 1 
  field <- NULL
  if (model$call[[1]] == "nnet.formula")
  {
    terms <- attributes(model$terms)
    field$name <- names(terms$dataClasses)
    field$class <- terms$dataClasses
    target <- field$name[1]
    number.of.fields <- length(terms$term.labels) + 1  # number of input nodes + target
    number.of.inputs <- length(terms$term.labels)
  }
  else  # nnet.default
  {
    number.of.fields <- model$n[1] + 1  # number of input nodes + target
    number.of.inputs <- model$n[1]
    target <- "y"
    field$name[1] <- target
    field$class[[field$name[1]]] <- "numeric"
    for (i in 1:number.of.inputs)
    {
      tmp <- paste("x", i, sep='')
      field$name[i + 1] <- tmp
      field$class[[field$name[i + 1]]] <- "numeric"
    }
  }
  
  ################################################################################
  # According to the nnet documentation:
  # If the response in formula is a factor, an appropriate classification 
  # network is constructed; this has one output and entropy fit if the number 
  # of levels is two, and a number of outputs equal to the number of classes 
  # and a softmax output stage for more levels.
  # If the response is not a factor, it is passed on unchanged to nnet.default.
  #
  # However, we will actually export a network with two output neurons for binary 
  # classification with a softmax output stage. 
  # 
  
  normalization.method <- "none"
  skipLayers <- FALSE
  linearOutputUnits <- FALSE
  
  if (length(model$call$skip) > 0)
  {
    if(model$call$skip == "T" || model$call$skip == "TRUE")
    {
      skipLayers <- TRUE
    }
  }
  if (model$nunits > model$nsunits)
  {	
    linearOutputUnits <- TRUE
  }
  if (model$softmax)
  {
    normalization.method <- "softmax"
  }
  if (model$censored)
  {
    stop("PMML does not support the censored variant of softmax!")
  }
  
  # Levels
  
  if (field$class[[field$name[1]]] == "factor")
  {
    field$levels[[field$name[1]]] <- model$lev
  }
  factor_count <- 1
  for (i in 1:number.of.inputs)
  {
    if (field$class[[field$name[i + 1]]] == "factor")
    {
      field$levels[[field$name[i + 1]]] <- model$xlevels[[factor_count]]
      factor_count <- factor_count + 1
    }
  }
  
  ##############################################################################
  # PMML
  
  pmml <- pmml3.2RootNode()
  
  # PMML -> Header
  
  if (is.null(copyright))
    copyright <- "Copyright (c) 2008 Zementis, Inc. (www.zementis.com)"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  # PMML -> DataDictionary
  
  pmml <- append.XMLNode(pmml, pmml.nnet.DataDictionary(field))     
  
  #############################################################################
  # PMML -> NeuralNetwork
  
  
  if (model$n[length(model$n)] == 1 && field$class[[field$name[1]]] == "factor")
  {
    temp <- number.of.neural.layers + 1
  }
  else
  {
    temp <- number.of.neural.layers
  }
  
  if (field$class[[field$name[1]]] == "factor")
  {
    nnet.model <- xmlNode("NeuralNetwork",
                          attrs=c(modelName=model.name,
                            functionName="classification",
                            numberOfLayers=temp,
                            activationFunction="logistic")) 		
  }
  else
  {
    nnet.model <- xmlNode("NeuralNetwork",
                          attrs=c(modelName=model.name,
                            functionName="regression",
                            numberOfLayers=temp,
                            activationFunction="logistic")) 
  }
  
  # PMML -> NeuralNetwork -> MiningSchema
  
  temp = grep("as.factor", target, value = TRUE, fixed = TRUE)
  if (length(temp) > 0)
  {
    tempName <- strsplit(target,"")
    endPos <- (length(tempName[[1]]) - 1)
    target <- substring(target,11,endPos)
  }
  
  nnet.model <- append.XMLNode(nnet.model, pmml.nnet.MiningSchema(field, target))
  
  
  ##############################################################################
  # PMML -> NeuralNetwork -> NeuralInputs
  
  neuralInputs <- xmlNode("NeuralInputs",
                          attrs=c(numberOfInputs=as.numeric(model$n[1])))
  input_count <- 1
  factor_count <- 1
  for (i in 1:number.of.inputs)
  {
    if (field$class[[field$name[i+1]]] == "factor")
    {
      number.of.values = length(model$xlevels[[factor_count]])
      usedValues <- model$xlevels[[factor_count]]
      factor_count <- factor_count + 1
      
      for (j in 1:number.of.values)
      {
        if (j > 1) # skips first category during dummyfication
        {
          neuralInputNode <- xmlNode("NeuralInput", 
                                     attrs=c(id=as.numeric(input_count)))
          input_count <- input_count + 1
          
          fieldName <- paste("derivedNI_",terms$term.labels[i],sep="")
          fieldName <- paste(fieldName,usedValues[j],sep="")
          
          derivedFieldNode <- xmlNode("DerivedField",
                                      attrs=c(name=fieldName, 
                                        optype="continuous",
                                        dataType="double"))
          
          normDiscreteNode <- xmlNode("NormDiscrete",
                                      attrs=c(field=terms$term.labels[i],
                                        value=usedValues[j]))
          
          derivedFieldNode <- append.XMLNode(derivedFieldNode, normDiscreteNode)
          
          neuralInputNode <- append.XMLNode(neuralInputNode, derivedFieldNode)
          
          neuralInputs <- append.XMLNode(neuralInputs, neuralInputNode)
        }
      }
    }
    else 
    {
      neuralInputNode <- xmlNode("NeuralInput", 
                                 attrs=c(id=as.numeric(input_count)))
      input_count <- input_count + 1
      
      name <- field$name[i + 1]
      fieldName <- paste("derivedNI_",name,sep="")	
      
      derivedFieldNode <- xmlNode("DerivedField",
                                  attrs=c(name=fieldName, 
                                    optype="continuous",
                                    dataType="double"))
      
      fieldRefNode <- xmlNode("FieldRef",
                              attrs=c(field=terms$term.labels[i]))
      
      derivedFieldNode <- append.XMLNode(derivedFieldNode, fieldRefNode)
      
      neuralInputNode <- append.XMLNode(neuralInputNode, derivedFieldNode)
      
      neuralInputs <- append.XMLNode(neuralInputs, neuralInputNode)
    }				  
    
  }
  
  nnet.model <- append.XMLNode(nnet.model, neuralInputs)
  
  number.of.inputs <- model$n[1]
  
  #############################################################################
  # PMML -> NeuralNetwork -> NeuralLayers
  
  wtsID <- 1
  neuronID <- number.of.inputs
  previous.number.of.neurons <- number.of.inputs
  for (i in 1:number.of.neural.layers)
  {
    number.of.neurons <- model$n[i + 1]
    
    if (i == number.of.neural.layers) # output layer
    {
      if (number.of.neurons == 1 && field$class[[field$name[1]]] == "factor")
      {
        neuralLayerNode <- xmlNode("NeuralLayer",
                                   attrs=c(numberOfNeurons=as.numeric(number.of.neurons)))
      }
      else if (model$softmax)
      {
        neuralLayerNode <- xmlNode("NeuralLayer",
                                   attrs=c(numberOfNeurons=as.numeric(number.of.neurons),
                                     activationFunction="identity", 
                                     normalizationMethod="softmax"))
      }
      else if (linearOutputUnits)
      {
        neuralLayerNode <- xmlNode("NeuralLayer",
                                   attrs=c(numberOfNeurons=as.numeric(number.of.neurons),
                                     activationFunction="identity"))				
      }
      else
      {
        neuralLayerNode <- xmlNode("NeuralLayer",
                                   attrs=c(numberOfNeurons=as.numeric(number.of.neurons)))				
      }
    }
    else # hidden layer
    {
      neuralLayerNode <- xmlNode("NeuralLayer",
                                 attrs=c(numberOfNeurons=as.numeric(number.of.neurons)))	
    }
    
    for (j in 1:number.of.neurons)
    {
      neuronID <- neuronID + 1
      
      neuronNode <- xmlNode("Neuron", 
                            attrs=c(id=as.numeric(neuronID),
                              bias=model$wts[wtsID]))
      wtsID <- wtsID + 1
      
      if (i == number.of.neural.layers && j==1) # output layer
      {
        first.outputNeuronID <- neuronID
			}
      if (i == number.of.neural.layers && skipLayers)
      {
        previous.number.of.neurons <- previous.number.of.neurons + number.of.inputs
      }
      for (k in 1:previous.number.of.neurons)
      {
				number.of.connections <- model$n[i + 1]
				
				connectionNode <- xmlNode("Con",
                                                          attrs=c(from=model$conn[wtsID],
												weight=model$wts[wtsID]))
                                wtsID <- wtsID + 1
				
				neuronNode <- append.XMLNode(neuronNode, connectionNode)
                              }
			neuralLayerNode <- append.XMLNode(neuralLayerNode, neuronNode)
    }
    
    previous.number.of.neurons <- number.of.neurons
    
    nnet.model <- append.XMLNode(nnet.model, neuralLayerNode)
  }
  
  # Special case for NN with 1 output neuron implementing classification
  # Code creates an extra neural layer with a connection set to 1 and bias 
  # to 0 so that the threshold function can be applied. 
  # The previous layer is assumed to have an output from 0 to 1 and so the 
  # threshold is set to 0.5. 
  
  
  if (number.of.neurons == 1 && field$class[[field$name[1]]] == "factor")
	{
          neuralLayerNode <- xmlNode("NeuralLayer",
                                     attrs=c(numberOfNeurons="2",
                                       activationFunction="threshold",threshold = "0.5"))	
          
          neuronID <- neuronID + 1
          first.outputNeuronID <- neuronID
          
          neuronNode <- xmlNode("Neuron", 
                                attrs=c(id=as.numeric(neuronID),
                                  bias="1.0"))
          
          connectionNode <- xmlNode("Con",
                                    attrs=c(from=neuronID - 1,
                                      weight="-1.0"))	  
          
          neuronNode <- append.XMLNode(neuronNode, connectionNode)
          
          neuralLayerNode <- append.XMLNode(neuralLayerNode, neuronNode)
          
          neuronID <- neuronID + 1
          
          neuronNode <- xmlNode("Neuron", 
                                attrs=c(id=as.numeric(neuronID),
                                  bias="0.0"))
          
          connectionNode <- xmlNode("Con",
                                    attrs=c(from=neuronID - 2,
                                      weight="1.0"))				  
          
          neuronNode <- append.XMLNode(neuronNode, connectionNode)
          
          neuralLayerNode <- append.XMLNode(neuralLayerNode, neuronNode)
          
          nnet.model <- append.XMLNode(nnet.model, neuralLayerNode)
          
          number.of.neurons <- number.of.neurons + 1
          
          previous.number.of.neurons <- number.of.neurons
          
	}
  
  ##############################################################################
  # PMML -> NeuralNetwork -> NeuralOutputs
  
  neuralOutputs <- xmlNode("NeuralOutputs",
				             attrs=c(numberOfOutputs=previous.number.of.neurons))
		
	for (i in 1:number.of.neurons)
	{
		neuralOutputNode <- xmlNode("NeuralOutput", 
				                    attrs=c(outputNeuron=first.outputNeuronID))
							
            first.outputNeuronID <- first.outputNeuronID + 1
		
		if (field$class[[field$name[1]]] == "factor")
		{	
                  targetName=target
                  temp = grep("as.factor", field$name[1], value = TRUE, fixed = TRUE)     
			if (length(temp) > 0)
			{
				target <- field$name[1]
				tempName <- strsplit(field$name[1],"")
				endPos <- (length(tempName[[1]]) - 1)
				targetName <- substring(target,11,endPos)
			}       

			fieldName <- paste("derivedNO_",targetName,sep="")
			
			derivedFieldNode <- xmlNode("DerivedField",
					 	                attrs=c(name=fieldName, 
								                optype="continuous",
								                dataType="double"))
			
			normDiscreteNode <- xmlNode("NormDiscrete",
				  						attrs=c(field=targetName,
												value=model$lev[i]))		
				
			derivedFieldNode <- append.XMLNode(derivedFieldNode,normDiscreteNode)				
			
		}
		else # regression
		{
			name <- field$name[1]
			fieldName <- paste("derivedNO_",name,sep="")
			
			derivedFieldNode <- xmlNode("DerivedField",
					                    attrs=c(name=fieldName, 
							                  optype="continuous",
							                  dataType="double"))
		
			fieldRefNode <- xmlNode("FieldRef",
									attrs=c(field=field$name[1]))
						
	    	derivedFieldNode <- append.XMLNode(derivedFieldNode,fieldRefNode)
		
	    }
		
		neuralOutputNode <- append.XMLNode(neuralOutputNode, derivedFieldNode)
		
		neuralOutputs <- append.XMLNode(neuralOutputs, neuralOutputNode)	
		
	}

	nnet.model <- append.XMLNode(nnet.model, neuralOutputs)
	
	# Add to the top level structure.
	
	pmml <- append.XMLNode(pmml, nnet.model)
	
	return(pmml)
}

########################################################################
# SVM
#
# Author: Zementis, Inc. (www.zementis.com) E-mail: info@zementis.com
# Date: 17 Jan 2008
# Implements a PMML exporter for ksvm objects (Support Vector Machines)
#
########################################################################


# Function pmml.ksvm.Header

pmml.ksvm.Header <- function(description, copyright, app.name)
{
  # Header

  KSVMVERSION <- "1.1.5"
  # "1.1.4" # Add pmml.ksvm. Fix extensions. 
  # "1.1.3" Fixes for new version of randomSurvivalForest.
  # "1.1.2" Expose pmml.lm in NAMESPACE - woops.
  # "1.1.1" Add pmml.lm

  if (is.null(copyright))
    header <- xmlNode("Header", attrs=c(description=description))
  else
    header <- xmlNode("Header",
                      attrs=c(copyright=copyright, description=description))

  # Header -> Extension

  header <- append.XMLNode(header,
                           xmlNode("Extension",
                                   attrs=c(name="timestamp",
                                         value=sprintf("%s", Sys.time()),
                                         extender="Rattle")))

  header <- append.XMLNode(header, xmlNode("Extension",
                                           attrs=c(name="description",
                                                 value=sprintf("%s", Sys.info()["user"]),
                                                 extender="Rattle")))

  # Header -> Application

  header <- append.XMLNode(header, xmlNode("Application",
                                           attrs=c(name=app.name,
                                             version=KSVMVERSION)))

  return(header)
}

###################################################################
# Function pmml.ksvm.DataDictionary

pmml.ksvm.DataDictionary <- function(field, dataset)
{
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$names
  # field$levels is indexed by fields$names

  number.of.fields <- length(field$name)
  number.of.data.names = length(names(dataset))
  
  # DataDictionary

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    # Determine the operation type

    optype <- "UNKNOWN"
    datype <- "UNKNOWN"
    values <- NULL
    modified.target <- FALSE

    if (field$class[[field$name[i]]] == "numeric")
    {
      optype <- "continuous"
      datype <- "double"
    }
    else if (field$class[[field$name[i]]] == "factor")
    {
      optype <- "categorical"
      datype <- "string"  
      
      temp = grep("as.factor", field$name[i], value = TRUE, fixed = TRUE)
      if (i == 1 && length(temp) > 0)
      {
         target <- field$name[i]
         tempName <- strsplit(field$name[i],"")
         endPos <- (length(tempName[[1]]) - 1)
         field$name[i] <- substring(target,11,endPos)
         modified.target <- TRUE
      }       
    }

    # DataDictionary -> DataField

    data.fields[[i]] <- xmlNode("DataField", attrs=c(name=field$name[i],
                                                   optype=optype,
                                                   dataType=datype))
                                                   
    if (modified.target == TRUE) 
    {
       field$name[1] <- target
    }
    
    # DataDictionary -> DataField -> Value

    if (i == 1)  # target?
    {
       if (field$function.name == "classification")
       {
       	for (j in 1:length(field$levels[[field$name[i]]]))
            	data.fields[[i]][[j]] <- xmlNode("Value",
                                                 attrs=c(value=field$levels[[field$name[i]]][j]))
       }
       else   # field$function.name == regression
       {
       	   optype <- "continuous"
           datatype <- "double"

           if (length (field$levels[[field$name[i]]]) == 2)
           {
              data.fields[[i]][[1]] <- xmlNode("Interval",
                                               attrs=c(closure="closedClosed",
                                                     leftMargin=field$levels[[field$name[i]]][1],
                                                     rightMargin=field$levels[[field$name[i]]][2]))
           }
        }
     }
  }
  data.dictionary$children <- data.fields

  return(data.dictionary)

}

###################################################################
# Function pmml.ksvm.MiningSchema

pmml.ksvm.MiningSchema <- function(field, target=NULL)
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

##################################################################
# Function pmml.ksvm
#

pmml.ksvm <- function(model,
                      model.name="SVM_model",
                      app.name="Rattle/PMML",
                      description="Support Vector Machine PMML Model",
                      copyright=NULL,
                      data.name,
                      ...)
{
  if (! inherits(model, "ksvm"))
    stop("Not a legitimate ksvm object.")
  
  if (! is.object(data.name))
    stop("Specified dataset not a legitimate object.")

  require(XML, quietly=TRUE)

  # Collect the required information.

  attributes.model <- attributes(model)
  terms.model <- attributes.model$terms
  terms <- attributes(terms.model)
  field <- NULL
  field$name <- names(terms$dataClasses)
  field$class <- terms$dataClasses
  target <- field$name[1]
  number.of.labels <- length(terms$term.labels)
  number.of.fields <- length(field$name)
  number.of.SV <- model@nSV
  
  #####################################################################
  # Regression Vs. Classification
  #
  # Assumes target is the first variable: Numeric = Regression, Factor
  # = Classification

  if (field$class[[1]][1] == "numeric")
  {
    field$function.name <- "regression"
  }
  else
  {
    field$function.name <- "classification"
  }
  
  ###################################################################
  # Determining the number of SVMs:
  # For a classification task with more than two classes, ksvm will
  # generate the correspondent number of SVMs: one machine per class.
  # For a two class problem, only one machine will be generated.

  if (field$function.name == "classification" && model@nclass > 2)
  {
    number.of.SVMs <- (model@nclass * (model@nclass - 1)) / 2 
  }
  else
  {
    number.of.SVMs <- 1
  }
  
  ######################################################################
  # Using and manipulating predicted (or target) variable
  # 1) Assumes that there is a single factor and this is the target
  # 2) Assumes that target is the first variable.
  # First, remove as.factor() from target name
  # Second, capture classes for levels.
  
  temp = grep("as.factor", target, value = TRUE, fixed = TRUE)
  if (field$function.name == "classification" && length(temp) > 0)
  {
    tempName <- strsplit(target,"")
    endPos <- (length(tempName[[1]]) - 1)
    target <- substring(target,11,endPos)
  }
  
  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")  
      field$levels[[field$name[i]]] <- model@lev
  }
  
  # PMML

  pmml <- pmml3.2RootNode()

  # PMML -> Header
  
  if (is.null(copyright))
    copyright <- "Copyright (c) 2008 Zementis, Inc. (www.zementis.com)"
  pmml <- append.XMLNode(pmml, pmml.ksvm.Header(description,
                                                copyright, app.name))

  # PMML -> DataDictionary
  
  pmml <- append.XMLNode(pmml, pmml.ksvm.DataDictionary(field, data.name))
  
  # PMML -> SupportVectorMachineModel
  
  if (field$function.name == "classification" && model@nclass == 2)
  {
    ksvm.model <- xmlNode("SupportVectorMachineModel",
                          attrs=c(modelName=model.name,
                            functionName=field$function.name,
                            algorithmName="supportVectorMachine",
                            svmRepresentation="SupportVectors",
                            alternateBinaryTargetCategory=model@lev[2]))
  }
  else
  {
    ksvm.model <- xmlNode("SupportVectorMachineModel",
                          attrs=c(modelName=model.name,
                            functionName=field$function.name,
                            algorithmName="supportVectorMachine",
                            svmRepresentation="SupportVectors"))
  }
  
  # PMML -> SupportVectorMachineModel -> MiningSchema

  field$name[1] <- target
  ksvm.model <- append.XMLNode(ksvm.model,
                               pmml.ksvm.MiningSchema(field, target))
  
  
  ##########################################################################
  # PMML -> SupportVectorMachineModel -> Targets
  # Targets are necessary to scale SVM output and make data compatible
  # with ksvm's algorithm (post-processing) in case of regression
  
  if (field$function.name == "regression")
  {
    TargetsList <- xmlNode("Targets")
    
    targetNode <- xmlNode("Target", 
                          attrs=c(field=target,
                            rescaleConstant=
                            attributes.model$scaling$y.scale$`scaled:center`,
                            rescaleFactor=
                            attributes.model$scaling$y.scale$`scaled:scale`))
    
    TargetsList <- append.XMLNode(TargetsList, targetNode)
    
    ksvm.model <- append.XMLNode(ksvm.model, TargetsList) 
  }
  

  ###################################################################
  # PMML -> SupportVectorMachineModel -> LocalTransformations 
  #
  # LocalTransformations are necessary to scale x and y and make data
  # compatible with ksvm's algorithm (pre-processing)
  
  number.of.data.names <- length(names(data.name))  
  number.of.scaled <- length(attributes.model$scaling$x.scale$`scaled:center`)
  
  LocalTransformations <- xmlNode("LocalTransformations")
  
  for (i in 1:number.of.labels)
  {
    if (field$class[[field$name[i+1]]] == "factor")
    {
      for (j in 1:number.of.data.names)
        if (terms$term.labels[i] == names(data.name)[j])
        {
          number.of.values = length(levels(data.name[[j]]))
          usedValues <- levels(data.name[[j]])
          break
        }
      for (j in 1:number.of.values)
      {
        fieldName <- paste("derived_",terms$term.labels[i],sep="")
        fieldName <- paste(fieldName,usedValues[[j]],sep="")
        
        derivedFieldNode <- xmlNode("DerivedField",
                                    attrs=c(name=fieldName, 
                                      optype="continuous",
                                      dataType="double"))
        
        normDiscreteNode <- xmlNode("NormDiscrete",
                                    attrs=c(field=terms$term.labels[i],
                                      value=usedValues[j]))
        
        derivedFieldNode <- append.XMLNode(derivedFieldNode, normDiscreteNode)
        
        LocalTransformations <- append.XMLNode(LocalTransformations,
                                               derivedFieldNode)
      }
    }
    else
    {
      for (j in 1:number.of.scaled)
      {
        if (number.of.scaled == 1) break
        if (terms$term.labels[i] ==
            names(attributes.model$scaling$x.scale$'scaled:center'[j]))
          break	
      }
      
      normValue <- (attributes.model$scaling$x.scale$`scaled:center`[[j]] * -1) / 
        attributes.model$scaling$x.scale$`scaled:scale`[[j]]
      
      fieldName <- paste("derived_",terms$term.labels[i],sep="")
     
      derivedFieldNode <- xmlNode("DerivedField",
                                  attrs=c(name=fieldName, 
                                    optype="continuous",
                                    dataType="double"))
      normContinuousNode <- xmlNode("NormContinuous", 
                                    attrs=c(field=terms$term.labels[i]))
      
      linearNormNode <- xmlNode("LinearNorm", 
                                attrs=c(orig="0",
                                  norm=normValue))
      
      normContinuousNode <- append.XMLNode(normContinuousNode, linearNormNode)
      
      linearNormNode <- xmlNode("LinearNorm", 
                                attrs=c(orig=attributes.model$scaling$x.scale$`scaled:center`[[j]],
                                  norm="0"))
      
      normContinuousNode <- append.XMLNode(normContinuousNode, linearNormNode)
      
      derivedFieldNode <- append.XMLNode(derivedFieldNode, normContinuousNode)
      
      LocalTransformations <- append.XMLNode(LocalTransformations,
                                             derivedFieldNode)
    } 
  }
  
  ksvm.model <- append.XMLNode(ksvm.model, LocalTransformations)  
  
  
  ###########################################################################
  # Support PMML Kernel Functions
  # PMML -> SupportVectorMachineMode -> KernelTypeNode

  if (model@kcall[[4]] == "rbfdot")
  {
    KernelTypeNode <- xmlNode("RadialBasisKernelType",
                              attrs=c(gamma=model@kernelf@kpar$sigma,
                                description="Radial basis kernel type"))
  }
  else if (model@kcall[[4]] == "polydot")
  {
    KernelTypeNode <- xmlNode("PolynomialKernelType",
                              attrs=c(gamma=model@kernelf@kpar$scale,
                                coef0=model@kernelf@kpar$offset,
                                degree=model@kernelf@kpar$degree,
                                description="Polynomial kernel type"))
  }
  else if (model@kcall[[4]] == "vanilladot")
  {
    KernelTypeNode <- xmlNode("LinearKernelType",
                              attrs=c(description="Linear kernel type"))
  }
  else if (model@kcall[4] == "tanhdot")
  {
    KernelTypeNode <- xmlNode("SigmoidKernelType",
                              attrs=c(gamma=model@kernelf@kpar$scale,
                                coef0=model@kernelf@kpar$offset,
                                description="Sigmoid kernel type"))
  }
  
  ksvm.model <- append.XMLNode(ksvm.model, KernelTypeNode)
  
  
  # PMML -> SupportVectorMachineMode -> VectorDictionary
  
  VectorDictionary <- xmlNode("VectorDictionary",
                              attrs=c(numberOfVectors=as.numeric(number.of.SV)))

  ########################################################################## 
  # Allocate and initialize variables to make multi class problems possible
  
  number.of.SV.entries <- length(attributes.model$scaling$scaled)
  ix.matrix <- array(0, dim=c(number.of.SVMs, number.of.SV))
  supportVectorEntries <- array(0, dim=c(number.of.SV, number.of.SV.entries))
  all.coef <- array(0, dim=c(number.of.SVMs, number.of.SV))
  usedAlphaID <- vector("list", number.of.SV)
  for (i in 1:number.of.SV) usedAlphaID[[i]] <- 0
  newID <- 1
  
  if (field$function.name == "classification")
  {
    for (ix in 1:number.of.SVMs)
    {
      coeff <- coef(model)
      number.of.coeff <- length(coeff[[ix]])
      
      for (i in 1:number.of.coeff)
      {
        all.coef[ix,i] <- coeff[[ix]][i]
        sameCoeff <- FALSE
        for (j in 1:number.of.SV)
        {
          if (usedAlphaID[[j]] == model@alphaindex[[ix]][i])
          {
            sameCoeff <- TRUE
            ix.matrix[ix,i] <- j
          }
        }
        if (sameCoeff == FALSE)
        {
          ix.matrix[ix,i] <- newID
          usedAlphaID[[newID]] <- model@alphaindex[[ix]][i]
          for (j in 1:number.of.SV.entries)
          {
            supportVectorEntries[newID,j] = model@xmatrix[[ix]][i,j] 
          }
          newID <- (newID + 1)
        }
      }
    }
  }
  else   # Regression
  {
    coeff <- coef(model)
    number.of.coeff <- length(coeff)
    
    for (i in 1:number.of.coeff)
    {
      all.coef[1,i] <- coeff[i]
      ix.matrix[1,i] <- i
      usedAlphaID[[i]] <- model@alphaindex[[i]]
      for (j in 1:number.of.SV.entries)
      {
        supportVectorEntries[i,j] = model@xmatrix[i,j] 
      }
    }
  }
  
  ###########################################################################
  # PMML -> SupportVectorMachineMode -> VectorDictionary -> VectorFieldsList
  #
  # When implementing the code to deal with categorical inputs, we found a
  # potential problem with ksvm. When it produces dummy variables for say
  # 3 categorical variables with 4 categories each, it produces four dummy
  # variables for the first categorical variable, but three variables
  # for the two subsequent categorical variables. The code below mimics 
  # the problem for sake of consistency with ksvm. Otherwise, it would not
  # execute. 
  # We have already contacted
  # Alexandros Karatzoglou and reported the issue. Whenever we learn
  # that ksvm has been fixed, we will alter the code below to reflect the
  # fix.

  VectorFieldsList <- xmlNode("VectorFields",
                              attrs=c(numberOfFields=number.of.SV.entries))
					 
  firstFactor <- TRUE
  for (i in 1:number.of.labels)
  {
    if (field$class[[field$name[i+1]]] == "factor")
    {
      for (j in 1:number.of.data.names)
        if (terms$term.labels[i] == names(data.name)[j])
        {
          number.of.values = length(levels(data.name[[j]]))
          usedValues <- levels(data.name[[j]])
          break
        }		  
      for (j in 1:number.of.values)
      {
        # Reflecting the problem ... by using an if statement
        if (j > 1 || firstFactor)
        {
          fieldName <- paste("derived_",terms$term.labels[i],sep="")
          fieldName <- paste(fieldName,usedValues[[j]],sep="")
          
          vectorFieldsNode <- xmlNode("FieldRef",
                                      attrs=c(field=fieldName))
          VectorFieldsList <- append.XMLNode(VectorFieldsList, vectorFieldsNode)
          firstFactor <- FALSE
        }
      }   
    }
    else
    {
      fieldName <- paste("derived_",terms$term.labels[i],sep="")
      
      vectorFieldsNode <- xmlNode("FieldRef",
                                  attrs=c(field=fieldName))
      VectorFieldsList <- append.XMLNode(VectorFieldsList, vectorFieldsNode)
    }
  }
  
  VectorDictionary <- append.XMLNode(VectorDictionary, VectorFieldsList)
  
  
  # PMML -> SupportVectorMachineModel -> VectorDictionary -> VectorInstances
  
  for (i in 1:number.of.SV)
  {
    vectorInstanceNode <- xmlNode("VectorInstance",
                                  attrs=c(id=as.numeric(i)))
    
    vectorIndices <- NULL
    entries <- NULL
    for (j in 1:number.of.SV.entries)
    {
      vectorIndices <- append(vectorIndices, j)
      vectorIndices <- append(vectorIndices," ")
      
      entries <- append(entries,supportVectorEntries[i,j])
      entries <- append(entries," ")
    }
    
    sparseArrayNode <- xmlNode("REAL-SparseArray",
                               attrs=c(n=as.numeric(number.of.labels)),
                               xmlNode("Indices",vectorIndices),
                               xmlNode("REAL-Entries",entries))
    
    vectorInstanceNode <- append.XMLNode(vectorInstanceNode, sparseArrayNode)
    
    VectorDictionary <- append.XMLNode(VectorDictionary, vectorInstanceNode)
  }
  
  ksvm.model <- append.XMLNode(ksvm.model, VectorDictionary)
  
  ############################################################
  # PMML -> SupportVectorMachineModel -> SupportVectorMachine
  
  if (field$function.name == "classification" && number.of.SVMs > 2)
  {
    target1 <- vector("list", number.of.SVMs)
    target2 <- vector("list", number.of.SVMs)
    ix <- 1
    for (i in 1:length(model@lev))
    {
      for (j in i:length(model@lev))
      {
        if (j > i)
        {
          target1[[ix]] <- i
          target2[[ix]] <- j
          ix <- ix + 1
        }
      }
    }
  }
  
  for (ix in 1:number.of.SVMs)
  {
    # Number of Support Vectors needs to be the same as number of coefficients in PMML.
    
    if (field$function.name == "classification")
    {
      coeff <- coef(model)
      number.of.coeff <- length(coeff[[ix]])
      
      if (number.of.SVMs > 2)
      {
        SupportVectorMachine <- xmlNode("SupportVectorMachine",
                                        attrs=c(targetCategory=model@lev[target1[[ix]]]))
        
        targetExtension <- xmlNode("Extension",
                                   attrs=c(name="alternateTargetCategory",
                                     value=model@lev[target2[[ix]]],
                                     extender="ADAPA"))
        
        SupportVectorMachine <- append.XMLNode(SupportVectorMachine, targetExtension)           
      }
      else  # binary classification
      {
        SupportVectorMachine <- xmlNode("SupportVectorMachine",
                                        attrs=c(targetCategory=model@lev[ix]))
      }
    }
    else   # Regression
    {
      coeff <- coef(model)
      number.of.coeff <- length(coeff)
      
      SupportVectorMachine <- xmlNode("SupportVectorMachine")
    }
    
    # PMML -> SupportVectorMachineModel -> SupportVectorMachine -> SupportVectorsList
    
    SupportVectorsList <- xmlNode("SupportVectors",
                                  attrs=c(numberOfAttributes=as.numeric(number.of.SV.entries),
                                    numberOfSupportVectors=as.numeric(number.of.coeff)))
    
    
    for (i in 1:number.of.coeff)
    {
      supportVectorNode <- xmlNode("SupportVector",
                                   attrs=c(vectorId=as.numeric(ix.matrix[ix,i])))
      SupportVectorsList <- append.XMLNode(SupportVectorsList, supportVectorNode)
    }
    
    SupportVectorMachine <- append.XMLNode(SupportVectorMachine, SupportVectorsList)
    
    # PMML -> SupportVectorMachineModel -> SupportVectorMachine -> CoefficientsList
    
    bias <- (model@b[ix] * -1)
    
    CoefficientsList <- xmlNode("Coefficients",
                                attrs=c(absoluteValue=as.numeric(bias),
                                  numberOfCoefficients=as.numeric(number.of.coeff)))
    
    for (i in 1:number.of.coeff)
    {
      coefficientNode <- xmlNode("Coefficient",
                                 attrs=c(value=as.numeric(all.coef[ix,i])))
      CoefficientsList <- append.XMLNode(CoefficientsList, coefficientNode)
    }
    
    SupportVectorMachine <- append.XMLNode(SupportVectorMachine, CoefficientsList)
    
    ksvm.model <- append.XMLNode(ksvm.model, SupportVectorMachine)
    
  }
  
  
  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, ksvm.model)
  
  return(pmml)
}

########################################################################

pmml.rpart <- function(model,
                       model.name="RPart_Model",
                       app.name="Rattle/PMML",
                       description="RPart decision tree model",
                       copyright=NULL, ...)
{
  if (! inherits(model, "rpart"))
    stop("Not a legitimate rpart object")
  if (model$method != "class")
    stop("Currently only classification is handled.")
  
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)

  # Collect the required information. We list all variables,
  # irrespective of whether they appear in the final model. This
  # seems to be the standard thing to do with PMML. It also adds
  # extra information - i.e., the model did not need these extra
  # variables!

  field <- NULL
  field$name <- as.character(attr(model$terms, "variables"))[-1]
  number.of.fields <- length(field$name)
  field$class <- attr(model$terms, "dataClasses")
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- attr(model, "ylevels")
      else
        field$levels[[field$name[i]]] <- attr(model,"xlevels")[[field$name[i]]]
  }

  # PMML

  pmml <- pmmlRootNode()

  # PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007-2008 Togaware"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> TreeModel

  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="rpart",
                          splitCharacteristic="binarySplit"))


  # PMML -> TreeModel -> MiningSchema

  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  # PMML -> TreeModel -> Node

  depth <- rpart:::tree.depth(as.numeric(row.names(model$frame)))
  count <- model$frame$n
  score <- attr(model, "ylevels")[model$frame$yval]
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

  tree.model <- append.XMLNode(tree.model, node)

  # Add to the top level structure.

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

  pmml <- pmmlRootNode()

  # PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007-2008 Togaware"
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

pmml.rpart.as.rules <- function(model,
                                model.name="RPart_Model",
                                app.name="RPart",
                                description="RPart model as rules",
                                copyright=NULL)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)
  
  if (! inherits(model, "rpart")) stop("Not a legitimate rpart tree")

  # Collect the required information

  field <- NULL
  field$name <- as.character(attr(model$terms, "variables"))[-1]
  number.of.fields <- length(field$name)
  field$class <- attr(model$terms, "dataClasses")
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- attr(model, "ylevels")
      else
        field$levels[[field$name[i]]] <- attr(model,"xlevels")[[field$name[i]]]
  }

  # PMML
  
  pmml <- pmmlRootNode()

  # PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007-2008 Togaware"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  # PMML -> DataDictionary
  
  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> RuleSetModel
  
  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          splitCharacteristic="binary",
                          algorithmName="rpart"))

  # MiningSchema
  
  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  # Add in actual tree nodes.

  rule.set <- xmlNode("RuleSet")
  rule.set <- append.XMLNode(rule.set,
                             xmlNode("RuleSelectionMethod",
                                     attrs=c(criterion="firstHit")))
  
  # Visit each leaf node to generate a rule.

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
  
  # Add to the top level structure.
  
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

  # Collect the required information.

  field <- NULL
  field$name <-  colnames(model$centers)
  number.of.fields <- length(field$name)

  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name

  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  # PMML

  pmml <- pmmlRootNode()

  # PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright (c) 2007-2008 Togaware"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> ClusteringModel

  cl.model <- xmlNode("ClusteringModel",
                      attrs=c(modelName=model.name,
                        functionName="clustering",
                        algorithmName="KMeans",
                        modelClass="centerBased",
                        numberOfClusters=number.of.clusters))

  # PMML -> ClusteringModel -> MiningSchema

  cl.model <- append.XMLNode(cl.model, pmmlMiningSchema(field))

  # PMML -> ClusteringModel -> ComparisonMeasure
  
  cl.model <- append.XMLNode(cl.model, xmlNode("ComparisonMeasure",
                                               attrs=c(kind="distance")))
  
  # PMML -> ClusteringModel -> Cluster -> Array
  
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
  # Based on RANDOM SURVIVAL FOREST 2.0.0, Copyright 2006, Cleveland Clinic
  # Original by Hemant Ishwaran and Udaya B. Kogalur
  # Unified with the pmml package by Graham Williams
  
  if (sum(inherits(model, c("rsf", "forest"), TRUE) == c(1, 2)) != 2)
    stop("Not a legitimate (rsf, forest) object")

  require(XML, quietly=TRUE)
  require(randomSurvivalForest, quietly=TRUE)

  # Collect the required information.

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

  forestSeed = model$seed
  if (is.null(forestSeed))
    stop("RSF forestSeed content is NULL.  Please ensure object is valid.")

  # PMML

  pmml <- pmmlRootNode()

  # PMML -> Header

  if (is.null(copyright))
    copyright <- "Copyright 2006, Cleveland Clinic"
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> MiningBuildTask

  buildNode <- xmlNode("MiningBuildTask")

  # PMML -> MiningBuildTask -> Extension
  
  extensionNode <- xmlNode("Extension")

  # PMML -> MiningBuildTask -> Extension -> X-RSF-Formula

  extensionNode <- append.XMLNode(extensionNode,
                                  xmlNode("X-RSF-Formula",
                                          attrs=c(name=formula)))

  # PMML -> MiningBuildTask -> Extension -> X-RSF-BootstrapSeeds -> Array
    
  extensionNode <- append.XMLNode(extensionNode, 
                                  xmlNode("X-RSF-ForestSeed", 
                                          attrs=c(value=forestSeed)))

  # PMML -> MiningBuildTask -> Extension -> TimesOfInterest

  extensionNode <- append.XMLNode(extensionNode, 
                                  xmlNode("X-RSF-TimesOfInterest", 
                                          xmlNode("Array", 
                                                  attrs=c(type="double",
                                                    n=length(timeInterest)), 
                                                  paste(timeInterest,
                                                        collapse="  \n  "))))
  
  # Add into the PMML.

  pmml <- append.XMLNode(pmml, append.XMLNode(buildNode, extensionNode))
  
  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))
  
  # Create a dummy XML node object to insert into the recursive
  # output object.

  internalNode <- xmlNode("Null")
  
  # Define the variables for the offset and leaf count in the
  # recursive output object.

  offset <- leafCount <- 1
  
  # Create the recursive output object.  This would be unnecessary if
  # it was possible to declare global variables in a package.

  recursiveOutput <- list(internalNode = internalNode,
                          offset = offset, leafCount = leafCount)
  
  # Loop through all trees in the forest and extract the data.

  for (b in 1:numTrees)
  {
    treeModelNode <- xmlNode("TreeModel",
                             attrs=c(modelName=b, functionName="prediction",
                               algorithmName="rsf",
                               splitCharacteristic="binary"))

    # PMML -> TreeModel [b] -> MiningSchema
    
    treeModelNode <- append.XMLNode(treeModelNode, pmmlMiningSchema(field))
    
    # Global dependencies: (field$name, forest)
    
    # Initialize the root node.  This differs from the rest of the
    # internal nodes in the PMML structure.

    treeRoot <- xmlNode("Node", attrs=c(score=0, id=1))
    treeRoot <- append.XMLNode(treeRoot, xmlNode("True"))
    
    rootParmID <- nativeArray$parmID[recursiveOutput$offset] 
    rootSpltPT <- nativeArray$spltPT[recursiveOutput$offset]
    
    recursiveOutput$offset <- recursiveOutput$offset + 1
    recursiveOutput$leafCount <- 1
    
    # Check that the current tree is not a stump (root node only with
    # no branches)

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
    
    # Add the current tree to the PMML data structure.

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
  # pmml <- xmlTreeParse("../TARGET.rpart.xml")
  # pmml <- xmlTreeParse("../TARGET.rpart.xml", useInternalNodes=TRUE)

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
