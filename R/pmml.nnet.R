# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-06-21 14:50:01 Graham Williams>
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
  
  pmml <- pmmlRootNode("3.2")
  
  # PMML -> Header
  
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
  
  #########################################
  #  OUTPUT
  nnet.model <- append.XMLNode(nnet.model, pmmlOutput(field,target))
  
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

