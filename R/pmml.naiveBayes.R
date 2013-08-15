# PMML: Predictive Model Markup Language
#
# This part of the pmml package handles naive bayes models
#
# Time-stamp: <2013-06-05 19:48:25 Tridivesh Jena>
#
# Copyright (c) 2013 Zementis, Inc.
# Implements a PMML exporter for naiveBayes objects (e1071 package)
#
# Author: Tridivesh Jena, Zementis, Inc. (www.zementis.com)
# E-mail: info@zementis.com
# Date: 8 March 2013
#
# This file is part of the pmml package.
#
# The pmml package is free: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# The pmml package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# To review the GNU General Public License see <http://www.gnu.org/licenses/>
########################################################################
# naiveBayes PMML exporter
# Implemented: 081513 by Tridivesh Jena (info@zementis.com) to add the
# capability to export naive bayes models with categorical and continuous variables.

pmml.naiveBayes <- function(model,
                          model.name="naiveBayes_Model",
                          app.name="Rattle/PMML",
                          description="NaiveBayes Model",
                          copyright=NULL,
                          transforms=NULL,
			  dataset=NULL,
                          predictedField,
                          ...)
{
  if (! inherits(model, "naiveBayes")) stop("Not a legitimate naiveBayes object")

  #require(XML, quietly=TRUE)
  require(e1071, quietly=TRUE)

  # field names and attributes are not given 
  # They must be inferred from the information given in the tables 
  field <- NULL
# target field name not given in R model object. 
# Use dataset to get name...cannot assume in general that the target is the first column
# find it by going thru columns and checking if the levels of the variable is the same
# as the target levels...which are given in the R model output

  target <- predictedField 
  colname <- 1

  if(predictedField=="Y")
    stop("predicted variable name not given.")

  if(!is.null(transforms))
  {
    if(!(predictedField %in% names(transforms$data)))
      stop("predictedField not in list of original fields.")
  }

  if(is.null(model$levels))
  {
    stop("Unable to determine levels of target variable.
          If numeric, please ensure it is read as a factor by using as.factor(variable_name)")
  }

  field$name <- c(target,names(model$tables))
  number.of.fields <- length(field$name)
  numfac <- 0
  field$class[[target]] <- "factor"
  field$levels[[target]] <- model$levels

  for(i in 1:length(names(model$tables)))
  {
    name <- names(model$tables)[i]
    if (length(grep("^as.factor\\(", name)))
    {
     name <- sub("^as.factor\\((.*)\\)", "\\1", name)
    }

    if(!is.null(colnames(model$tables[[i]])))
    {
      field$class[[name]] <- "factor"
      field$levels[[name]] <- colnames(model$tables[[i]])
      numfac = numfac+1
    } else
    {
      # continuous variable
      # Stop after explaining continuous NaiveBayes models not yet supported.
      # stop("continuous variables not yet supported for NaiveBayes models.")
      # Above no longer true 
      field$class[[name]] <- "numeric"
    }
  }

  # if no categorical variables exist, make a placeholder up to satisfy schema requirements
  if(numfac==0)
  {
    if("DiscretePlaceHolder" %in% field$name)
    {
      tname <- "Temp"
    } else 
    {
      tname <- "DiscretePlaceHolder"
    }
    field$name <- c(field$name,tname)
    number.of.fields = number.of.fields + 1
    field$class[[tname]] <- "factor"
    field$levels[[tname]] <- "pseudoValue"
  }

   # 090216 Support transforms if available.
  
  if (.supportTransformExport(transforms))
  {
    field <- .unifyTransforms(field, transforms)
    transforms <- .activateDependTransforms(transforms)
  }

#  numFac <- 1
  field$origName <- NA
  for(i in 1:number.of.fields)
  { 
    #Tridi: remove any 'as.factor' from field names
    if (length(grep("^as.factor\\(", field$name[i])))
    {
     field$origName[i] <- field$name[i]
     field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
#     numFac = numFac + 1
    } else
    {
      field$origName[i] <- NA
    }
  }

  # PMML

  pmml <- .pmmlRootNode("4.1")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))
  
  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, NULL, NULL, transforms))

  # PMML -> NaiveBayesModel

  thresh <- NULL
  if(!is.null(as.list(model$call)$threshold))
  {
    thresh <- as.list(model$call)$threshold
  } else
  {
    thresh <- 0.001
  }
  the.model <- xmlNode("NaiveBayesModel",
                       attrs=c(modelName=model.name,
                         functionName="classification",
                         threshold=thresh))

  # PMML ->  MiningSchema
  
  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, NULL, transforms))

  #########################################
  #  OUTPUT
  the.model <- append.XMLNode(the.model, .pmmlOutput(field,target))

  # PMML -> LocalTransformations 

  if (.supportTransformExport(transforms))
    the.model <- append.XMLNode(the.model, .gen.transforms(transforms))

  if(!is.null(transforms))
  {
  the.model <- append.XMLNode(the.model,pmmlLocalTransformations(field, transforms))
  }

  prob <- NULL
  BayesInputsNode <- xmlNode("BayesInputs")
  for(i in 2:number.of.fields)
  {
    if(field$class[[field$name[i]]] == "factor")
    {
      BayesInputNode <- xmlNode("BayesInput",attrs=c(fieldName=field$name[i]))
      for(j in 1:length(field$levels[[field$name[i]]]))
      {
        PairCountsNode <- xmlNode("PairCounts",attrs=c(value=field$levels[[field$name[i]]][j]))
        TargetValueCountsNode <- xmlNode("TargetValueCounts")
        for(k in 1:length(field$levels[[target]]))
        {
          valuek = field$levels[[target]][k]
          # PMML needs counts....R gives probability.
	  # convert probability to count by multiplying by the total number of cases for that category
          if(is.null(field$origName[i]) || is.na(field$origName[i]))
          {
             prob <- model$tables[[field$name[i]]][k,j]
          } else
          {
	     prob <- model$tables[[field$origName[i]]][k,j]
          }
	  # if prob is null, field name doesnt exist in model tables => it is the temp field
	  if(is.null(prob))
          {
	    prob <- 1.0
	  }
          countk = prob*model$apriori[[k]] 
          TargetValueCountNode <- xmlNode("TargetValueCount",attrs=c(value=valuek,count=countk))
          TargetValueCountsNode <- append.XMLNode(TargetValueCountsNode,TargetValueCountNode)
        }
        PairCountsNode <- append.XMLNode(PairCountsNode,TargetValueCountsNode)
        BayesInputNode <- append.XMLNode(BayesInputNode,PairCountsNode)
      }
      BayesInputsNode <- append.XMLNode(BayesInputsNode,BayesInputNode)
    } else 
    {
      ExtensionNode <- xmlNode("Extension")
      BayesInputNode <- xmlNode("BayesInput",attrs=c(fieldName=field$name[i]))

      TargetValueStatsNode <- xmlNode("TargetValueStats")
      for(j in 1:length(field$levels[[target]]))
      {
        TargetValueStatNode <- xmlNode("TargetValueStat",attrs=c(value=field$levels[[target]][j]))
	avg <- model$tables[[field$name[i]]][j,][1]
        std <- model$tables[[field$name[i]]][j,][2]
        var <- std*std
        GaussianNode <- xmlNode("GaussianDistribution",attrs=c(mean=avg,variance=var))
	TargetValueStatNode <- append.xmlNode(TargetValueStatNode,GaussianNode)
	TargetValueStatsNode <- append.xmlNode(TargetValueStatsNode,TargetValueStatNode) 
      }
      BayesInputNode <- append.xmlNode(BayesInputNode,TargetValueStatsNode)
      ExtensionNode <- append.xmlNode(ExtensionNode,BayesInputNode)
      BayesInputsNode <- append.XMLNode(BayesInputsNode,ExtensionNode)
    }
  }

  the.model <- append.XMLNode(the.model,BayesInputsNode)
  BayesOutputNode <- xmlNode("BayesOutput",attrs=c(fieldName=target))
  TargetValueCountsNode <- xmlNode("TargetValueCounts")
  for(i in 1:length(field$levels[[target]]))
  {
    valuei <- field$levels[[target]][i]
    counti <- model$apriori[[i]]
    TargetValueCountNode <- xmlNode("TargetValueCount",attrs=c(value=valuei,count=counti))
    TargetValueCountsNode <- append.XMLNode(TargetValueCountsNode,TargetValueCountNode)
  }
  BayesOutputNode <- append.XMLNode(BayesOutputNode,TargetValueCountsNode)
  the.model <- append.XMLNode(the.model,BayesOutputNode)
 
  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, the.model)
  
  return(pmml)
}
