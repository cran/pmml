# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2012-12-03 22:26:40 Graham Williams>
#
# Copyright (c) 2009-2010 Togaware Pty Ltd
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
#
# SVM Module
#
# Implements a PMML exporter for ksvm objects (Support Vector Machines)
#
# Author: Zementis, Inc. (www.zementis.com)
# E-mail: info@zementis.com
# Date: 17 Jan 2008

.ksvm.Header <- function(description, copyright, app.name)
{
  # Header

  KSVMVERSION <- "1.2.27"
  # "1.2.27" Took care the case when using the default kernal
  #          Took care the case when model$scaling is NULL
  # "1.1.8" Fixed dataField issues and REAL-SPARSE ARRAY n issue
  # "1.1.4" # Add pmml.ksvm. Fix extensions.
  # "1.1.3" Fixes for new version of randomSurvivalForest.
  # "1.1.2" Expose pmml.lm in NAMESPACE - woops.
  # "1.1.1" Add pmml.lm

  if (is.null(copyright))
    copyright <- .generateCopyright()
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
# Function .ksvm.DataDictionary

.ksvm.DataDictionary <- function(field, dataset, weights=NULL)
{
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$names
  # field$levels is indexed by fields$names

  number.of.fields <- length(field$name)
  number.of.data.names = length(names(dataset))
  
  # DataDictionary
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=number.of.fields))
  if (! is.null(weights))
    data.dictionary <-append.XMLNode(data.dictionary, xmlNode("Extension",
                                                              attrs=c(name="Weights",
                                                                value=weights,
                                                                extender="Rattle")))
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

      # Deal with the target variable by removing the "as.factor(...)"
      # from around the variable name. 110101 It appears that the
      # strsplit is simply splitting the string into individual
      # chars. Why is that so and is it correct?
      
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
    
    if(i==1) # For the target field.
    {
      predictedFieldName = field$name[1];
      if(modified.target == TRUE)
      {
        field$name[1] <- target
      }
      data.dictionary <- .getPredictedDataField(data.dictionary, field,
                                               predictedFieldName, optype, datype)
    }
    else  # For non-target fields.
    {
      # 110101 Graham: We should really only pass through the field
      # name and the levels, rahter than relying on indexing the
      # dataset within the function with the field name etc.

      data.dictionary <- .getInputDataField(data.dictionary,field,dataset,i,optype,datype)
    }
  }

  return(data.dictionary)
}

###################################################################
# Function .ksvm.MiningSchema

.ksvm.MiningSchema <- function(field, target=NULL)
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
                      transforms=NULL,
                      dataset=NULL,
                      weights=NULL,
                      ...)
{
  if (! inherits(model, "ksvm"))
    stop("Not a legitimate ksvm object.")
  
  if (! is.object(dataset))
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


  if (field$class[[field$name[1]]] == "factor")
      field$levels[[field$name[1]]] <- model@lev

  for (i in 2:number.of.fields)
  {
    lev <- NULL
    if (field$class[[field$name[i]]] == "factor")
    {
     dlevel <- unique(dataset[field$name[i]])
     for(j in 1:nrow(dlevel))
     {
      lev <- c(lev,as.character(dlevel[j,1]))
     }
     field$levels[[field$name[i]]] <- lev
    }
  }


##########################################
  
#  for (i in 1:number.of.fields)
#  {
#    if (field$class[[field$name[i]]] == "factor")
#      field$levels[[field$name[i]]] <- model@lev
#  }
 
####################################  

  # PMML

  pmml <- .pmmlRootNode("4.1")

  # PMML -> Header
  
  pmml <- append.XMLNode(pmml, .ksvm.Header(description, copyright, app.name))

  # PMML -> DataDictionary
 
  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, dataset, weights=weights, transformed=transforms))
  
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
                               .pmmlMiningSchema(field, target, NULL, transformed=transforms))

  # Output 
  ksvm.model <- append.XMLNode(ksvm.model, .pmmlOutput(field, target))  
  
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


  if (.supportTransformExport(transforms))
  {
    field <- .unifyTransforms(field, transforms)
    transforms <- .activateDependTransforms(transforms)
  }
  
  number.of.data.names <- length(names(dataset))

  number.of.scaled <- 0  
  if(length(model@scaling) > 0)
  {
    number.of.scaled <- length(attributes.model$scaling$x.scale$`scaled:center`)
  }
  
  LocalTransformations <- xmlNode("LocalTransformations")
 
  # test of Zementis xform functions
  if(!is.null(transforms))
  {
    LocalTransformations <- pmmlLocalTransformations(field, transforms, LocalTransformations)
  }

  for (i in 1:number.of.labels)
  {
    if (field$class[[field$name[i+1]]] == "factor")
    {
      for (j in 1:number.of.data.names)
      {
        if (terms$term.labels[i] == names(dataset)[j])
        {
          number.of.values = length(levels(dataset[[j]]))
          usedValues <- levels(dataset[[j]])
          break
        }
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
      for (j in seq_len(number.of.scaled))
      {
        if (number.of.scaled == 1) break
        if (terms$term.labels[i] ==
            names(attributes.model$scaling$x.scale$'scaled:center'[j]))
          break
      }
      
      centerValue <- attributes.model$scaling$x.scale$`scaled:center`[[j]]
      scaleValue <- attributes.model$scaling$x.scale$`scaled:scale`[[j]]
      normValue <- (centerValue * -1) / scaleValue
      
      fieldName <- paste("derived_",terms$term.labels[i],sep="")  
      derivedFieldNode <- xmlNode("DerivedField",
                                  attrs=c(name=fieldName,
                                    optype="continuous",
                                    dataType="double"))
      normContinuousNode <- xmlNode("NormContinuous",
                                    attrs=c(field=terms$term.labels[i]))
      linearNormNode1 <- xmlNode("LinearNorm", attrs=c(orig="0", norm=normValue))
      linearNormNode2 <- xmlNode("LinearNorm", attrs=c(orig=centerValue,norm="0")) 
      
      if(centerValue > 0)
      {
        normContinuousNode <- append.XMLNode(normContinuousNode, linearNormNode1)
        normContinuousNode <- append.XMLNode(normContinuousNode, linearNormNode2)
      }
      else
      {
        normContinuousNode <- append.XMLNode(normContinuousNode, linearNormNode2)
        normContinuousNode <- append.XMLNode(normContinuousNode, linearNormNode1)
      }
      
      derivedFieldNode <- append.XMLNode(derivedFieldNode, normContinuousNode)
      
      LocalTransformations <- append.XMLNode(LocalTransformations,
                                             derivedFieldNode)
    }
  }
  
  ksvm.model <- append.XMLNode(ksvm.model, LocalTransformations)
  
  
  ###########################################################################
  # Support PMML Kernel Functions
  # PMML -> SupportVectorMachineMode -> KernelTypeNode
  
  if (is.null(model@kcall[["kernel"]]))
  {
    KernelTypeNode <- xmlNode("RadialBasisKernelType",
                              attrs=c(gamma=model@kernelf@kpar$sigma,
                                description="Radial basis kernel type"))
  }
  else
  {
    if (model@kcall[["kernel"]] == "rbfdot")
    {
      KernelTypeNode <- xmlNode("RadialBasisKernelType",
                                attrs=c(gamma=model@kernelf@kpar$sigma,
                                  description="Radial basis kernel type"))
    }
    else if (model@kcall[["kernel"]] == "polydot")
    {
      KernelTypeNode <- xmlNode("PolynomialKernelType",
                                attrs=c(gamma=model@kernelf@kpar$scale,
                                  coef0=model@kernelf@kpar$offset,
                                  degree=model@kernelf@kpar$degree,
                                  description="Polynomial kernel type"))
    }
    else if (model@kcall[["kernel"]] == "vanilladot")
    {
      KernelTypeNode <- xmlNode("LinearKernelType",
                                attrs=c(description="Linear kernel type"))
    }
    else if (model@kcall[["kernel"]] == "tanhdot")
    {
      KernelTypeNode <- xmlNode("SigmoidKernelType",
                                attrs=c(gamma=model@kernelf@kpar$scale,
                                  coef0=model@kernelf@kpar$offset,
                                  description="Sigmoid kernel type"))
    }
  }
  
  ksvm.model <- append.XMLNode(ksvm.model, KernelTypeNode)
  
  # PMML -> SupportVectorMachineMode -> VectorDictionary
  
  VectorDictionary <- xmlNode("VectorDictionary",
                              attrs=c(numberOfVectors=as.numeric(number.of.SV)))

  ##########################################################################
  # Allocate and initialize variables to make multi class problems possible
  
  if (field$function.name == "classification")
  {
  number.of.SV.entries <- length(model@xmatrix[[1]][1,])
  } 
  else
  {
    number.of.SV.entries <- length(model@xmatrix[1,])
  }
  # number.of.SV.entries <- length(attributes.model$scaling$scaled) #110101
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
        if (terms$term.labels[i] == names(dataset)[j])
        {
          number.of.values = length(levels(dataset[[j]]))
          usedValues <- levels(dataset[[j]])
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
                               attrs=c(n = number.of.SV.entries ),
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

###############################################################################################
# function: .getPredictedDataField
#
# goal: create the data field for the predicted field
#
# refactored in June, 2008 
##############################################################################################
.getPredictedDataField <- function(dataDictionary,field,predictedFieldName,optype,datatype)
{
    target <- field$name[1]
    predictedDataField <- xmlNode("DataField", attrs=c(name = predictedFieldName, optype=optype, dataType=datatype))

    if (field$function.name == "classification"){
       for (j in 1:length(field$levels[[target]]))
       {
            predictedDataField <- append.XMLNode( predictedDataField,xmlNode("Value", attrs=c(value=field$levels[[target]][j])) )
       }
    } else   # field$function.name == regression
    {
          optype <- "continuous"
          datatype <- "double"

          if (length (field$levels[[target]]) == 2)
          {
             predictedDataField <- append.XMLNode( predictedDataField,
              xmlNode("Interval", attrs=c(closure="closedClosed", leftMargin=field$levels[[target]][1], rightMargin=field$levels[[target]][2])))
          }
    }

    dataDictionary <- append.XMLNode(dataDictionary,predictedDataField)
    return(dataDictionary)
}

########################################################################
# function: .getInputDataField
# goal: create the data field for the input data field
# author: Zementis
# refactored: June, 2008
#
# modified: 110101 Graham: Fix bug where i is field$name index, not
#           dataset column index. Result was that factors were getting
#           the next variable's levels, not its own levels.
#
########################################################################

# 110101 Graham: We should really only pass through the field name and
# the levels, rather than relying on indexing the dataset within the
# function with the field name etc.

.getInputDataField <- function(dataDictionary,field,dataset,i,optype,datatype)
{
  field.name <- field$name[i]
  
  inputDataField <- xmlNode("DataField",
                            attrs=c(name=field.name,
                              optype=optype,
                              dataType=datatype))

  if(optype == "categorical") # For categorical data field.
  {
    field.levels <- levels(dataset[[field.name]])
    for (j in 1:length(field.levels))
      inputDataField <- append.XMLNode(inputDataField,
                                       xmlNode("Value",
                                               attrs=c(value=field.levels[j])))
  }
  else  # For a continuous data field.
  {
    if (length (field$levels[[field.name]]) == 2)
    {
      inputDataField <-
        append.XMLNode(inputDataField,
                       xmlNode("Interval",
                               attrs=c(closure="closedClosed",
                                 leftMargin=field$levels[[field.name]][1],
                                 rightMargin=field$levels[[field.name]][2])))
    }
  }
  dataDictionary <- append.XMLNode(dataDictionary,inputDataField)
  return(dataDictionary)
}
