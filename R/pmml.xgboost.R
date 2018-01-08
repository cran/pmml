# PMML: Predictive Model Markup Language
#
# Copyright (c) 2016-2017, Zementis, Inc. 
#
# This file is part of the PMML package for R.
#
# The PMML package is free software: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of 
#
# The PMML package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
######################################################################################
#
# Author: Tridivesh Jena

#' Generate PMML for a xgb.Booster object from the xgboost package
#' 
#' @param model an object created by the 'xgboost' function
#' @param model.name optional; the model name.
#' @param app.name optional; name where the model was created. 
#' @param description optional; description of the model.
#' @param copyright optional; a copyright statement.
#' @param transforms optional; any pre-processing information from the pmmlTransformations package.
#' @param unknownValue optional; a missing value replacement.
#' @param inputFeatureNames input variable names used in training the model
#' @param outputLabelName name of the predicted field
#' @param outputCategories possible values of the predicted field, for classification models.
#' @param xgbDumpFile name of file saved using 'xgb.dump' function.
#' @param parentInvalidValueTreatment invalid value treatment at the top MiningField level.
#' @param childInvalidValueTreatment invalid value treatment at the model segment MiningField level.
#' @param ... further arguments passed to other methods.
#' 
#' @return PMML representation of the \code{xgb.Booster} object.
#' 
#' @details The \code{xgboost} function takes as its input either an \code{xgb.DMatrix} object or
#' a numeric matrix. The input field information is not stored in the R model object, 
#' hence the field information must be passed on as inputs. This enables the PMML 
#' to specify field names in its model representation. The R model object does not store 
#' information about the fitted tree structure either. However, this information can
#' be extracted from the \code{xgb.model.dt.tree} function and the file saved using the
#' \code{xgb.dump} function. The xgboost library is therefore needed in the environmant and this 
#' saved file is needed as an input as well.
#' 
#' The following objectives are currently supported: \code{multi:softprob},
#' \code{multi:softmax}, \code{binary:logistic}.
#' 
#' The pmml exporter will throw an error if the xgboost model model only has one tree.
#' 
#'
#' 
#' @examples 
#' # Standard example using the xgboost package example model
#' # make the xgboost model using xgb.DMatrix object as inputs
#' \dontrun{
#' library(xgboost)
#' data(agaricus.train, package='xgboost')
#' data(agaricus.test, package='xgboost')
#' train <- agaricus.train
#' test <- agaricus.test
#' model1 <- xgboost(data = train$data, label = train$label, max_depth = 2,eta = 1, nthread = 2, 
#'                   nrounds = 2, objective = "binary:logistic")
#' }
#' 
#' # the input feature names for the xgb.DMatrix object can be extracted as colnames(train$data)
#' # the output field name and categories must be inferred. Looking at train$label informs us 
#' # that the output categories are either 0 or 1. The name cannot be inferred and so will be 
#' # given a name "prediction1" save the tree information required in an external file
#' \dontrun{
#' xgb.dump(model1, "model1.dumped.trees")
#' }
#' # Now all requiredinput parameters are known:
#' \dontrun{
#' pmml(model1,inputFeatureNames=colnames(train$data),outputLabelName="prediction1",
#'  outputCategories=c("0","1"),xgbDumpFile="model1.dumped.trees")
#' }
#' 
#' # use iris dataset to make a multinomial model
#' # input data as a matrix
#' \dontrun{
#' model2 <- xgboost(data = as.matrix(iris[,1:4]), label = as.numeric(iris[,5])-1, 
#'                max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "multi:softprob",
#'                num_class=3)
#' }
#'                
#' # The field names are easily extracted from the columnnames and the categories are converted to
#' # numeric format by xgboost.
#' # save the tree information file
#' \dontrun{
#' xgb.dump(model2, "model2.dumped.trees")
#' 
#' pmml(model2,inputFeatureNames=colnames(as.matrix(iris[,1:4])),outputLabelName="Species",
#' outputCategories=c(1,2,3),xgbDumpFile="model2.dumped.trees")
#' }
#' 
#' @seealso \code{\link[pmml]{pmml}},
#' \href{http://dmg.org/pmml/v4-3/GeneralStructure.html}{PMML Schema}
#' 
#' @export
pmml.xgb.Booster <- function(model,
                              model.name="xboost_Model",
                              app.name="R",
                              description="Extreme Gradient Boosting Model",
                              copyright=NULL,
                              transforms=NULL,
	                      inputFeatureNames=NULL,
	                      outputLabelName=NULL,
	                      outputCategories=NULL,
	                      xgbDumpFile=NULL,
	                      unknownValue=NULL,
	                      parentInvalidValueTreatment="returnInvalid",
	                      childInvalidValueTreatment="asIs",
                              ...)

{
   if (! inherits(model, "xgb.Booster"))
    stop("Not a legitimate xgboost object")

   #requireNamespace("xgb.Booster",quietly=TRUE)

   # All inputs are either numeric or have to be one-hot-encoded before
   # reading in to the xgboost function. The input data also has to be
   # in Matrix or xgb.DMatrix format, so cannot use a data frame with
   # discretization. Therefore just use the built in tree parser to get
   # feture inputs and assume that they are the inputs. This might imply
   # that the model will miss some inputs which were not used or some 
   # factor levels which were not used. These feature names are in the
   # shape of 'VariableCategory' and so we need the variable name to
   # split and get category names. We therefore require the input field 
   # names as an input parameter. This can be in the example format of
   # colnames(iris) or colnames(dtrain$data) where dtrain is a xgb.DMatrix.

   # TO DO:
   #	1. What if input names are not given? Make inputs f1,f2 etc? Get from dump file?
   #	2. Implement for regression, tweedie, custom objective functions etc
   #	3. Test for xgb.train as well as xgboost
   #	4. Test for xgb.DMatrix and Matrix inputs

   if (!(model$params$objective %in% c("multi:softprob", "multi:softmax", "binary:logistic"))) {
     stop("Only the following objectives are supported: multi:softprob, multi:softmax, binary:logistic.")
   }
  
   if(is.null(inputFeatureNames)) {
     stop("Input feature names required at present version. Try using colnames function on Matrix, matrix or xgb.DMatrix$data")
   }
   if(is.null(xgbDumpFile)) {
     stop("Must Provide file name of text file where xgb model was saved.")
   }
   functionName <- "classification"
   if(is.null(outputCategories)) {
     warning("No output categories given; regression model assumed.")
     functionName <- "regression"
   }

   invalidValueTreatment_values <- c("returnInvalid", "asIs", "asMissing")
   if(!(parentInvalidValueTreatment %in% invalidValueTreatment_values)) {
     stop(paste0(c("\"", parentInvalidValueTreatment, "\" is not a valid enumeration value for parentInvalidValueTreatment. Use one of the following: ", 
                   paste(invalidValueTreatment_values, sep=",", collapse=", "))), ".")
   }
   
   if(!(childInvalidValueTreatment %in% invalidValueTreatment_values)) {
     stop(paste0(c("\"", childInvalidValueTreatment, "\" is not a valid enumeration value for childInvalidValueTreatment. Use one of the following: ", 
                   paste(invalidValueTreatment_values, sep=",", collapse=", "))), ".")
   }

   # get tree split information
   dtable <- xgboost::xgb.model.dt.tree(inputFeatureNames,model)
   # get all features used
   used_features <- unique(unlist(dtable[,4],use.names = F))
   #get rid of leaves
   used_features <- used_features[!str_detect(used_features,"[lL]eaf")]
   # deal with fields with = and others separately
   usedFieldswithEq <- used_features[str_detect(used_features,"=")]
   usedFieldsOthers <- used_features[!str_detect(used_features,"=")]
   # get rid of leaves from all input fields
   inputFeatureNames <- inputFeatureNames[!str_detect(inputFeatureNames,"[lL]eaf")]
   # get rid of inputs with =, these will be taken care of via splitFieldswithEq
   inputFeatureNames <- inputFeatureNames[!str_detect(inputFeatureNames,"=")]
   # if given, remove output field name
   if(!is.null(outputLabelName)) {
     inputFeatureNames <- inputFeatureNames[!str_detect(inputFeatureNames,paste0("^",outputLabelName,"$"))]
     usedFieldsOthers <- usedFieldsOthers[!str_detect(usedFieldsOthers,paste0("^",outputLabelName,"$"))]
   }
   
   # make vector of field names used by model. Go through the used features
   fields1 <- unique(sapply(str_split(usedFieldswithEq,"="),function(x){x[1]}))
   f <- vector()
   fields2 <- unique(unlist(lapply(inputFeatureNames,function(x){if(length(str_subset(usedFieldsOthers,x))>0){f<-c(f,x)}})))

   target <- outputLabelName
   field <- list()
   if(length(fields1) > 0) {
     field$name <- c(fields1,fields2,outputLabelName)
   } else {
     field$name <- c(fields2,outputLabelName)
   }

   maxLevels <- 0
   if(length(fields1) > 0){
     for(i in 1:length(fields1)) {
       leqs <- unlist(unique(sapply( str_split(usedFieldswithEq,"="), function(x){ if(x[1]==fields1[i]){x[2]}} )))
       if(length(leqs) > maxLevels) {
         maxLevels <- length(leqs)
       }
       field$levels[[fields1[i]]] <- list(leqs)
       field$levels[[fields1[i]]] <- field$levels[[fields1[i]]][[1]]
       field$class[[fields1[i]]] <- "factor"
     }
   }

   # Assume all fields without '=' are numeric. Original code wad to assume categorical fields could still be
   # denoted by fieldname(separator)category where separatoe could be '-' or '_' but now has been decided that
   # this assumption is too arbitary
   # if(length(fields2) > 0) {
   #   for(i in 1:length(fields2)) {
   #     s <- fields2[i]
   #     leqs <- unique(str_replace(usedFieldsOthers[str_detect(usedFieldsOthers,str_interp("^${s}[^-_]"))],str_interp("^${s}"),""))
   #     if(length(leqs) > maxLevels) {
   #       maxLevels <- length(leqs)
   #     }
   #     if(length(leqs) > 0) {
   #       field$levels[[fields2[i]]] <- list(leqs)
   #       field$levels[[fields2[i]]] <- field$levels[[fields2[i]]][[1]]
   #       field$class[[fields2[i]]] <- "factor"
   #     } else {
   #       #field$levels[[fields2[i]]] <- NULL
   #       field$class[[fields2[i]]] <- "numeric"
   #     }
   #   }
   # }
   if(length(fields2) > 0) {
     for(i in 1:length(fields2)) {
         field$class[[fields2[i]]] <- "numeric"
     }
   }

   if(functionName == "regression") {
     field$class[outputLabelName] <- "numeric"
   } else {
     field$class[outputLabelName] <- "factor"
     field$levels[[outputLabelName]] <- outputCategories
   }
   
   if(maxLevels > 0){
     dt <- data.frame(tmp=rep(NA,maxLevels))
     for(fname in field$name) {
       if(fname != outputLabelName) {
         l <- rep(field$levels[[fname]][1],maxLevels)
         l[1:length(field$levels[[fname]])] <- field$levels[[fname]]
         dt[,fname] <- l
       }
       dt[,"tmp"] <- NULL
     }
     xbox <- pmmlTransformations::WrapData(dt)
     for(i in 1:ncol(dt)) {
       xbox <- pmmlTransformations::NormDiscreteXform(xbox,xformInfo=colnames(dt)[i],levelSeparator="=",ignoreOperatorSigns=T)
     }
   }

   # read in xgb dump file
   dumpFile <- file(xgbDumpFile)
   treelines <- readLines(dumpFile)
   close(dumpFile)

   dtable[,"prediction"] <- as.double(NA)
   for(lin in treelines) {
     if(str_detect(lin,"^booster")) {
       boostNum <- str_extract(lin,"[0-9]+")
     }
     if(str_detect(lin,"leaf=")) {
       leafInfo <- str_split(lin,":leaf=")
       nodeId <- paste0(boostNum,"-",str_replace(leafInfo[[1]][1],"\t+",""))
       pred <- leafInfo[[1]][2]
       pick <- which(dtable[,"ID"]==nodeId)
       suppressWarnings(dtable[pick,"prediction"] <- as.numeric(pred))
       # Remove suppressWarnings?
     }
   }

   # PMML
   pmml <- .pmmlRootNode()

   # PMML -> Header
   pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))

   # PMML -> DataDictionary
   pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field,transformed=transforms))
   if(maxLevels > 0){
     pmml <- append.xmlNode(pmml,pmml(,transforms=xbox,transformationDictionary=T))
   }
   mmodel <- xmlNode("MiningModel",attrs=c(modelName=model.name,algorithmName="xgboost",functionName=functionName))
   mmodel <- append.XMLNode(mmodel,.pmmlMiningSchema(field,target,transforms,unknownValue,
                                                     invalidValueTreatment=parentInvalidValueTreatment))

   # Tridi: Add output fields
   # TODO 
   # THIS IS ONLY TRUE FOR CLASSIFICATION MODELS
   # TEST AND CHANGE IF/WHEN REGRESSION MODELS ARE IMPLEMENTED
   mmodel <- append.XMLNode(mmodel, .pmmlOutput(field, target, "categorical"))

   #Tridi: If interaction terms do exist, define a product in LocalTransformations and use
   # it as a model variable. This step is rare.
   interact <- FALSE
   number.of.fields <- length(field$name)
   for(fld in 1:number.of.fields)
   {
     if(length(grep(":",field$name[fld])) == 1)
     {
       interact <- TRUE
       drvnode <- xmlNode("DerivedField",attrs=c(name=field$name[fld],optype="continuous",
                                                               dataType="double"))
       applyNode <- xmlNode("Apply",attrs=c("function"="*"))
       for(fac in 1:length(strsplit(field$name[fld],":")[[1]]))
       {
         fldNode <- xmlNode("FieldRef",attrs=c(field=strsplit(field$name[fld],":")[[1]][fac]))
         if(length(grep("as\\.factor\\(",fldNode)) == 1)
           fldNode <- gsub("as.factor\\((\\w*)\\)","\\1", fldNode, perl=TRUE)
         applyNode <- append.XMLNode(applyNode, fldNode)
       }
       drvnode <- append.XMLNode(drvnode, applyNode)
      }
      if(interact)
        ltNode <- append.XMLNode(ltNode, drvnode)
   }
   if(interact && is.null(transforms))
   {
     ltNode <- xmlNode("LocalTransformations")
     mmodel <- append.XMLNode(mmodel, ltNode)
   }

   # test of Zementis xform functions
   if(interact && !is.null(transforms))
   {
      ltNode <- .pmmlLocalTransformations(field, transforms, ltNode)
      mmodel <- append.XMLNode(mmodel, ltNode)
   }
   if(!interact && !is.null(transforms))
   {
      mmodel <- append.XMLNode(mmodel,.pmmlLocalTransformations(field, transforms, ltNode))
   }

   segmentation <- xmlNode("Segmentation",attrs=c(multipleModelMethod="modelChain"))

   numTrees <- as.numeric(dtable[nrow(dtable),1]+1)
   objective <- model$params$objective
   # if((objective=="binary:logistic") | (objective=="multi:softprob")) {
   #   type <- "classification"
   # } else {
   #   type <- "regression"
   # }

   if(objective %in% c("binary:logistic","multi:softprob","multi:softmax")) {
     type <- "classification"
   } else {
     type <- "regression"
   }
   
   
   if(functionName == "classification" && objective != "binary:logistic") {
     if((numTrees/length(outputCategories)) != floor(numTrees/length(outputCategories))) {
       stop("Number of trees not a multiple of number of categories provided.")
     }
   }
   
   # segments <- lapply(1:numTrees,function(x){.makeXgSegment(x,dtable,model.name,field,target,unknownValue,asIs=TRUE)})
   segments <- lapply(1:numTrees,function(x){.makeXgSegment(x,dtable,model.name,field,target,unknownValue,childInvalidValueTreatment)})

   segmentation <- append.XMLNode(segmentation, segments)
   if(type == "classification") {
     lastSegment <- append.xmlNode(xmlNode("Segment",attrs=c(id=numTrees)),xmlNode("True"))
     lastModel <- .makeClassificationRegressionModel(field,target,outputCategories,numTrees,type,objective,childInvalidValueTreatment)
     lastSegment <- append.xmlNode(lastSegment,lastModel)
     segmentation <- append.xmlNode(segmentation,lastSegment)
   }

   mmodel <- append.XMLNode(mmodel,segmentation)
   pmml <- append.XMLNode(pmml, mmodel)

   return(pmml)
}

   .makeXgSegment <- function(treeNum,dtable,model.name,field,target,unknownValue=NULL,childInvalidValueTreatment)
   {
     treeNum <- treeNum - 1
     print(paste("Now converting tree ",treeNum," to PMML"))
     treeBeginRow <- min(which(dtable[,"Tree"]==treeNum))
     treeEndRow <- max(which(dtable[,"Tree"]==treeNum))
     # objective=="binary:logistic"|"multi:softprob"
     tinf <- data.frame(Node=dtable[treeBeginRow:treeEndRow,2],Feature=dtable[treeBeginRow:treeEndRow,4],
                          Split=dtable[treeBeginRow:treeEndRow,5],Yes=dtable[treeBeginRow:treeEndRow,6],
                          No=dtable[treeBeginRow:treeEndRow,7],Missing=dtable[treeBeginRow:treeEndRow,8],
                          prediction=dtable[treeBeginRow:treeEndRow,11])
     tinf[,"Yes"] <- suppressWarnings(as.numeric(unlist(lapply(1:(treeEndRow- treeBeginRow + 1),
                                                      function(x){str_replace(tinf[x,"Yes"],paste0(treeNum,"-"),"")}))))
     tinf[,"No"] <- suppressWarnings(as.numeric(unlist(lapply(1:(treeEndRow- treeBeginRow + 1),
                                                      function(x){str_replace(tinf[x,"No"],paste0(treeNum,"-"),"")}))))
     tinf[,"Missing"] <- suppressWarnings(as.numeric(unlist(lapply(1:(treeEndRow- treeBeginRow + 1),
                                                      function(x){str_replace(tinf[x,"Missing"],paste0(treeNum,"-"),"")}))))

     if(is.na(tinf[1,'Split'])){
       nodeF <- append.xmlNode(xmlNode("Node",attrs=c(score=tinf[1,'prediction'])),xmlNode("True"))
     } else {
       nodeList <- .make3Nodes(tinf)
       nodeF <- .make3Tree(nodeList,tinf)
     }
     # PMML -> TreeModel
     tree.model <- xmlNode("TreeModel", 
                          attrs=c(modelName=model.name, functionName="regression", algorithmName="xgboost", 
                          splitCharacteristic="multiSplit"))
     # PMML -> TreeModel -> MiningSchema
     tree.model <- append.XMLNode(tree.model, .pmmlMiningSchema(field, target,unknownValue=unknownValue,
                                                                invalidValueTreatment=childInvalidValueTreatment))
     treeOutput <- xmlNode("Output")
     treeOutputField <- xmlNode("OutputField",attrs=c(name=paste0("predictedValueTree",treeNum),dataType="double",optype="continuous",feature="predictedValue"))
     treeOutput <- append.xmlNode(treeOutput,treeOutputField)
     tree.model <- append.XMLNode(tree.model,treeOutput)

     # Add to the top level structure.
     segment <- xmlNode("Segment",attrs=c(id=treeNum))
     tru <- xmlNode("True")
     segment <- append.XMLNode(segment, tru)

     tree.model <- append.XMLNode(tree.model, nodeF)
     segment <- append.XMLNode(segment, tree.model)
     
     return(segment)
  }

.make3Tree <- function(nodeList,splitInfo)
{
  listLength <- as.integer(length(nodeList)/3)
  nodeListL <- nodeList[1:listLength]
  nodeListR <- nodeList[(listLength+1):(2*listLength)]
  nodeListM <- nodeList[(2*listLength+1):length(nodeList)]

  while(listLength > 1) {
    if(splitInfo[listLength,2] == "Leaf") {
      listLength <- listLength - 1
      next()
    }
    pickL <- which(splitInfo[,4] == (listLength-1))
    if(length(pickL) != 0) {
      nodeListL[[pickL]] <- append.xmlNode(nodeListL[[pickL]],list(nodeListL[[listLength]],nodeListR[[listLength]],nodeListM[[listLength]]))
    }
    pickR <- which(splitInfo[,5] == (listLength-1))
    if(length(pickR) != 0) {
      nodeListR[[pickR]] <- append.xmlNode(nodeListR[[pickR]],list(nodeListL[[listLength]],nodeListR[[listLength]],nodeListM[[listLength]]))
    }
    pickM <- which(splitInfo[,6] == (listLength-1))
    if(length(pickM) != 0) {
      nodeListM[[pickM]] <- append.xmlNode(nodeListM[[pickM]],list(nodeListL[[listLength]],nodeListR[[listLength]],nodeListM[[listLength]]))
    }
    listLength <- listLength - 1
  }

  nodeT <- append.xmlNode(xmlNode("Node"),xmlNode("True"))
  nodeT <- append.xmlNode(nodeT,list(nodeListL[[1]],nodeListR[[1]],nodeListM[[1]]))
  return(nodeT)
}


  .make3Nodes <- function(tinf)
  {
    # listLength <- min(which(is.na(tinf[,4])))-1
    listLength <- max(which(!is.na(tinf[,4])))
    nodeListLeft <- vector(mode="list",length=listLength)
    nodeListRight <- vector(mode="list",length=listLength)
    nodeListMissing <- vector(mode="list",length=listLength)
    for(i in 1:nrow(tinf)) {
      if(tinf[i,2] == "Leaf")
        next()

      if(!is.na(tinf[tinf[i,4]+1,7])) {
        node <- xmlNode("Node",attrs=c(score=tinf[tinf[i,4]+1,7]))
      } else {
        node <- xmlNode("Node")
      }
      predL <- xmlNode("SimplePredicate",attrs=c(field=as.character(tinf[i,2]),operator="lessThan",value=as.character(tinf[i,3])))
      nodeListLeft[[i]] <- append.xmlNode(node,predL)

      if(!is.na(tinf[tinf[i,5]+1,7])) {
        node <- xmlNode("Node",attrs=c(score=tinf[tinf[i,5]+1,7]))
      } else {
        node <- xmlNode("Node")
      }
      predR <- xmlNode("SimplePredicate",attrs=c(field=as.character(tinf[i,2]),operator="greaterOrEqual",value=as.character(tinf[i,3])))
      nodeListRight[[i]] <- append.xmlNode(node,predR)

      if(!is.na(tinf[tinf[i,6]+1,7])) {
        node <- xmlNode("Node",attrs=c(score=tinf[tinf[i,6]+1,7]))
      } else {
        node <- xmlNode("Node")
      }
      predM <- xmlNode("SimplePredicate",attrs=c(field=as.character(tinf[i,2]),operator="isMissing"))
      nodeListMissing[[i]] <- append.xmlNode(node,predM)
    }

    return(c(nodeListLeft,nodeListRight,nodeListMissing))
  }

.makeClassificationRegressionModel <- function(field,target,outputCategories,numTrees,type,objective,childInvalidValueTreatment)
{
  numCategories <- length(outputCategories)
  numTreesPerCategories <- as.integer(numTrees/numCategories)
  
  regrSegment <- append.xmlNode(xmlNode("Segment"),xmlNode("True"))
  if(type=="regression") {
    regrModel <- xmlNode("RegressionModel",attrs=c(functionName="regression",modelName="CollectingModel"))
  } else {
    regrModel <- xmlNode("RegressionModel",attrs=c(functionName="classification",normalizationMethod="softmax",modelName="CollectingModel"))
  }
  # regrModel <- append.xmlNode(regrModel,.pmmlMiningSchema(field,target))
  ms <- xmlNode("MiningSchema")
  mfs <- lapply(1:numTrees,function(x){xmlNode("MiningField",attrs=c(name=paste0("predictedValueTree",x-1),
                                                                     usageType="active",optype="continuous",
                                                                     invalidValueTreatment=childInvalidValueTreatment))})
  ms <- append.xmlNode(ms,mfs)
  regrModel <- append.xmlNode(regrModel,ms)

  ltNode <- xmlNode("LocalTransformations")
  if(objective=="binary:logistic") {
    dfNode <- xmlNode("DerivedField",attrs=c(name=paste0("SumCat",outputCategories[1]),dataType="double",optype="continuous"))
    applyNode <- xmlNode("Apply",attrs=c('function'="sum"))
    summedNodes <- lapply(seq(1,numTrees,1),function(x){xmlNode("FieldRef",attrs=c(field=paste0("predictedValueTree",x-1)))})
    applyNode <- append.xmlNode(applyNode,summedNodes)
    dfNode <- append.xmlNode(dfNode,applyNode)
    ltNode <- append.xmlNode(ltNode,dfNode)
    
    regrModel <- append.XMLNode(regrModel,ltNode)

    tableElement <- xmlNode("RegressionTable",attrs=c(intercept="0",targetCategory=outputCategories[1]))
    numericElement <- xmlNode("NumericPredictor",attrs=c(name=paste0("SumCat",outputCategories[1]),coefficient="0.0"))
    tableElement <- append.xmlNode(tableElement,numericElement)
    regrModel <- append.xmlNode(regrModel,tableElement)
    tableElement <- xmlNode("RegressionTable",attrs=c(intercept="0",targetCategory=outputCategories[2]))
    numericElement <- xmlNode("NumericPredictor",attrs=c(name=paste0("SumCat",outputCategories[1]),coefficient="-1.0"))
    tableElement <- append.xmlNode(tableElement,numericElement)
    regrModel <- append.xmlNode(regrModel,tableElement) 
  } else {
    for(i in 1:numCategories) {
      dfNode <- xmlNode("DerivedField",attrs=c(name=paste0("SumCat",outputCategories[i]),dataType="double",optype="continuous"))
      applyNode <- xmlNode("Apply",attrs=c('function'="sum"))
      summedNodes <- lapply(seq(i,numTrees,numCategories),function(x){xmlNode("FieldRef",attrs=c(field=paste0("predictedValueTree",x-1)))})
      applyNode <- append.xmlNode(applyNode,summedNodes)
      dfNode <- append.xmlNode(dfNode,applyNode)
      ltNode <- append.xmlNode(ltNode,dfNode)
    }
    regrModel <- append.XMLNode(regrModel,ltNode)
    for(i in 1:numCategories) {
      tableElement <- xmlNode("RegressionTable",attrs=c(intercept="0",targetCategory=outputCategories[i]))
      numericElement <- xmlNode("NumericPredictor",attrs=c(name=paste0("SumCat",outputCategories[i]),coefficient="1.0"))
      tableElement <- append.xmlNode(tableElement,numericElement)
      regrModel <- append.xmlNode(regrModel,tableElement)
    }
  }
  return(regrModel)
}
