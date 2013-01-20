# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-06-21 14:50:31 Graham Williams>
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

pmml.rsf <- function(model,
                     model.name="rsfForest_Model",
                     app.name="Rattle/PMML",
                     description="Random Survival Forest Tree Model",
                     copyright=NULL,
		     transforms=NULL, ...)
{
  # Based on RANDOM SURVIVAL FOREST 2.0.0, Copyright 2006, Cleveland Clinic
  # Original by Hemant Ishwaran and Udaya B. Kogalur
  # Unified with the pmml package by Graham Williams
  
  # Tridi 1/15/12
  # although seems logical, the constructed rsf model in R doesnt seem to require this.
  # remove this rather than force modeller to cast rsf object into a rsf,forest object  
  #  if (sum(inherits(model, c("rsf", "forest"), TRUE) == c(1, 2)) != 2)
  #    stop("Not a legitimate (rsf, forest) object")

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

  nativeArray <- model$forest$nativeArray
  if (is.null(nativeArray))
    stop("RSF nativeArray content is NULL. Please ensure object is valid.")
    
  numTrees <- length(as.vector(unique(nativeArray$treeID))) # Trees in forest
  
  timeInterest = model$timeInterest
  if (is.null(timeInterest))
    stop("RSF timeInterest content is NULL. Please ensure object is valid.")

  formula = model$formula
  if (is.null(formula))
    stop("RSF formula is NULL.  Please ensure the object is valid.")

  forestSeed = model$forest$seed
  if (is.null(forestSeed))
    stop("RSF forestSeed content is NULL.  Please ensure object is valid.")

  # PMML

  pmml <- .pmmlRootNode("4.1")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))

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

  pmml <- append.XMLNode(pmml, .pmmlDataDictionarySurv(field, model$predictedName, transformed=transforms))
  
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

  # <MiningModel>
  miningModelNode <- xmlNode("MiningModel", attrs=c(modelName="RrsfModel",functionName="regression")) 

  if (.supportTransformExport(transforms))
  {
    field <- .unifyTransforms(field, transforms)
    transforms <- .activateDependTransforms(transforms)
  }
  # MiningModel -> MiningSchema
  miningModelNode <- append.XMLNode(miningModelNode, .pmmlMiningSchemaSurv(field, model$predictedName, transformed=transforms))

  #Tridi: If interaction terms do exist, define a product in LocalTransformations and use
  # it as a model variable. This step is rare as randomForest seems to avoid multiplicative
  # terms.
  ltNode <- xmlNode("LocalTransformations")
  interact <- FALSE
  for(fld in 1:number.of.fields){
    if(length(grep(":",field$name[fld])) == 1){
     interact <- TRUE
     drvnode <- xmlNode("DerivedField",attrs=c(name=field$name[fld],optype="continuous",dataType="double"))
     applyNode <- xmlNode("Apply",attrs=c("function"="*"))
     for(fac in 1:length(strsplit(field$name[fld],":")[[1]])){
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
    mmodel <- append.XMLNode(mmodel, ltNode)

  # test of Zementis xform functions
  if(interact && !is.null(transforms))
  {
    ltNode <- pmmlLocalTransformations(field, transforms, ltNode)
    mmodel <- append.XMLNode(mmodel, ltNode)
  }
  if(!interact && !is.null(transforms))
  {
    mmodel <- append.XMLNode(mmodel,pmmlLocalTransformations(field, transforms, ltNode))
  }

  # ensemble method
  segmentationNode <- xmlNode("Segmentation",
			attrs=c(multipleModelMethod="average"))
#  miningModelNode <- append.XMLNode(miningModelNode,segmentationNode)
  
  # Loop through all trees in the forest and extract the data.

  for (b in 1:numTrees)
  {
    print(paste("Converting Tree",b," to PMML",sep=""))
    segmentNode <- xmlNode("Segment",attrs=c(id=b))
    predicateNode <- xmlNode("True")
    segmentNode <- append.XMLNode(segmentNode, predicateNode)

    treeName <- paste("Tree",b,sep="")
    treeModelNode <- xmlNode("TreeModel",
                             attrs=c(modelName=treeName, functionName="regression",
                               algorithmName="rsf",
                               splitCharacteristic="multiSplit"))

    # PMML --> TreeModel [b] -> MiningSchema
    
    treeModelNode <- append.XMLNode(treeModelNode, .pmmlMiningSchemaSurv(field, model$predictedName,NULL))
    
    # Global dependencies: (field$name, forest)
# Since different trees in the mining model cannot have different xformed field (7/12/2012)
# there is no need to define the LocalTransformations in each tree
#    ltNode <- xmlNode("LocalTransformations")
#    interact <- FALSE 
#    for(fld in 1:number.of.fields){
#      if(length(grep(":",field$name[fld])) == 1){
#       interact <- TRUE
#       drvnode <- xmlNode("DerivedField",attrs=c(name=field$name[fld],optype="continuous",
#                                                                 dataType="double"))
#       applyNode <- xmlNode("Apply",attrs=c("function"="*"))
#       for(fac in 1:length(strsplit(field$name[fld],":")[[1]])){
#         fldNode <- xmlNode("FieldRef",attrs=c(field=strsplit(field$name[fld],":")[[1]][fac]))
#         if(length(grep("as\\.factor\\(",fldNode)) == 1)
#           fldNode <- gsub("as.factor\\((\\w*)\\)","\\1", fldNode, perl=TRUE)
#         applyNode <- append.XMLNode(applyNode, fldNode) 
#       } 
#       drvnode <- append.XMLNode(drvnode, applyNode)
#      }
#      if(interact)
#        ltNode <- append.XMLNode(ltNode, drvnode)
#    }
#    if(interact)
#      treeModelNode <- append.XMLNode(treeModelNode, ltNode) 
    
    # Initialize the root node.  This differs from the rest of the
    # internal nodes in the PMML structure.

    treeRoot <- xmlNode("Node", attrs=c(score=0,id=recursiveOutput$offset))
    treeRoot <- append.XMLNode(treeRoot, xmlNode("True"))
    
    rootParmID <- nativeArray$parmID[recursiveOutput$offset] 
    rootSpltPT <- nativeArray$contPT[recursiveOutput$offset] 
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
      recursiveOutput <- .rsfMakeTree(recursiveOutput, nativeArray,
                                     field$name, b, -1, rootParmID,
                                     rootSpltPT,model)
      
      treeRoot <- append.XMLNode(treeRoot, recursiveOutput$internalNode)
      
      recursiveOutput$leafCount <- recursiveOutput$leafCount + 1
      
      # Create the right daughter nodes.  Note that the object node
      # content is irrelevant as input.

      recursiveOutput$internalNode <- NULL
      recursiveOutput <- .rsfMakeTree(recursiveOutput, nativeArray,
                                     field$name, b, +1, rootParmID,
                                     rootSpltPT,model)
      
      treeRoot <- append.XMLNode(treeRoot, recursiveOutput$internalNode)
      
    }
    
    # Add the current tree to the PMML data structure.

    treeModelNode <- append.XMLNode(treeModelNode, treeRoot)
    segmentNode <- append.XMLNode(segmentNode, treeModelNode)
    segmentationNode <- append.XMLNode(segmentationNode, segmentNode)
  }
  miningModelNode <- append.XMLNode(miningModelNode, segmentationNode)
  
  pmml <- append.XMLNode(pmml, miningModelNode)  
  return (pmml)
}

.rsfMakeTree <- function(recursiveObject, nativeArray, predictorNames,
                        b, daughter, splitParameter, splitValue, model)
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
  fwdSplitValue <- nativeArray$contPT[recursiveObject$offset]

  # Create the node that will be returned on this call.
  ident <- recursiveObject$offset
  if (fwdSplitParameter == 0)
  {
    rsfNode <- xmlNode("Node",attrs=c(id=ident))
    terminalFlag <- TRUE
  }
  else if (fwdSplitParameter != 0)
  {
    rsfNode <- xmlNode("Node",attrs=c(id=ident))
 
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
  pName <- predictorNames[splitParameter]
  if(length(grep("as\\.factor\\(",predictorNames[splitParameter])) == 1)
    pName <- gsub("as.factor\\((\\w*)\\)","\\1", predictorNames[splitParameter], perl=TRUE)
  rsfNode <- append.XMLNode(rsfNode,
                            xmlNode("SimplePredicate",
                                  attrs=c(field=pName,
                                    operator=parseString, value=splitValue)))

  # Increment the offset, always.

  recursiveObject$offset <- recursiveObject$offset + 1

  # Add nodes with time information

  if (terminalFlag == TRUE)
  {
    timeIntLength <- length(model$timeInterest)
    leafMaxLength <- length(model$time)
    treeJump <- timeIntLength * leafMaxLength
    node <- nativeArray$nodeID[ident]
    for(t in 1:timeIntLength)
    {
      nident <- paste(ident,t,sep="")
      if(t == 1)
      {
        nNode <- xmlNode("Node",attrs=c(score=model$cumHazard[((b-1)*treeJump)+node],id=nident))
        sPred <- xmlNode("SimplePredicate",attrs=c(field=model$predictedName,operator="lessOrEqual",value=model$timeInterest[t]))
        nNode <- append.XMLNode(nNode,sPred)
        rsfNode <- append.XMLNode(rsfNode,nNode)
      }
      else if(t == timeIntLength)
      {
        nNode <- xmlNode("Node",attrs=c(score=model$cumHazard[((b-1)*treeJump)+((t-1)*leafMaxLength)+node],id=nident))
        sPred <- xmlNode("SimplePredicate",attrs=c(field=model$predictedName,operator="greaterThan",value=model$timeInterest[t-1]))
        nNode <- append.XMLNode(nNode,sPred)
        rsfNode <- append.XMLNode(rsfNode,nNode)
      }
      else
      { 
        nNode <- xmlNode("Node",attrs=c(score=model$cumHazard[((b-1)*treeJump)+((t-1)*leafMaxLength)+node],id=nident))
        cPred <- xmlNode("CompoundPredicate",attrs=c(booleanOperator="and"))
        sPred1 <- xmlNode("SimplePredicate",attrs=c(field=model$predictedName,operator="greaterThan",value=model$timeInterest[t-1]))
        sPred2 <- xmlNode("SimplePredicate",attrs=c(field=model$predictedName,operator="lessOrEqual",value=model$timeInterest[t]))
        cPred <- append.XMLNode(cPred,sPred1)
        cPred <- append.XMLNode(cPred,sPred2)
        nNode <- append.XMLNode(nNode,cPred)
        rsfNode <- append.XMLNode(rsfNode,nNode)
      }
    }
  }

  # Parse left and then right, if this is not a terminal node.

  if (terminalFlag == FALSE)
  {
    # Parse left: Do not increment the leafCount.  Internally
    # increment the offset, always.  Note that the object node content
    # is irrelevant as input.

    recursiveObject$internalNode <- NULL
    recursiveObject <- .rsfMakeTree(recursiveObject, nativeArray,
                                   predictorNames, b, daughter = -1,
                                   fwdSplitParameter, fwdSplitValue, model)
    
    rsfNode <- append.XMLNode(rsfNode, recursiveObject$internalNode)
    
    # Parse right: Increment the leafCount.  Internally increment the
    # offset, always.  Note that the object node content is irrelevant
    # as input.
    
    recursiveObject$leafCount <- recursiveObject$leafCount + 1
    recursiveObject$internalNode <- NULL
    recursiveObject <- .rsfMakeTree(recursiveObject, nativeArray,
                                   predictorNames, b, daughter = +1,
                                   fwdSplitParameter, fwdSplitValue, model)
    
    rsfNode <- append.XMLNode(rsfNode, recursiveObject$internalNode)
    
  }
  
  # Modify the recursive object with the new internal node structure.

  recursiveObject$internalNode <- rsfNode
  
  return (recursiveObject)
  
}

