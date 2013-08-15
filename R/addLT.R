# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Add Transformations information to preexisting PMML model
#
# Time-stamp: <2013-08-10 19:48:25 Tridivesh Jena>
#
#
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# For a copy of the GNU General Public License, please see 
# <http://www.gnu.org/licenses/>.
#######################################################################
# Possible future functions:
# Node -> InternalNode
# nodeStr <- toString.XMLNode(Node)
# internalDocument <- xmlTreeParse(nodeStr,asText=TRUE,useInternalNodes=TRUE)
# internalNode <- getNodeSet(internalDocument,"/")[[1]]
#
# InternalNode -> Node
# internalNodeStr <- toString.XMLNode(internalNode)
# internalNodeDocument <- xmlTreeParse(internalNodeStr,asText=TRUE,useInternalNodes=FALSE)
# node <- internalNodeDocument$doc$children[[1]]
# XMLInternalDocument -> XMLNode
# xmlTreeParse(toString.XMLNode(modelInternalDocument),useInternalNodes=FALSE)$doc$children[[1]]
#
#######################################################################
#
# Brief summary of useful commands from the XML package...most are needed due to the 
# unecessarily large variety of XML storage formats
#
# getNodeSet(XMLInternalDocument) -> list(XMLInternalNode)
# xmlTreeParse(file, useInternalNodes=FALSE) -> XMLDocument
# xmlTreeParse(file, useInternalNodes=TRUE) -> XMLInternalDocument 
# from 2 steps above: XMLDocument$doc$children -> XMLNode
# string <- toString.XMLNode(XMLNode)  or XMLInternalNode
# xmlTreeParse(string,asText=TRUE,useInternalNodes=TRUE/FALSE) same as before
# addChildren(XMLNode,XMLNode) -> new XMLNode
# addChildren(XMLInternalNode, XMLInternalNode, at=integer) -> same previous XMLInternalNode, modified
# xmlParent(XMLInternalNode) -> XMLInternalNode
# pmml is XMLNode, saveXML can save XMLInternalNode as XMLNode
#
# getNodeSet(XMLInternalDocument,xpath,namespace)
# if namespace is p=http://www.dmg.org/PMML-4_1  then Model is p:Model
# Eg: predicted MiningField from PMML model is 
# ans <- getNodeSet(pmml,"/p:PMML/*/p:MiningSchema/child::*[attribute::name='predictedScore']",
#                   c(p="http://www.dmg.org/PMML-4_1"))
# Note we have to start from "/"   not "." 
# Once we extract an element from PMML using getNodeSet, namespace is "" and the next application
# doesnt require a namespace 
#

addLT <- function(xmlmodel=NULL, transforms=NULL) {

# we expect the input to be always a XML Node
# If a file is to be used, the function 'fileToXMLNode' should be used to convert the file to a XMLNode object

#  require(XML, quietly=TRUE)
  modelstring = toString.XMLNode(xmlmodel)
  modelInternalDocument <- xmlTreeParse(modelstring,asText=TRUE,useInternalNodes=TRUE)

# if no transforms, return xml model
# if input was filename, read it in, convert to xml, return 
  if(is.null(transforms))
  {
   return(xmlmodel)  
  }

# transforms is not null
# first read in the transform
  transformstring = toString.XMLNode(transforms)
  transformInternalDocument <- xmlTreeParse(transformstring,asText=TRUE,useInternalNodes=TRUE)

# Get DerivedField nodes from input transforms node
  dfNodeSet <- getNodeSet(transformInternalDocument,"/*/DerivedField")
  numdf <- length(dfNodeSet)
  discretizedFields <- list()

# DataDictionary and MiningSchema to be modified later
  dd <- getNodeSet(modelInternalDocument,"/p:PMML/p:DataDictionary",c(p="http://www.dmg.org/PMML-4_1"))[[1]]
  ms <- getNodeSet(modelInternalDocument,"/p:PMML/*/p:MiningSchema",c(p="http://www.dmg.org/PMML-4_1"))[[1]]

# xform using 1 derived field at a time
  for(i in 1:numdf)
  {
# derived field name and field name
    dfname <- getNodeSet(transformInternalDocument,"/*/DerivedField/@name")[[i]]
    fname <- getNodeSet(transformInternalDocument,"/*/DerivedField/descendant::*/attribute::field")[[i]]

# what kind of transformation is it? Different for each discrete type and else for continuous types
    ddfPath1 <- paste("/*/DerivedField[descendant::*/@field='",fname,"' and @name='",dfname,"']/child::NormDiscrete",sep="")
    xformName1 <- getNodeSet(transformInternalDocument,ddfPath1)
    ddfPath2 <- paste("/*/DerivedField[descendant::*/@field='",fname,"' and @name='",dfname,"']/descendant::MapValues",sep="")
    xformName2 <- getNodeSet(transformInternalDocument,ddfPath2)
    ddfPath3 <- paste("/*/DerivedField[@name='",dfname,"']/child::Discretize",sep="")
    xformName3 <- getNodeSet(transformInternalDocument,ddfPath3)
    if(length(xformName1) > 0)
    {
      origXformName <- "normdiscrete"
    } else if(length(xformName2) == 1)
    {
      origXformName <- "mapvalues"
    } else if(length(xformName3) == 1)
    {
      origXformName <- "discrete" 
    } else 
      origXformName <- "other"

    origName <- fname
    origDerivedName <- dfname
    found <- FALSE

    while(!found)
    {
#is there another field in LT to continue the chain?
      ddfPath <- paste("/*/DerivedField[descendant::*/@field='",dfname,"']",sep="")
      ddf <- getNodeSet(transformInternalDocument,ddfPath)
 
      if(length(ddf) == 1)
      {
#if so, does DD have the intermediate derived field name?
        ddfPath = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",dfname,"']",sep="")
        ddf = getNodeSet(modelInternalDocument,ddfPath,c(p="http://www.dmg.org/PMML-4_1"))

	if(length(ddf) == 1)
	{
#if so, delete it from DD
            deletePath <- paste("/p:PMML/p:DataDictionary/p:DataField[@name='",dfname,"']/preceding-sibling::*",sep="")
            deleteNodeSet <- getNodeSet(modelInternalDocument,deletePath,c(p="http://www.dmg.org/PMML-4_1"))

            if(!is.null(deleteNodeSet))
            {
              position = length(deleteNodeSet) + 1
              dd <- removeChildren(dd,position)
            }
#delete from MiningSchema as well
            deletePath <- paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",dfname,"']/preceding-sibling::*",sep="")
            deleteNodeSet <- getNodeSet(modelInternalDocument,deletePath,c(p="http://www.dmg.org/PMML-4_1"))
            if(!is.null(deleteNodeSet))
            {
              position = length(deleteNodeSet) + 1
              ms <- removeChildren(ms,position)
            }
	}
#go to next step of chain
        ddfPath1 <- paste("/*/DerivedField[descendant::*/@field='",dfname,"']/@name",sep="")
        ddfPath2 <- paste("/*/DerivedField[descendant::*/@field='",dfname,"']/*/attribute::field",sep="")
        dfname <- getNodeSet(transformInternalDocument,ddfPath1)[[1]]
        fname <- getNodeSet(transformInternalDocument,ddfPath2)[[1]] 
      } else
      {
#end of chain; find derived field in DD and replace by original name
#final derived field name is dfname; find in DD
	found <- TRUE
        ddfPath = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",dfname,"']",sep="")
        ddf = getNodeSet(modelInternalDocument,ddfPath,c(p="http://www.dmg.org/PMML-4_1"))

# use initial xform function to decide what to do in DD:
# General strategy: given a xform: field --> derivedField
# if field exists in DD already, just delete the DD field of the derived field
# also, find all categorical values of field in transforms, check if it already exists in DD
# and if not, add it. 
# The MiningField already exists, so just delete the one of the derived field
# If field does not already exist in DD:
# add the field in DD, delete the derived field there, same for MiningSchema
# 
# We must divide each step into 2 parts...what to do if the field already exists in DD or not
# This is to take care of the case where the same field is used to derived more than 1 derived fields
# presently used.....else we would get the same field more than once in the DD etc
  
	if(origXformName == "discrete")
	{
	 ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']",sep="")
         ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
	 if(length(ddf1) != 0)
	 {
# field already exists
# delete the derived field in DD
	  ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
          ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
          dd <- removeChildren(dd,length(ddf1)+1)
# find all categorical values of the derived field listed in DD
          ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Value",sep="")
          ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
          if(length(ddf1) == 0)
          {
# here and always later; if new values were added previously by this code, they do not have the same namespace
# sp for eg: p:Value will then not be found but Value will be found
            ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Value",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
          }
# discrete means field must be continuous. So delete all categorical values
	  if(length(ddf1) != 0)
 	  {
           for(i in 1:length(ddf1))
	   {
            ddf <- removeChildren(ddf,"Value")
	   }
	  }

# delete derived field in MS
          ddfPath1 = paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
          ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
          ms <- removeChildren(ms,length(ddf1)+1)
	  } else  
	  {
# field is a new field
# Add the field as a new field, discrete => double, continuous
	    ddf <- ddf[[1]]
	    ddf <- addAttributes(ddf,name=origName)
	    ddf <- addAttributes(ddf,dataType="double")
	    ddf <- addAttributes(ddf,optype="continuous")

# as before, delete all categorical values listed in DD
            ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Value",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            if(length(ddf1) == 0)
            {
             ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Value",sep="")
             ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            }
            if(length(ddf1) != 0)
            {
             for(i in 1:length(ddf1))
             {
              ddf <- removeChildren(ddf,"Value")
             }
            }

# find derived field in MS and simply rename the name to the field name
            msfPath <- paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",dfname,"']",sep="")
            msf <- getNodeSet(modelInternalDocument,msfPath,c(p="http://www.dmg.org/PMML-4_1"))
            if(length(msf) == 1)
            {
             msf <- msf[[1]]
             msf <- addAttributes(msf,name=origName)
            }
	  }
	} else if(origXformName == "normdiscrete")
	{
          ddfPath0 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']",sep="")
          ddf0 = getNodeSet(modelInternalDocument,ddfPath0,c(p="http://www.dmg.org/PMML-4_1"))
          ddfPath2 = paste("/*/DerivedField[descendant::NormDiscrete/@field='",origName,"']/NormDiscrete/@value",sep="")
          ddf2 = getNodeSet(transformInternalDocument,ddfPath2)
	  if(length(ddf0) == 0)
	  {
# field not in DD
	   if(!(origName %in% discretizedFields))
           {
# normdiscrete takes 1 field and makes multiple derived fields
# the first time we see a normdiscrete, find all other derived fields from the same field and make them
# values of the field.
# the condition above ensures that we do not repeat the next time in the list we see the derived field from the same field

	     discretizedFields <- c(discretizedFields,origName)

# add field in DD
             if(length(ddf) == 1)
             {
               ddf <- ddf[[1]]
               ddf <- addAttributes(ddf,name=origName)
               ddf <- addAttributes(ddf,dataType="string")
               ddf <- addAttributes(ddf,optype="categorical")
             }
             ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Value/@value",sep="")
             ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
             if(length(ddf1) == 0)
             {
               ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Value/@value",sep="")
               ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
             }
             ddfPath2 = paste("/*/DerivedField[descendant::NormDiscrete/@field='",origName,"']/NormDiscrete/@value",sep="")
             ddf2 = getNodeSet(transformInternalDocument,ddfPath2)

# ddf2 collects all possible categories of the field; now add them to DD
             if(length(ddf2) != 0)
             {
              for(i in 1:length(ddf2))
	      {
		if(!(ddf2[i] %in% ddf1))
		{
# ddf2 is attributes, so we use paste to covert to string object to make new node
                 val <- paste(ddf2[[i]],sep="")
                 node <- newXMLNode("Value",attrs=c(value=val))
                 ddf <- addChildren(ddf, node)
		}
	      }
             }

# replace derived field in MS by field
             msfPath <- paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",dfname,"']",sep="")
             msf <- getNodeSet(modelInternalDocument,msfPath,c(p="http://www.dmg.org/PMML-4_1"))
             if(length(msf) == 1)
             {
               msf <- msf[[1]]
               msf <- addAttributes(msf,name=origName)
             }	      
           } else
	   {

# already take care of this derived field above; just delete it from DD and MS
            ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            dd <- removeChildren(dd,length(ddf1)+1)

            ddfPath1 = paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            ms <- removeChildren(ms,length(ddf1)+1)
	   }
	  } else
	  {
# field already in DD
           if(!(origName %in% discretizedFields))
           {
# collect all possible categories and add them if not already there
             discretizedFields <- c(discretizedFields,origName)
             ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Value/@value",sep="")
             ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
	     if(length(ddf1) == 0)
	     {
	       ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Value/@value",sep="")
               ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
	     }
             ddfPath2 = paste("/*/DerivedField[descendant::NormDiscrete/@field='",origName,"']/NormDiscrete/@value",sep="")
             ddf2 = getNodeSet(transformInternalDocument,ddfPath2)

             if(length(ddf2) != 0)
             {
              for(i in 1:length(ddf2))
              {
                if(!(ddf2[i] %in% ddf1))
                {
                 val <- paste(ddf2[[i]],sep="")
                 node <- newXMLNode("Value",attrs=c(value=val))
                 ddf0[[1]] <- addChildren(ddf0[[1]], node)
                }
              }
             }

# now delete the derived field from DD and MS
            ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            dd <- removeChildren(dd,length(ddf1)+1)

            ddfPath1 = paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            ms <- removeChildren(ms,length(ddf1)+1)
           } else
	   {
# the next derived field from normdiscrete of the same field.
# Took care of it above, so just delete it from DD and MS
            ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            dd <- removeChildren(dd,length(ddf1)+1)

            ddfPath1 = paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
            ms <- removeChildren(ms,length(ddf1)+1)
 	   }
	  }
	} else if(origXformName == "mapvalues")
	{
          ddfPath00 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']",sep="")
          ddf00 = getNodeSet(modelInternalDocument,ddfPath00,c(p="http://www.dmg.org/PMML-4_1"))

# This is not needed as it is redefined inside the next if statement: CHECK
          ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Value/@value",sep="")
          ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
	  if(length(ddf1) == 0)
	  {
	    ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Value/@value",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
	  }

# find all inputs from the MapValues node
          ddfPath0 <- paste("/*/DerivedField[descendant::*/@field='",origName,"']/descendant::FieldColumnPair/@column",sep="")
          inputColumn <- getNodeSet(transformInternalDocument,ddfPath0)
          ddfPath2 = paste("/*/DerivedField[descendant::*/@field='",origName,"']/descendant::",inputColumn[[1]],sep="")
          ddf2 = getNodeSet(transformInternalDocument,ddfPath2)

	  if(length(ddf00) == 0)
	  {
# field is new
# add field; mapvalues assumes here that it is string, categorical
	   ddf <- ddf[[1]]
           ddf <- addAttributes(ddf,name=origName)
           ddf <- addAttributes(ddf,dataType="string")
           ddf <- addAttributes(ddf,optype="categorical")

# Check: probably not needed as new field implies already existing values don't already exist          
           ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Value",sep="")
           ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
           if(length(ddf1) == 0)
           {
            ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Value",sep="")
            ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
           }
           if(length(ddf1) != 0)
           {
            for(i in 1:length(ddf1))
            {
             ddf <- removeChildren(ddf,"Value")
            }
           }

# add possible values
           if(length(ddf2) != 0)
           {
            for(i in 1:length(ddf2))
	    {
	     if(is.null(ddf1))
		ddf1 <- list()
	     if(!(xmlValue(ddf2[[i]]) %in% ddf1))
	     {
# ddf2 here is a node and unlike discretize, pasting a node will give both node name and value
# So we use xmlValue to get the value of the node 
	      val <- xmlValue(ddf2[[i]])
	      node <- newXMLNode("Value",attrs=c(value=val))
              ddf <- addChildren(ddf, node)
	     }
	    } 
           }

#rename MS fderived field with field
           msfPath <- paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",dfname,"']",sep="")
           msf <- getNodeSet(modelInternalDocument,msfPath,c(p="http://www.dmg.org/PMML-4_1"))
           if(length(msf) == 1)
           {
            msf <- msf[[1]]
            msf <- addAttributes(msf,name=origName)
           }
	  } else
	  {
# field already exists
# add values to the pre-existing field node if not already there
           if(length(ddf2) != 0)
           {
            for(i in 1:length(ddf2))
            {
             if(!(xmlValue(ddf2[[i]]) %in% ddf1))
             {
              val <- paste(xmlValue(ddf2[[i]]),sep="")
              node <- newXMLNode("Value",attrs=c(value=val))
              ddf00[[1]] <- addChildren(ddf00[[1]], node)
             }
            }
           }

# now delete the derived field from DD and MS
           ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
           ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
           dd <- removeChildren(dd,length(ddf1)+1)

           ddfPath1 = paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",origDerivedName,"']/preceding-sibling::*",sep="")
           ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
           ms <- removeChildren(ms,length(ddf1)+1)
	  }
      } else
      {
# all other cases: the continuous ones
# simply replace deived field name in DD with field name
        if(length(ddf) == 1)
        {
          ddf <- ddf[[1]]
          ddf <- addAttributes(ddf,name=origName)
        }
# if DD had intervals, delete them. This proper interval will be defined in LT
        ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::p:Interval",sep="")
        ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
        if(length(ddf1) == 0)
        {
          ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField[@name='",origName,"']/child::Interval",sep="")
          ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
        }
        if(length(ddf1) != 0)
        {
         for(i in 1:length(ddf1))
         {
          ddf <- removeChildren(ddf,"Interval")
         }
        }

#find in MS and replace as well
        msfPath <- paste("/p:PMML/*/p:MiningSchema/p:MiningField[@name='",dfname,"']",sep="")
        msf <- getNodeSet(modelInternalDocument,msfPath,c(p="http://www.dmg.org/PMML-4_1"))
        if(length(msf) == 1)
        {
          msf <- msf[[1]]
          msf <- addAttributes(msf,name=origName)
        }
      }
    }
   }
  }
# TODO: remove all duplicate names from DD and MS

# Does input PMML have LocalTransformations already?
  ltNodeSet <- getNodeSet(modelInternalDocument,"/p:PMML/*/p:LocalTransformations",c(p="http://www.dmg.org/PMML-4_1"))

# if not, just add the whole input LocalTransformations element
# else add as last children in LT 
  position = 0 
  if(is.null(ltNodeSet) || length(ltNodeSet)==0) 
  {
# go through the expected sequence of elements; find the last element from the sequence
# (parent model)->Extension->MiningSchema->Output->ModelStats->ModelExplanation->Targets
    ms <- getNodeSet(modelInternalDocument,"/p:PMML/*/p:MiningSchema",c(p="http://www.dmg.org/PMML-4_1"))[[1]]

    if(is.null(ms) || length(ms)==0)
      stop("MiningSchema element is required.")

    mdl <- xmlParent(ms)

    if(grepl("Extension",toString.XMLNode(getNodeSet(mdl,"./*"))))
      position = position + 1

    # can skip this, but not...just to be nicer looking! 
    if(grepl("MiningSchema",toString.XMLNode(getNodeSet(mdl,"./*"))))
      position = position + 1

    if(grepl("Output",toString.XMLNode(getNodeSet(mdl,"./*"))))
      position = position + 1

    if(grepl("ModelStats",toString.XMLNode(getNodeSet(mdl,"./*"))))
      position = position + 1

    if(grepl("ModelExplanation",toString.XMLNode(getNodeSet(mdl,"./*"))))
      position = position + 1

    if(grepl("Targets",toString.XMLNode(getNodeSet(mdl,"./*"))))
      position = position + 1

    ltNode <- getNodeSet(transformInternalDocument,"/*")[[1]]

    addChildren(mdl,ltNode,at=position)
  } else
  {
    addChildren(ltNodeSet[[1]],dfNodeSet,at=0)
  }  

  ddfPath1 = paste("/p:PMML/p:DataDictionary/p:DataField",sep="")
  ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
  if(length(ddf1) == 0)
  {
    ddfPath1 = paste("/p:PMML/p:DataDictionary/DataField",sep="")
    ddf1 = getNodeSet(modelInternalDocument,ddfPath1,c(p="http://www.dmg.org/PMML-4_1"))
  }
  if(length(ddf1) != 0)
  {
    dd <- addAttributes(dd,numberOfFields=length(ddf1))
  }


  returnNode <- xmlTreeParse(toString.XMLNode(modelInternalDocument),useInternalNodes=FALSE)$doc$children[[1]]
  returnNode <- removeAttributes(returnNode,"schemaLocation")

  return(returnNode) 
}

