# PMML: Predictive Model Markup Language
#
# Copyright (c) 2009-2013, some parts by Togaware Pty Ltd and other by Zementis, Inc. 
#
# This file is part of the PMML package for R.
#
# The PMML package is free software: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of 
# the License, or (at your option) any later version.
#
# The PMML package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
######################################################################################

#######################################################################
# MAIN PMML FUNCTION
#
# modified 081513 to add implementation of transformations generator
# and transformations element addition;  tridivesh.jena@zementis.com
# 
pmml <- function(model=NULL,
                 model.name="Rattle_Model",
                 app.name="Rattle/PMML",
                 description=NULL,
                 copyright=NULL,
                 transforms=NULL,
                 ...)
{
  if(is.null(model) && !is.null(transforms))
  {
    field <- NULL
    field$name <- names(transforms$fieldData)
    field$class <- transforms$fieldData[,"dataType"]
    names(field$class) <- row.names(transforms$fieldData)

    return(.pmmlLocalTransformations(field, transforms, NULL))
  }
  else if(grepl("XMLNode",toString(class(model))))
  {
    return(.addLT(model, transforms))
  }
  else
  {
    UseMethod("pmml")
  }
}

########################################################################
# UTILITY FUNCTIONS

.markupSpecials <- function(x)
  gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", x)))

.generateCopyright <- function()
{
  return(paste("Copyright (c)", format(Sys.time(), "%Y"), Sys.info()["user"]))
}

.sdecimal2binary <- function(x)
{
  return(rev(.sdecimal2binary.smallEndian(x)))
}

.sdecimal2binary.smallEndian <- function(x)
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

# Function .pmmlRootNode

.pmmlRootNode <- function(version)
{
  if (version == "4.1")
    node <- xmlNode("PMML",
                    attrs=c(version="4.1",
                      xmlns="http://www.dmg.org/PMML-4_1",
                      "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance", 
                      "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-4_1",
                        "http://www.dmg.org/v4-1/pmml-4-1.xsd")))
  else
    node <- xmlNode("PMML",
                    attrs=c(version="4.0",
                      xmlns="http://www.dmg.org/PMML-4_0",
                      "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance",
                      "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-4_0",
                        "http://www.dmg.org/v4-0/pmml-4-0.xsd")))

  return(node)
}

.pmmlHeader <- function(description, copyright, app.name)
{
    if (is.null(copyright)) copyright <- .generateCopyright()
    
    # Header Node
    header <- xmlNode("Header", attrs=c(copyright=copyright, description=description))
    
    # Header -> Extension for user info
    header <- append.XMLNode(header, xmlNode("Extension", attrs=c(name="user",value=sprintf("%s", Sys.info()["user"]), extender=app.name)))
    
    # Header -> Application
    VERSION <- "1.4"
    header <- append.XMLNode(header, xmlNode("Application", attrs=c(name=app.name, version=VERSION)))
    
    # Header -> Timestamp
    header <- append.XMLNode(header, xmlNode("Timestamp", sprintf("%s", Sys.time())))
    
    return(header)
}


.pmmlLocalTransformations <- function(field, transforms=NULL, LTelement=NULL)
{
  # 090806 Generate and return a LocalTransformations element that incldues
  # each supplied field.
  #
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$name

  # LocalTransformations
  if(is.null(LTelement))
  {
   local.transformations <- xmlNode("LocalTransformations")
  }
  target <- field$name[1]

  if(!is.null(transforms))
  {
    inputs <- transforms$fieldData

# list of all fields derived from the target field
   targetDL <- NULL
   targetDL <- c(targetDL,target)

# not used code to make sure list of unique elements 
#       flist <- flist[!duplicate(flist)]

# code to output all fields, possibly to allow user to output any derived fields via OutputField element
   for(i in 1:nrow(inputs))
   {

    if(inputs[i,"origFieldName"] %in% targetDL)
    {
     targetDL <- c(targetDL,rownames(inputs)[i])
     if(rownames(inputs)[i] %in% field$name[-1])
     {
       stop("Target variable and derivations are not allowed to be used as input variables.")
     }
    } else 
    {
     fname <- rownames(inputs)[i]

     if(inputs[fname,"type"] == "derived" && fname != target)
     {
      if(inputs[fname,"transform"] == "zxform")
      {
       origName <- inputs[fname,"origFieldName"]
       missing <- inputs[fname,"missingValue"]
       dfNode <- xmlNode("DerivedField",attrs=c(name=fname,dataType="double",optype="continuous"))
       if(!is.na(missing))
       {
         ncNode <- xmlNode("NormContinuous",attrs=c(mapMissingTo=missing,field=origName))
       } else
       {
         ncNode <- xmlNode("NormContinuous",attrs=c(field=origName))     
       }

       o1 <- as.numeric(inputs[fname,"centers"])
       o2 <- as.numeric(inputs[fname,"centers"]) + as.numeric(inputs[fname,"scales"])
       lnNode1 <- xmlNode("LinearNorm",attrs=c(orig=o1,norm="0"))
       lnNode2 <- xmlNode("LinearNorm",attrs=c(orig=o2,norm="1"))

       ncNode <- append.XMLNode(ncNode, lnNode1)
       ncNode <- append.XMLNode(ncNode, lnNode2)
       dfNode <- append.XMLNode(dfNode, ncNode)
#       local.transformations <- append.XMLNode(local.transformations, dfNode)
      } else if(inputs[fname,"transform"] == "minmax")
      {
       origName <- inputs[fname,"origFieldName"]
       missing <- inputs[fname,"missingValue"]
       dfNode <- xmlNode("DerivedField",attrs=c(name=fname,dataType="double",optype="continuous"))
       if(!is.na(missing))
       {
         ncNode <- xmlNode("NormContinuous",attrs=c(mapMissingTo=missing,field=origName)) 
       } else
       {
         ncNode <- xmlNode("NormContinuous",attrs=c(field=origName)) 
       }

       o1 <- inputs[fname,"sampleMin"]
       n1 <- inputs[fname,"xformedMin"]
       o2 <- inputs[fname,"sampleMax"]
       n2 <- inputs[fname,"xformedMax"]
       lnNode1 <- xmlNode("LinearNorm",attrs=c(orig=o1,norm=n1))
       lnNode2 <- xmlNode("LinearNorm",attrs=c(orig=o2,norm=n2))
       ncNode <- append.XMLNode(ncNode, lnNode1)
       ncNode <- append.XMLNode(ncNode, lnNode2)
       dfNode <- append.XMLNode(dfNode, ncNode)
#       local.transformations <- append.XMLNode(local.transformations, dfNode)
      } else if(inputs[fname,"transform"] == "MapValues")
      {
       map <- inputs[fname,"fieldsMap"][[1]]

       dtype <- map[2,ncol(map)]
       if(dtype == "numeric")
       {
	dtype <- "double"
	otype <- "continuous"
       } else if(dtype == "boolean")
       {
        dtype <- "boolean"
        otype <- "categorical" 
       } else 
       {
        dtype <- "string"
	otype <- "categorical"
       }

       dfNode <- xmlNode("DerivedField",attrs=c(name=fname,dataType=as.character(dtype),optype=otype))
       default <- inputs[fname,"default"]
       missing <- inputs[fname,"missingValue"]
       if(dtype == "boolean")
       {
	if((default==1) || (toupper(default)==TRUE))
	{
	 default="true"
	} else
	{
	 default="false"
	}
        if((missing==1) || (toupper(missing)==TRUE))
        {
         missing="true"
        } else
        {
         missing="false"
        }
       }

       if(!is.na(default) && !is.na(missing))
       {
	mapvNode <- xmlNode("MapValues",attrs=c(mapMissingTo=missing,defaultValue=default,outputColumn="output"))
       } else if(!is.na(default) && is.na(missing))
       {
	mapvNode <- xmlNode("MapValues",attrs=c(defaultValue=default,outputColumn="output"))
       } else if(is.na(default) && !is.na(missing))
       {
	mapvNode <- xmlNode("MapValues",attrs=c(mapMissingTo=missing,outputColumn="output"))
       } else
       {
	mapvNode <- xmlNode("MapValues",attrs=c(outputColumn="out"))
       } 

       for(j in 1:(ncol(map)  - 1))
       {
	colname <- paste("input",j,sep="")
	val <- as.character(map[1,j])
	fcpNode <- xmlNode("FieldColumnPair",attrs=c(field=val,column=colname))
	mapvNode <- append.XMLNode(mapvNode,fcpNode)
       }

       inline <- xmlNode("InlineTable")
       for(j in 3:nrow(map))
       {
	row <- xmlNode("row")
	for(k in 1:(ncol(map) - 1))
	{
	 initNode <- xmlNode(paste("input",k,sep=""),value=as.character(map[j,k]))
         row <- append.XMLNode(row, initNode)
	}
	out <- xmlNode("output",value=as.character(map[j,ncol(map)]))
        row <- append.XMLNode(row, out)
	inline <- append.XMLNode(inline,row)
       }

       mapvNode <- append.XMLNode(mapvNode,inline)
       dfNode <- append.XMLNode(dfNode,mapvNode)
      } else if(inputs[fname,"transform"] == "NormDiscrete")
      {
       map <- inputs[fname,"fieldsMap"][[1]]
       dfName <- row.names(inputs)[i]
       missing <- inputs[fname,"missingValue"]
 
       dfNode <- xmlNode("DerivedField",attrs=c(name=dfName,dataType="double",optype="continuous"))
       if(!is.na(missing))
       {
         normNode <- xmlNode("NormDiscrete",attrs=c(field=as.character(inputs[fname,"origFieldName"]),value=as.character(map[1]),mapMissingTo=missing))
       } else
       {
         normNode <- xmlNode("NormDiscrete",attrs=c(field=as.character(inputs[fname,"origFieldName"]),value=as.character(map[1])))
       }
       dfNode <- append.XMLNode(dfNode,normNode)
      } else if(inputs[fname,"transform"] == "discretize")
      {
	maps <- inputs[fname,"fieldsMap"][[1]]
	missingVal <- inputs[fname,"missingValue"]
	defVal <- inputs[fname,"default"]

	origName <- inputs[fname,"origFieldName"] 
	map <- maps[c(-1,-2),]
	dtype <- as.character(inputs[fname,"dataType"])
        if(dtype == "numeric")
        {
         dtype <- "double"
         otype <- "continuous"
        }

	# The following doesnt work as there seems to be no way in PMML to have predicates
	# which indicate if a boolean variable is true or false; and this issue comes up
	# when derived fields of type boolean are used in Tree models
	# We cannot use the operator "isIn" as  in 
	#  <... booleanOperator="isIn> <Array type="string>"TRUE"</Attay
	# as then we need an array of type boolean
	# and that is not allowed; and ADAPA complains if boolean vaiable is tested as being 
	# contained in an Arraya of type string  
	# else if(dtype == "boolean")

        # {
        # dtype <- "boolean"
        # otype <- "categorical"
        # }

	else
        {
         dtype <- "string"
         otype <- "categorical"
        }

        if(dtype == "boolean")
        {
         if((default==1) || (toupper(default)==TRUE))
         {
          default="true"
         } else
         {
          default="false"
         }
         if((missing==1) || (toupper(missing)==TRUE))
         {
          missing="true"
         } else
         {
          missing="false"
         }
        }

	dfNode <- xmlNode("DerivedField",attrs=c(name=fname,dataType=dtype,optype=otype))
        if(!is.na(defVal) && !is.na(missingVal))
        {
         discNode <- xmlNode("Discretize",attrs=c(field=origName,mapMissingTo=missingVal,defaultValue=defVal)) 
        } else if(!is.na(defVal) && is.na(missingVal))
        {
         discNode <- xmlNode("Discretize",attrs=c(field=origName,defaultValue=defVal))
        } else if(is.na(defVal) && !is.na(missingVal))
        {
         discNode <- xmlNode("Discretize",attrs=c(field=origName,mapMissingTo=missingVal))
        } else
        {
         discNode <- xmlNode("Discretize",attrs=c(field=origName))
        }

	for(i in 1:nrow(map))
	{ 
	 dbinNode <- xmlNode("DiscretizeBin",attrs=c(binValue=map[i,2]))
	 clsr <- paste(map[1,3],map[i,5],sep="")
	 if(!is.na(map[i,4]))
  	 {
	  if(!is.na(map[i,6]))
	  {
	   intrNode <- xmlNode("Interval",attrs=c(closure=clsr,leftMargin=map[i,4],rightMargin=map[i,6]))
	  } else
	  {
	   intrNode <- xmlNode("Interval",attrs=c(closure=clsr,leftMargin=map[i,4]))
	  }
	 } else
	 {
	  intrNode <- xmlNode("Interval",attrs=c(closure=clsr,rightMargin=map[i,6]))
	 }
	 dbinNode <- append.XMLNode(dbinNode,intrNode)
	 discNode <- append.XMLNode(discNode,dbinNode)
	}
	dfNode <- append.XMLNode(dfNode,discNode) 

      }

      if(is.null(LTelement))
      {
       local.transformations <- append.XMLNode(local.transformations, dfNode)
      } else
      {
       LTelement <- append.XMLNode(LTelement, dfNode)
      }
     }
     }
    }
  }

  if(is.null(LTelement))
  {
   return(local.transformations)
  } else
  {
   return(LTelement)
  }
}

#####################################################################
# PMML Output element

.pmmlOutput <- function(field, target=NULL, optype=NULL)
{
  number.of.fields <- length(field$name)

  output <- xmlNode("Output")
  output.fields <- list()

  for (i in 1:number.of.fields)
  {
    if (field$name[i]==target)
    {
      targetout = target
      if(length(grep("as\\.factor\\(",targetout)) == 1)
      {
        targetout <- gsub("as.factor\\((\\w*)\\)","\\1", targetout, perl=TRUE)
      }

      if (is.null(optype))
        output.fields[[1]] <- xmlNode("OutputField",
                                      attrs=c(name=gsub(" ","",paste("Predicted_",targetout)),
                                        feature="predictedValue"))
      else
        output.fields[[1]] <- xmlNode("OutputField",
                                      attrs=c(name=gsub(" ","",paste("Predicted_",targetout)),
                                        optype=optype,
                                        dataType=ifelse(optype=="continuous",
                                          "double", "string"),
                                        feature="predictedValue"))

     {
      for (j in seq_along(field$levels[[field$name[i]]]))
        output.fields[[j+1]] <- xmlNode("OutputField",
                                        attrs=c(name=paste("Probability_",
                                                  field$levels[[field$name[i]]][j],
                                                  sep=""),
                                          optype="continuous",
                                          dataType = "double",
                                          feature="probability",
                                          value= field$levels[[field$name[i]]][j]))
     }
    }
  }
  
  output$children <- output.fields
  return(output)
}
