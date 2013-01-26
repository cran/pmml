# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2013-01-26 07:59:38 Graham Williams>
#
# Copyright (c) 2009-2012 Togaware Pty Ltd
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
# MAIN PMML FUNCTION

pmml <- function(model,
                 model.name="Rattle_Model",
                 app.name="Rattle/PMML",
                 description=NULL,
                 copyright=NULL,
                 transforms=NULL,
                 dataset=NULL,
                 ...)
  UseMethod("pmml")

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
  # 080621 Don't use a default since we should specify in the call
  # which version the code is targetting - for documentation purposes.

  ##  PMML.VERSION <- "3.1" 080621 Did I user this string anywhere for any
  ##  external purpose. Maybe not - so it should be removed.
  
  # 080615 - The xmlns= namespace addition causes a warning on calling
  # getNodeSet unless we specify the namespace there:
  # 	> doc <- xmlTreeParse("test.xml", useInternalNodes=TRUE)
  #     > els <- getNodeSet(doc, "/PMML/DataDictionary/DataField")
  #     Warning message:
  #     using http://www.dmg.org/PMML-3_1 as prefix for default
  #     namespace http://www.dmg.org/PMML-3_1 
  #     > els <- getNodeSet(doc, "//p:DataField", c(p="http://www.dmg.org/PMML-3_1"))
  # We are supposed to include a default namespace, so do so. 

  if (version == "3.1")
    node <- xmlNode("PMML",
                    attrs=c(version="3.1",
                      xmlns="http://www.dmg.org/PMML-3_1",
                      "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance"))
  else if (version == "3.2")
    node <- xmlNode("PMML",
                    attrs=c(version="3.2",
                      xmlns="http://www.dmg.org/PMML-3_2",
                      "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance", 
                      "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-3_2",
                        "http://www.dmg.org/v3-2/pmml-3-2.xsd")))
  else if (version == "4.1")
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
  # Header
  
  VERSION <- "1.2.34"
  DATE <- "2013-01-26"
  REVISION <- "27"

  if (is.null(copyright)) copyright <- .generateCopyright()
  header <- xmlNode("Header",
                    attrs=c(copyright=copyright, description=description))

  # Header -> User (Extension)
  #
  # 100519 wenching.lin@zementis.com pointed out that the DMG spec
  # requires the Extension to be first.
  
  header <- append.XMLNode(header, xmlNode("Extension",
                                           attrs=c(name="user",
                                             value=sprintf("%s",
                                               Sys.info()["user"]),
                                             extender=app.name)))

  # Header -> Application

  header <- append.XMLNode(header, xmlNode("Application",
                                           attrs=c(name=app.name,
                                             version=paste(VERSION, "r", REVISION, sep=""))))

  # Header -> Timestamp
						   
  header <- append.XMLNode(header,
                           xmlNode("Timestamp", sprintf("%s", Sys.time())))

  return(header)
}

.pmmlDataDictionary <- function(field, dataset=NULL, weights=NULL, transformed=NULL)
{
  # 090806 Generate and return a DataDictionary element that incldues
  # each supplied field.
  #
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$name
  # field$levels is indexed by fields$name
  #
  # 091003 If the dataset is supplied then also include an Interval
  # element within the DataField for each numeric variable.

  number.of.fields <- length(field$name)

  if(field$name[1] == "ZementisClusterIDPlaceHolder")
  {
   begin<-2
  } else 
  {
   begin <- 1
  }

  namelist <- list()
  optypelist <- list()
  datypelist <- NULL
  fname <- NULL
  data.fields <- list()
  # discrete place holder variable name as set in pmml.naiveBayes.R
  DPL1 <- "DiscretePlaceHolder"
  DPL2 <- "Temp"
  DPL3 <- "predictedScore"

  if(!is.null(transformed))
  {
   for(i in 1:nrow(transformed$fieldData))
   {
     # Determine the operation type

      type <- as.character(transformed$fieldData[i,"dataType"])
      if(type == "numeric")
      {
        datypelist[[row.names(transformed$fieldData)[i]]] <- "double"
      } else if(type == "logical")
      {
        datypelist[[row.names(transformed$fieldData)[i]]] <- "boolean"
      } else
      {
        datypelist[[row.names(transformed$fieldData)[i]]] <- "string"
      }

      if(type == "numeric")
      {
        optypelist[[row.names(transformed$fieldData)[i]]] <- "continuous"
      } else
      {
	optypelist[[row.names(transformed$fieldData)[i]]] <- "categorical"
      }

   }
   if(field$name[1] == "survival")
   {
    datypelist[[field$name[1]]] <- "double"
    optypelist[[field$name[1]]] <- "continuous"
   }
   if(DPL1 %in% field$name)
   {
     datypelist[[DPL1]] <- "string"
     optypelist[[DPL1]] <- "categorical"
   }
   if(DPL2 %in% field$name)
   {
     datypelist[[DPL2]] <- "string"
     optypelist[[DPL2]] <- "categorical"
   }
   if(DPL3 %in% field$name)
   {
     datypelist[[DPL3]] <- "double"
     optypelist[[DPL3]] <- "continuous"
   } 
  } else
  {
   for(i in begin:number.of.fields)
   {
   fname <- field$name[i]
   if(length(grep("as\\.factor\\(",field$name[i])) == 1)
#   if(length(grep("^as\\.factor\\(.*\\)",field$name[1])) == 1)
   {
        fname <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)
   }
     # Determine the operation type

    optype <- "UNKNOWN"
    datype <- "UNKNOWN"
    values <- NULL

    if (field$class[[field$name[i]]] == "numeric")
    {
      optypelist[[fname]] <- "continuous"
      datypelist[[fname]] <- "double"
    }
    else if (field$class[[field$name[i]]] == "logical")
    {
      optypelist[[fname]] <- "categorical"
      datypelist[[fname]] <- "boolean"
    }
    else if (field$class[[field$name[i]]] == "factor")
    {
      optypelist[[fname]] <- "categorical"
      datypelist[[fname]] <- "string"
    }
   }
  }

  for (i in begin:number.of.fields)
  {
    # DataDictionary -> DataField

     if(!is.null(transformed) && i!=1)
     {
       if(is.na(transformed$fieldData[field$name[i],"origFieldName"]))
       {
	if(is.na(transformed$fieldData[field$name[i],"transform"]))
	{
	 if(!(field$name[i] %in% namelist))
         {
          namelist <- c(namelist,field$name[i])
         }
	}
       } else
       {
	  if(transformed$fieldData[field$name[i],"transform"] == "MapValues")
	  {
	   ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
	   for(j in 1:length(ofname))
	   {
	    if(!(ofname[j] %in% namelist))
	    {
	     namelist <- c(namelist,ofname[j])
	    }
	   }
	   fname <- NA
	  }
	  else
	  {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
# following for loop not needed as multiple parents via mapvalues dealt with above
	   for(j in 1:length(ofname))
	   {
            fname <- ofname[j]
            while(!is.na(ofname[j]))
            {
             fname <- ofname[j]
	     xvalue <- transformed$fieldData[fname,"transform"]
	     if(!is.na(xvalue) && xvalue == "MapValues")
	     {
	      parents <- transformed$fieldData[fname,"origFieldName"][[1]]
	      for(j in 1:length(parents))
              {
               if(!(parents[j] %in% namelist))
               {
                namelist <- c(namelist,parents[j])
               }
              }
	      fname <- NA
	      break 
	     }
             ofname[j] <- transformed$fieldData[ofname[j],"origFieldName"][[1]]
            }
	    if(!(fname %in% namelist))
            {
             namelist <- c(namelist,fname)
            }
	   }
	  }

       }

     } else
     {

      fName <- field$name[i]
      if(!is.na(field$class[fName]) && field$class[fName] == "factor")
      {
       optypelist[[fName]] <- "categorical"
      }

      if(length(grep("as\\.factor\\(",field$name[i])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)

      if(!is.na(field$class[fName]) && field$class[fName] == "factor")
      {
       optypelist[[fName]] <- "categorical"
      }


      if(!(fName %in% namelist) && fName != "ZementisClusterIDPlaceHolder")
      {
       namelist <- c(namelist,fName)
      }

     }
  }

  # DataDictionary

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=length(namelist)))

  if (! is.null(weights) && length(weights))
    data.dictionary <-append.XMLNode(data.dictionary, xmlNode("Extension",
                                                              attrs=c(name="Weights",
                                                              value=weights, extender="Rattle")))

  nmbr <- 1
  for(ndf2 in 1:length(namelist))
  {
   if(!is.na(namelist[ndf2]))
   {
    optype <- optypelist[[namelist[ndf2][[1]]]]
    datype <- datypelist[[namelist[ndf2][[1]]]]
    data.fields[[nmbr]] <- xmlNode("DataField", attrs=c(name=namelist[ndf2],
                                             optype=optype, dataType=datype))

   }

   # DataDictionary -> DataField -> Interval

   fname <- namelist[ndf2][[1]]
   if (optypelist[[fname]] == "continuous" && !is.null(dataset) && fname != "survival")
   {
    dataval <- NULL
    for(j in 1:length(dataset[[fname]]))
    {
     dataval<-c(dataval,as.numeric(dataset[[fname]][j]))
    }

    interval <-  xmlNode("Interval",
                           attrs=c(closure="closedClosed", 
			     leftMargin=min(dataval, na.rm=TRUE), # 091025 Handle missing values
                             rightMargin=max(dataval, na.rm=TRUE))) # 091025 Handle missing values
    data.fields[[nmbr]] <- append.XMLNode(data.fields[[nmbr]], interval)
   }

   # DataDictionary -> DataField -> Value
   name <- namelist[nmbr][[1]]
   if (optypelist[[name]] == "categorical")
   {
     if(is.null(field$levels[[name]]) && !is.null(transformed))
     {
       lev <- levels(as.list(unique(transformed$data[name]))[[1]])
       for (j in seq_along(lev))
       {
         data.fields[[nmbr]][[j]] <- xmlNode("Value",
                          attrs=c(value=.markupSpecials(lev[j])))
       }
     } else
     {
       for (j in seq_along(field$levels[[namelist[nmbr][[1]]]]))
       {
         data.fields[[nmbr]][[j]] <- xmlNode("Value",
                          attrs=c(value=.markupSpecials(field$levels[[namelist[nmbr][[1]]]][j])))
       }
     }
   }

   data.dictionary <- append.XMLNode(data.dictionary, data.fields[[nmbr]])
   nmbr <- nmbr + 1
  }

  return(data.dictionary)
}

.pmmlDataDictionarySurv <- function(field, timeName, statusName, dataset=NULL, weights=NULL, transformed=NULL)
{
  # Tridi 012712
  # modify for a survival model. Survival forests do not typically have
  # a predicted field. Add a generic predicted field.If a predicted
  # field is included, this field will just be ignored by the model. 
  # 090806 Generate and return a DataDictionary element that incldues
  # each supplied field.
  #
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$name
  # field$levels is indexed by fields$name
  #
  # 091003 If the dataset is supplied then also include an Interval
  # element within the DataField for each numeric variable.

  number.of.fields <- length(field$name)
  ii<-0

  optypelist <- list()
  namelist <- list()
  datypelist <- list()
  data.fields <- list()

  if(field$name[1] == "ZementisClusterIDPlaceHolder")
  {
   begin <- 2
  } else 
  {
   begin <- 1
  }

  if(!is.null(transformed))
  {
   for(i in 1:nrow(transformed$fieldData))
   {
     # Determine the operation type

      type <- as.character(transformed$fieldData[i,"dataType"])
      if(type == "numeric")
      {
        datypelist[[row.names(transformed$fieldData)[i]]] <- "double"
      } else if(type == "logical")
      {
        datypelist[[row.names(transformed$fieldData)[i]]] <- "boolean"
      } else 
      {
        datypelist[[row.names(transformed$fieldData)[i]]] <- "categorical"
      }

      if(type == "numeric")
      {
        optypelist[[row.names(transformed$fieldData)[i]]] <- "continuous"
      } else
      {
        optypelist[[row.names(transformed$fieldData)[i]]] <- "categorical"
      }
   }
   if(field$name[1] == "survival")
   {
    datypelist[[field$name[1]]] <- "double"
    optypelist[[field$name[1]]] <- "continuous"
   }
  } else
  {
   for(i in begin:number.of.fields)
   {
     # Determine the operation type

    optype <- "UNKNOWN"
    datype <- "UNKNOWN"
    values <- NULL

    if (field$class[[field$name[i]]] == "numeric")
    {
      optypelist[[field$name[i]]] <- "continuous"
      datypelist[[field$name[i]]] <- "double"
    } else if (field$class[[field$name[i]]] == "logical")
    {
      optypelist[[field$name[i]]] <- "categorical"
      datypelist[[field$name[i]]] <- "boolean"
    }
    else if (field$class[[field$name[i]]] == "factor")
    {
      optypelist[[field$name[i]]] <- "categorical"
      datypelist[[field$name[i]]] <- "string"
    }
   }
  }

  for (i in 1:number.of.fields)
  {
    if(length(grep(":",field$name[i])) == 1){
    } else 
    {
    ii<-ii+1 

    # DataDictionary -> DataField
     if(!is.null(transformed))
     {
       if(is.na(transformed$fieldData[field$name[i],"origFieldName"]))
       {
        if(is.na(transformed$fieldData[field$name[i],"transform"]))
        {
         if(!(field$name[i] %in% namelist))
         {
          namelist <- c(namelist,field$name[i])
         }
        }
       } else
       {
          if(transformed$fieldData[field$name[i],"transform"] == "MapValues")
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
           for(j in 1:length(ofname))
           {
            if(!(ofname[j] %in% namelist))
            {
             namelist <- c(namelist,ofname[j])
            }
           }
           fname <- NA
          }
          else
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
# following for loop not needed as multiple parents via mapvalues dealt with above
           for(j in 1:length(ofname))
           {
            fname <- ofname[j]
            while(!is.na(ofname[j]))
            {
             fname <- ofname[j]
	     xvalue <- transformed$fieldData[fname,"transform"]
             if(!is.na(xvalue) && xvalue == "MapValues")
             {
              parents <- transformed$fieldData[fname,"origFieldName"][[1]]
              for(j in 1:length(parents))
              {
               if(!(parents[j] %in% namelist))
               {
                namelist <- c(namelist,parents[j])
               }
              }
              fname <- NA
              break
             }
             ofname[j] <- transformed$fieldData[ofname[j],"origFieldName"][[1]]
            }
            if(!(fname %in% namelist))
            {
             namelist <- c(namelist,fname)
            }
           }
          }
       }
     } else
# if no transforms
     {
      if(length(grep("as\\.factor\\(",field$name[ii])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[ii], perl=TRUE)
      else
        fName <- field$name[ii]

      data.fields[[ii]] <- xmlNode("DataField", attrs=c(name=fName,
                                                optype=optypelist[[fName]],
                                                dataType=datypelist[[fName]]))

      if(!(fName %in% namelist))
      {
       namelist <- c(namelist,fName)
      }
    
     }
    }
   }

    # DataDictionary -> DataField -> Interval
    nmbr <- 1
    for(ndf2 in 1:length(namelist))
    {
     fname <- namelist[[ndf2]]
     if (optypelist[[fname]] == "continuous" && ! is.null(dataset))
     {
      interval <-  xmlNode("Interval",
                           attrs=c(closure="closedClosed",
                             leftMargin=min(dataset[[namelist[[ndf2]]]],
                               na.rm=TRUE), # 091025 Handle missing values
                             rightMargin=max(dataset[[namelist[[nmbr]]]],
                               na.rm=TRUE))) # 091025 Handle missing values
      data.fields[[ii]] <- append.XMLNode(data.fields[[ii]], interval)
     }
    
    # DataDictionary -> DataField -> Value

    if (optypelist[[fname]] == "categorical")
    {
      if(is.null(field$levels[[fname]]) && !is.null(transformed))
      {
        lev <- levels(as.list(unique(transformed$data[fname]))[[1]])
        for (j in seq_along(lev))
        {
          data.fields[[ii]][[j]] <- xmlNode("Value",
                          attrs=c(value=.markupSpecials(lev[j])))
        }
      } else
      {
        for (j in seq_along(field$levels[[fname]]))
        {
          data.fields[[ii]][[j]] <- xmlNode("Value",
                          attrs=c(value=.markupSpecials(field$levels[[fname]][j])))
        }
      }
    }

#    if (optypelist[[fname]] == "categorical")
#      for (j in seq_along(field$levels[[fname]]))
#        data.fields[[ii]][j] <- xmlNode("Value",
#                                         attrs=c(value=
#                                           .markupSpecials(field$levels[[fname]][j])))
    }

  if (! is.null(weights) && length(weights))
    data.dictionary <-append.XMLNode(data.dictionary, xmlNode("Extension",
                                                              attrs=c(name="Weights",
                                                                value=weights,
                                                                extender="Rattle")))

  data.fields[[ii+1]] <- xmlNode("DataField", attrs=c(name=statusName,
                                                optype="continuous",dataType="double"))
  data.fields[[ii+2]] <- xmlNode("DataField", attrs=c(name=timeName,
                                                optype="continuous",dataType="double"))
  data.fields[[ii+3]] <- xmlNode("DataField", attrs=c(name="cumulativeHazard",
                                                optype="continuous",dataType="double"))

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=length(namelist)+3))
  data.dictionary <- append.XMLNode(data.dictionary, data.fields)


  return(data.dictionary)
}


.pmmlMiningSchema <- function(field, target=NULL, inactive=NULL, transformed=NULL)
{
  # Generate the PMML for the MinimgSchema element.

  # 091003 Currently we only include the name and usageType
  # attributes. We could also include relative importance (like a
  # correlation between 0 and 1), invalidValueTreatment (returnInvalid
  # to return a value indicating an invalid result; asis to return a
  # value without modification; asMissing to treat it as a missing
  # value and return the missingValueReplacement value instead),
  # missingValueReplacement, and outliers (asis, asMisingValues,
  # asExtremeValues).
  
  # 081103 Add inactive to list (as supplementary) those variables
  # that should be marked as inactive in the model. This was added so
  # that singularities can be identified as inactive for a linear
  # model. It could also be used to capture ignored variables, if they
  # were to ever be included in the variable list.

  number.of.fields <- length(field$name)
  mining.fields <- list()

  if(field$name[1] == "ZementisClusterIDPlaceHolder")
  {
   begin <- 2
  } else
  {
   begin <- 1
  }

  mining.schema <- xmlNode("MiningSchema")
  namelist <- NULL
  for (i in begin:number.of.fields)
  {
    # 081103 Find out which variables should be marked as
    # inactive. Currently the inactive list is often supplied from
    # lm/glm as the variables which result in singularities in the
    # model. However, for categorics, this is the indicator variable,
    # like GenderMale. The test for %in% fails! So as a quick fix use
    # grep. This is not a solution (because the variable Test is a
    # substring of TestAll, etc). 

    # 090328 Revert to the exact test. We need to be cleverer in what
    # we pass through in the inactive vector. Whilst GenderMale might
    # be NA and thus is the only value included for this categoric,
    # for a categoric with more levels we need to no treat the others
    # as inactive so the whole categroic itself should not be
    # inactive. For now, the simple reversion works. 090808 Move from
    # the use of inactive to supplementary to be in line with the DTD.
   
    # Once we allow transformed fields, fields not used directly may not
    # be supplementary as fields derived from them are active. Just output
    # all fields as active. 
    #if (field$name[i] %in% inactive) usage <- "supplementary"
    # 090328 if (length(grep(field$name[i], inactive))) usage <- "inactive"
    
    if(!is.null(transformed) && i!=1)
    {
       if(is.na(transformed$fieldData[field$name[i],"origFieldName"]))
       {
        if(is.na(transformed$fieldData[field$name[i],"transform"]))
        {
         if(!(field$name[i] %in% namelist))
         {
          namelist <- c(namelist,field$name[i])
         }
        }
       } else
       {
          if(transformed$fieldData[field$name[i],"transform"] == "MapValues")
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
           for(j in 1:length(ofname))
           {
            if(!(ofname[j] %in% namelist))
            {
             namelist <- c(namelist,ofname[j])
            }
           }
           fname <- NA
          }
          else
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
# following for loop not needed as multiple parents via mapvalues dealt with above
           for(j in 1:length(ofname))
           {
            fname <- ofname[j]
            while(!is.na(ofname[j]))
            {
             fname <- ofname[j]
	     xvalue <- transformed$fieldData[fname,"transform"]
             if(!is.na(xvalue) && xvalue == "MapValues")
             {
              parents <- transformed$fieldData[fname,"origFieldName"][[1]]
              for(j in 1:length(parents))
              {
               if(!(parents[j] %in% namelist))
               {
                namelist <- c(namelist,parents[j])
               }
              }
              fname <- NA
              break
             }
             ofname[j] <- transformed$fieldData[ofname[j],"origFieldName"][[1]]
            }
            if(!(fname %in% namelist))
            {
             namelist <- c(namelist,fname)
            }
           }
          }

       }
    } else
    {
      fName <- field$name[i]

      if(length(grep("as\\.factor\\(",field$name[i])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)

      if(!(fName %in% namelist) && fName != "ZementisClusterIDPlaceHolder")
      {
       namelist <- c(namelist,fName)
      }

    }
 
  }

  for(j in 1:length(namelist))
  {
   if(!is.na(namelist[j]))
   {

    if (is.null(target))
    {
      usage <- "active"
    } 
    else
    {
      usage <- ifelse(namelist[j] == target, "predicted", "active")
    }

    # if field name is the naive bayes categorical field place holder, add missingValueReplacement
    if(namelist[j]=="Temp" || namelist[j]=="DiscretePlaceHolder")
    {
      if(length(field$levels[[namelist[j]]])==1)
      {
        mf <- xmlNode("MiningField", attrs=c(name=namelist[j],
                            usageType=usage,missingValueReplacement=field$levels[[namelist[j]]]))
      }
    } else
    {
      mf <- xmlNode("MiningField", attrs=c(name=namelist[j],
                                                usageType=usage))
    }
    mining.schema <- append.XMLNode(mining.schema, mf)
   }
  }

  return(mining.schema)
}

.pmmlMiningSchemaRF <- function(field, target=NULL, inactive=NULL, transformed=NULL)
{
  # Generate the PMML for the MinimgSchema element.

  number.of.fields <- length(field$name)
  mining.fields <- list()

  namelist <- NULL
  for (i in 1:number.of.fields)
  {
    if (is.null(target))
      usage <- "active"
    else
      usage <- ifelse(field$name[i] == target, "predicted", "active")

    if (field$name[i] %in% inactive) usage <- "supplementary"

    if(!is.null(transformed))
    {
       if(is.na(transformed$fieldData[field$name[i],"origFieldName"]))
       {
        if(is.na(transformed$fieldData[field$name[i],"transform"]))
        {
         if(!(field$name[i] %in% namelist))
         {
          namelist <- c(namelist,field$name[i])
         }
        }
       } else
       {
          if(transformed$fieldData[field$name[i],"transform"] == "MapValues")
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
           for(j in 1:length(ofname))
           {
            if(!(ofname[j] %in% namelist))
            {
             namelist <- c(namelist,ofname[j])
            }
           }
           fname <- NA
          }
          else
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
# following for loop not needed as multiple parents via mapvalues dealt with above
           for(j in 1:length(ofname))
           {
            fname <- ofname[j]
            while(!is.na(ofname[j]))
            {
             fname <- ofname[j]
	     xvalue <- transformed$fieldData[fname,"transform"]
             if(!is.na(xvalue) && xvalue == "MapValues")
             {
              parents <- transformed$fieldData[fname,"origFieldName"][[1]]
              for(j in 1:length(parents))
              {
               if(!(parents[j] %in% namelist))
               {
                namelist <- c(namelist,parents[j])
               }
              }
              fname <- NA
              break
             }
             ofname[j] <- transformed$fieldData[ofname[j],"origFieldName"][[1]]
            }
            if(!(fname %in% namelist))
            {
             namelist <- c(namelist,fname)
            }
           }
          }

       }
      nmbr <- 1
      for(ndf2 in 1:length(namelist))
      {
       if(!is.na(namelist[ndf2]))
       {
        mining.fields[[nmbr]] <- xmlNode("MiningField", attrs=c(name=namelist[ndf2],
                                                usageType=usage))
        nmbr <- nmbr + 1
       }
      }
    } else
    { 
      mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field$name[i],
                                    usageType=usage))
    }


  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}

.pmmlMiningSchemaSurv <- function(field, timeName, statusName, target=NULL, inactive=NULL, transformed=NULL)
{
  # Tridi 012712
  # Generate the PMML for the MinimgSchema element for a survival model.
  # A survival forest has an output not usually in the input field names.
  # Just add an extra mining field of type predicted.

  # 091003 Currently we only include the name and usageType
  # attributes. We could also include relative importance (like a
  # correlation between 0 and 1), invalidValueTreatment (returnInvalid
  # to return a value indicating an invalid result; asis to return a
  # value without modification; asMissing to treat it as a missing
  # value and return the missingValueReplacement value instead),
  # missingValueReplacement, and outliers (asis, asMisingValues,
  # asExtremeValues).

  # 081103 Add inactive to list (as supplementary) those variables
  # that should be marked as inactive in the model. This was added so
  # that singularities can be identified as inactive for a linear
  # model. It could also be used to capture ignored variables, if they
  # were to ever be included in the variable list.

  namelist <- NULL
  number.of.fields <- length(field$name)
  mining.fields <- list()
  targetExists <- 0
  ii <- 0
  for (i in 1:number.of.fields)
  {
    if(length(grep(":",field$name[i])) == 1){
   } else {
    ii <- ii+1
    if (is.null(target))
      usage <- "active"
    else
      usage <- ifelse(field$name[i] == target, "predicted", "active")

    if (usage == "predicted")
     targetExists = 1

    # 081103 Find out which variables should be marked as
    # inactive. Currently the inactive list is often supplied from
    # lm/glm as the variables which result in singularities in the
    # model. However, for categorics, this is the indicator variable,
    # like GenderMale. The test for %in% fails! So as a quick fix use
    # grep. This is not a solution (because the variable Test is a
    # substring of TestAll, etc).

    # 090328 Revert to the exact test. We need to be cleverer in what
    # we pass through in the inactive vector. Whilst GenderMale might
    # be NA and thus is the only value included for this categoric,
    # for a categoric with more levels we need to no treat the others
    # as inactive so the whole categroic itself should not be
    # inactive. For now, the simple reversion works. 090808 Move from
    # the use of inactive to supplementary to be in line with the DTD.

    if (field$name[i] %in% inactive) usage <- "supplementary"
    # 090328 if (length(grep(field$name[i], inactive))) usage <- "inactive"

    if(!is.null(transformed))
    {
       if(is.na(transformed$fieldData[field$name[i],"origFieldName"]))
       {
        if(is.na(transformed$fieldData[field$name[i],"transform"]))
        {
         if(!(field$name[i] %in% namelist))
         {
          namelist <- c(namelist,field$name[i])
         }
        }
       } else
       {
          if(transformed$fieldData[field$name[i],"transform"] == "MapValues")
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
           for(j in 1:length(ofname))
           {
            if(!(ofname[j] %in% namelist))
            {
             namelist <- c(namelist,ofname[j])
            }
           }
           fname <- NA
          }
          else
          {
           ofname <- transformed$fieldData[field$name[i],"origFieldName"][[1]]
# following for loop not needed as multiple parents via mapvalues dealt with above
           for(j in 1:length(ofname))
           {
            fname <- ofname[j]
            while(!is.na(ofname[j]))
            {
             fname <- ofname[j]
	     xvalue <- transformed$fieldData[fname,"transform"]
             if(!is.na(xvalue) && xvalue == "MapValues")
             {
              parents <- transformed$fieldData[fname,"origFieldName"][[1]]
              for(j in 1:length(parents))
              {
               if(!(parents[j] %in% namelist))
               {
                namelist <- c(namelist,parents[j])
               }
              }
              fname <- NA
              break
             }
             ofname[j] <- transformed$fieldData[ofname[j],"origFieldName"][[1]]
            }
            if(!(fname %in% namelist))
            {
             namelist <- c(namelist,fname)
            }
           }
          }

       }
      nmbr <- 1
      for(ndf2 in 1:length(namelist))
      {
       if(!is.na(namelist[ndf2]))
       {
        mining.fields[[nmbr]] <- xmlNode("MiningField", attrs=c(name=namelist[ndf2],
                                                usageType=usage))
        nmbr <- nmbr + 1
       }
      }
    } else
    { 
      if(length(grep("as\\.factor\\(",field$name[i])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)
      else
        fName <- field$name[i]

      mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=fName,
                                    usageType=usage))
    }

   }
  }
  # add a predicted mining field if none exist
  if (targetExists == 0)
   mining.fields[[ii + 1]] <- xmlNode("MiningField",
                                              attrs=c(name=statusName,
                                               usageType="active"))
   mining.fields[[ii + 2]] <- xmlNode("MiningField",
                                              attrs=c(name=timeName,
                                               usageType="active"))
   mining.fields[[ii + 3]] <- xmlNode("MiningField",
                                              attrs=c(name="cumulativeHazard",
                                               usageType="predicted"))
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields


  return(mining.schema)

}

pmmlLocalTransformations <- function(field, transforms=NULL, LTelement=NULL)
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
      if (is.null(optype))
        output.fields[[1]] <- xmlNode("OutputField",
                                      attrs=c(name=gsub(" ","",paste("Predicted_",target)),
                                        feature="predictedValue"))
      else
        output.fields[[1]] <- xmlNode("OutputField",
                                      attrs=c(name=gsub(" ","",paste("Predicted_",target)),
                                        optype=optype,
                                        dataType=ifelse(optype=="continuous",
                                          "double", "string"),
                                        feature="predictedValue"))

     if(field$name[i] %in% names(field$levels))
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

########################################################################
# Begin supporting transforms.

.gen.transforms <- function(transforms) 
{
  return(NULL)
}
