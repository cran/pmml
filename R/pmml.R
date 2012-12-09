# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2012-12-08 20:22:11 Graham Williams>
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
  
  VERSION <- "1.2.32"

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
                                             version=VERSION)))

  # Header -> Timestamp
						   
  header <- append.XMLNode(header,
                           xmlNode("Timestamp", sprintf("%s", Sys.time())))

  return(header)
}

.pmmlDataDictionary <- function(field, dataset=NULL, weights=NULL)
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

    # DataDictionary -> DataField -> Interval

    if (optype == "continuous" && ! is.null(dataset))
    {
      interval <-  xmlNode("Interval",
                           attrs=c(closure="closedClosed",
                             leftMargin=min(dataset[[field$name[i]]],
                               na.rm=TRUE), # 091025 Handle missing values
                             rightMargin=max(dataset[[field$name[i]]],
                               na.rm=TRUE))) # 091025 Handle missing values
      data.fields[[i]] <- append.XMLNode(data.fields[[i]], interval)
    }
    
    # DataDictionary -> DataField -> Value

    if (optype == "categorical")
      for (j in seq_along(field$levels[[field$name[i]]]))
        data.fields[[i]][[j]] <- xmlNode("Value",
                                         attrs=c(value=
                                           .markupSpecials(field$levels[[field$name[i]]][j])))
  }

  if (! is.null(weights) && length(weights))
    data.dictionary <-append.XMLNode(data.dictionary, xmlNode("Extension",
                                                              attrs=c(name="Weights",
                                                                value=weights,
                                                                extender="Rattle")))
  
  data.dictionary <- append.XMLNode(data.dictionary, data.fields)

  return(data.dictionary)
}

.pmmlDataDictionarySurv <- function(field, timeName, dataset=NULL, weights=NULL)
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
  # DataDictionary

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=number.of.fields+1))

  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    if(length(grep(":",field$name[i])) == 1){
    } else {
    ii<-ii+1 
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
     if(length(grep("as\\.factor\\(",field$name[ii])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[ii], perl=TRUE)
     else
        fName <- field$name[ii]
     data.fields[[ii]] <- xmlNode("DataField", attrs=c(name=fName,
                                                optype=optype,
                                                dataType=datype))

    # DataDictionary -> DataField -> Interval
    if (optype == "continuous" && ! is.null(dataset))
    {
      interval <-  xmlNode("Interval",
                           attrs=c(closure="closedClosed",
                             leftMargin=min(dataset[[field$name[i]]],
                               na.rm=TRUE), # 091025 Handle missing values
                             rightMargin=max(dataset[[field$name[i]]],
                               na.rm=TRUE))) # 091025 Handle missing values
      data.fields[[ii]] <- append.XMLNode(data.fields[[ii]], interval)
    }

    # DataDictionary -> DataField -> Value

    if (optype == "categorical")
      for (j in seq_along(field$levels[[field$name[i]]]))
        data.fields[[ii]][[j]] <- xmlNode("Value",
                                         attrs=c(value=
                                           .markupSpecials(field$levels[[field$name[i]]][j])))
  }
  }

  if (! is.null(weights) && length(weights))
    data.dictionary <-append.XMLNode(data.dictionary, xmlNode("Extension",
                                                              attrs=c(name="Weights",
                                                                value=weights,
                                                                extender="Rattle")))

  data.fields[[ii+1]] <- xmlNode("DataField", attrs=c(name="predictedField",
                                                optype="continuous",dataType="double"))
  data.fields[[ii+2]] <- xmlNode("DataField", attrs=c(name=timeName,
                                                optype="continuous",dataType="double"))

  data.dictionary <- append.XMLNode(data.dictionary, data.fields)


  return(data.dictionary)
}


.pmmlMiningSchema <- function(field, target=NULL, inactive=NULL)
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

  for (i in 1:number.of.fields)
  {
    if (is.null(target))
      usage <- "active"
    else
      usage <- ifelse(field$name[i] == target, "predicted", "active")

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
      
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field$name[i],
                                    usageType=usage))
  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}

.pmmlMiningSchemaRF <- function(field, target=NULL, inactive=NULL)
{
  # Generate the PMML for the MinimgSchema element.

  number.of.fields <- length(field$name)
  mining.fields <- list()

  for (i in 1:number.of.fields)
  {
    if (is.null(target))
      usage <- "active"
    else
      usage <- ifelse(field$name[i] == target, "predicted", "active")

    if (field$name[i] %in% inactive) usage <- "supplementary"

    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field$name[i],
                                    usageType=usage, invalidValueTreatment="asIs"))
  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}

.pmmlMiningSchemaSurv <- function(field, timeName, target=NULL, inactive=NULL)
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
    if(length(grep("as\\.factor\\(",field$name[i])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)
    else
        fName <- field$name[i]
    mining.fields[[ii]] <- xmlNode("MiningField",
                                  attrs=c(name=fName,
                                    usageType=usage))
  }
  }
  # add a predicted mining field if none exist
  if (targetExists == 0)
   mining.fields[[ii + 1]] <- xmlNode("MiningField",
                                              attrs=c(name="predictedField",
                                               usageType="predicted"))
   mining.fields[[ii + 2]] <- xmlNode("MiningField",
                                              attrs=c(name=timeName,
                                               usageType="active"))

  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
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
  
  output$children <- output.fields
  return(output)
}

########################################################################
# Begin supporting transforms.

.gen.transforms <- function(transforms) 
{
  return(NULL)
}
