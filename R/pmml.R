# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2009-01-05 10:32:55 Graham Williams>
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
# MAIN PMML FUNCTION

pmml <- function(model,
                 model.name="Rattle_Model",
                 app.name="Rattle/PMML",
                 description=NULL,
                 copyright=NULL,
                 transforms=NULL,
                 ...)
  UseMethod("pmml")

########################################################################
# UTILITY FUNCTIONS

generateCopyright <- function()
{
  return(paste("Copyright (c)", format(Sys.time(), "%Y"), Sys.info()["user"]))
}

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

# Function pmmlRootNode

pmmlRootNode <- function(version)
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
  else
    node <- xmlNode("PMML",
                    attrs=c(version="3.2",
                      xmlns="http://www.dmg.org/PMML-3_2",
                      "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance", 
                      "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-3_2",
                        "http://www.dmg.org/v3-2/pmml-3-2.xsd")))
  return(node)
}

pmmlHeader <- function(description, copyright, app.name)
{
  # Header
  
  VERSION <- "1.2.0" # Fix documentation and packaing and release to CRAN
    # "1.1.20" # Bug - fix rpart var names with transforms
    # "1.1.19" # Tidyup and update ClusterField
    # "1.1.18" # Include pmml.hclust in NAMESPACE
    # "1.1.17" # Export hclust as kmeans.
    # "1.1.16" # export pmml.multinom
    # "1.1.15" # Handle multinomial model.
    # "1.1.14" # Handle singularities in lm/glm better.
    # "1.1.13" # Support export of poisson(log)
    # "1.1.12" # Tree Array have quoted values. 0 for base in regression
    # "1.1.11" # Bug fix for pmml.lm - continuing to fix below problem
    # "1.1.10" # Bug fix for pmml.lm with categorical logistic target
    # "1.1.9" # Update rpart/nnet/ksvm from Zementis + many improvements
    # "1.1.8" # Increase number of digits extracted for rpart tests.
    # "1.1.7" # Add arules.
    # "1.1.6"
    # "1.1.5" # Add pmml.nnet.
    # "1.1.4" # Add pmml.ksvm. Fix extensions. 
    # "1.1.3" # Fixes for new version of randomSurvivalForest.
    # "1.1.2" Expose pmml.lm in NAMESPACE - woops.
    # "1.1.1" Add pmml.lm

  if (is.null(copyright)) copyright <- generateCopyright()
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

pmmlMiningSchema <- function(field, target=NULL, inactive=NULL)
{
  # 081103 Add inactive to list those variables that should be marked
  # as inactive in the model. This was added so that singularities can
  # be identified as inactive for a linear model. It could also be
  # used to capture ignored variables, if they were to ever be
  # included in the variable list.
  
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
    # substring of TestAll, etc)
    
    # 081103 if (field$name[i] %in% inactive) usage <- "inactive"
    if (length(grep(field$name[i], inactive))) usage <- "inactive"
      
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field$name[i],
                                    usageType=usage))
  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}
