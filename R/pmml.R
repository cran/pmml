# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2009-03-06 21:28:05 Graham Williams>
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
  
  VERSION <- "1.2.9" # Further fix a pmml.lm bug.
    # "1.2.8" # Fix a pmml.lm bug.
    # "1.2.7" # Export logistic classes
    # "1.2.6" # Support RMA transforms.
    # "1.2.5" # Include .TRANSFORM constants within pmml package.
    # "1.2.4" # Include collection of utility transform functions.
    # "1.2.3" # Bug fixes
    # "1.2.2" # Add test for transform support.
    # "1.2.1" # Streamline conditional handling of transforms.
    # "1.2.0" # Fix documentation and packaing and release to CRAN
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
      for (j in seq_along(field$levels[[field$name[i]]]))
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

########################################################################
# TRANSFORMS FRAMEWORK

# GLOBAL CONSTANTS

.TRANSFORMS.NORM.CONTINUOUS <- c("RRC_", "R01_", "RMD_", "RMA_")
.TRANSFORMS.IMPUTE <- paste(c("IZR", "IMN", "IMD", "IMO", "ICN"), "_", sep="")
.TRANSFORMS.APPLY <- c("RLG_")
.TRANSFORMS.BIN <- c("BQ_", "BK_", "BE_")
.TRANSFORMS <- c(.TRANSFORMS.NORM.CONTINUOUS,
                 .TRANSFORMS.APPLY,
                 .TRANSFORMS.IMPUTE,
                 .TRANSFORMS.BIN)

supportTransformExport <- function(transforms=NULL)
{
  # 090208 Returns TRUE/FALSE. The default is that we don't have
  # Rattle code to generate the transforms in the pmml - i.e.,
  # pmml.transforms is not defined. A package which does will define a
  # pmml.transforms.
  
  return(exists("pmml.transforms") &&
         # length(getAnywhere("pmml.transforms")$objs) > 0 &&
         ! is.null(transforms))
}

unifyTransforms <- function(field, transforms)
{
  # 090102 Clean up the list of variables in field based on the known
  # transforms. The variable transforms is a list of strings,
  # generally of the form "A_B_C_D_E", at least for the transforms I
  # am working with at present (see .TRANSFORMS). A is the type of
  # transform, and D and E are the two (more or less, dependingon the
  # type, A) parameters of the transform. We make sure "B_C" (which
  # might have more than a single underscore) is included in the list
  # of vars and that "A_B_C_D_E" is not included in the list.

  # 090111 We also make sure that the field class is reset
  # appropriately, in special cases. So a binning transform, which is
  # now a categoric, was originally a numeric! Change it back.

  for (i in transforms)
  {

    tr.type <- transformToCode(i)
    ibase <- transformToDerived(i)

    # 090102 The name ibase should be in the list of vars, where we
    # replace it with the the same name, but remove the first
    # component, to give B_C.

    if (ibase %in% field$name)
    {
      index <- which(ibase == field$name)
      
      # 081229 I should probably be testing if index is NULL

      new.var <- sub("^([^_]*)_", "", field$name[index])

      # 090111 For the binning operations, be sure to change the class
      # from categoric back to numeric.

      if (tr.type %in% .TRANSFORMS.BIN)
        field$class[index] <- "numeric"

      # 090102 If the new var is already in the input variables, then
      # simply remove the entry naming the transformed var, otherwise
      # replace the entry naming the transformed var with the new var.
      
      if (new.var %in% field$name)
      {
        field$name <- field$name[-index]
        field$class <- field$class[-index]
      }
      else
        field$name[index] <- new.var
    }
  }

  # 090102 Reset the field$class names to correspond to the new
  # variables.

  names(field$class) <- field$name
  
  return(field)
}

isTransformVar <- function(var.name)
{
  return(transformToCode(var.name) %in% .TRANSFORMS)
}

transformToCode <- function(var)
{
  code <- sub("^([^_]*_).*$", "\\1", var)
  if (sprintf("%s_", substr(code, 1, 2)) %in% .TRANSFORMS.BIN)
    code <- sprintf("%s_", substr(code, 1, 2))
      
  return(code)
}

transformToDerived <- function(var)
{
  if (transformToCode(var) %in% .TRANSFORMS.APPLY)
    # No args
    return(var) 
  else if (transformToCode(var) %in% .TRANSFORMS.IMPUTE)
    # One arg
    return(sub("^([^_]*_)(.*)(_[^_]*)$", "\\1\\2", var))
  else if (transformToCode(var) %in% .TRANSFORMS.NORM.CONTINUOUS)
    # two args
    return(sub("^([^_]*_)(.*)(_[^_]*){2}$", "\\1\\2", var))
  else if (transformToCode(var) %in% .TRANSFORMS.BIN)
  {
    nparms <- as.integer(sub("^..([^_]*)_.*$", "\\1", var)) + 1
    return(sub(sprintf("^([^_]*_)(.*)(_[^_]*){%s}$", nparms), "\\1\\2", var))
  }
  else
    return(NULL)
}

transformToOriginal <- function(var)
{
  return(sub("^[^_]*_", "", transformToDerived(var)))
}

transformToParms <- function(var)
{
  if (transformToCode(var) %in% .TRANSFORMS.BIN)
  {
    nparms <- as.integer(sub("^..([^_]*)_.*$", "\\1", var)) + 1
    return(unlist(strsplit(
           sub("^ ", "",
           gsub("_", " ",
           sub(sprintf("^([^_]*_)(.*)((_[^_]*){%s})$",
                       nparms), "\\3", var))), " ")))
  }
}

