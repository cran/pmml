# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Handle glm models.
#
# Time-stamp: <2012-12-04 05:35:09 Graham Williams>
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
# Linear Model PMML exporter
#
# Implemented: 070528 rguha@indiana.edu based on Graham's template for
# handling rpart trees.
#
# Modified: 080201 by Zementis, Inc. (info@zementis.com) to add the
# capability to export binary logistic regression models using glm.
#
# Modified: 090103 by Graham Williams to add transforms framework.

pmml.glm <- function(model,
                    model.name="General_Regression_Model",
                    app.name="Rattle/PMML",
                    description="Generalized Linear Regression Model",
                    copyright=NULL,
                    transforms=NULL,
                    ...)
{
  if (! inherits(model, "glm")) stop("Not a legitimate lm object")
  require(XML, quietly=TRUE)

  # Collect the required information.

  # For a regression, all variables will have been used except those
  # with a NA coefficient indicating singularities. We mark
  # singularities as inactive shortly.

  terms <- attributes(model$terms)
  
  field <- NULL

  field$name <- names(terms$dataClasses)
  # 101009 Check for a "(weights)" data class and remove it. This
  # arises in the glm models when a Weight variable is used. Not sure
  # why the glm model records this here.
  weights <- which(field$name == "(weights)")
  if (length(weights)) field$name <- field$name[-weights]
  orig.names <- field$name

  field$class <- terms$dataClasses
  if (length(weights)) field$class <- field$class[-weights]
  orig.class <- field$class

  # 090103 Support transforms if available.

  if (.supportTransformExport(transforms))
  {
    field <- .unifyTransforms(field, transforms)
    transforms <- .activateDependTransforms(transforms)
  }
  number.of.fields <- length(field$name)

  target <- field$name[1]
  if (length(grep("^as.factor\\(", field$name[1])))
  {
    field$name[1] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[1])
    target <- field$name[1]
    names(field$class)[1] <- target
    levels(model$data[[target]]) <- levels(model$model[[1]]) 
  }

  # 090501 Identify those who are singularities. For numerics, this is
  # easy since the names are just the variable names. For categorics
  # this gets tricky because the names include the levels. So we need
  # to keep in inactive the actual variable name, if all coefficients
  # for that variable are NAs.
  
  inactive <- names(which(is.na(coef(model))))
  active <- names(which(!is.na(coef(model))))

  # 110113 For the following grep, with certain Japanes characters we
  # see the string including a "[" in some encoding and causes the
  # grep to fail. We can't do the usual Encoding<- "UTF-8" trick since
  # the characters already look like UTF-8. But using enc2utf8 works -
  # does it hurt doing it always, or just when we have Japanese? Needs
  # testing.

  field$name <- enc2utf8(field$name)
  
  # These are the actual variable names. 110113 Should be using grepl
  # rather than grep and then tmp>0!
  
  tmp <- sapply(sapply(field$name, grep, inactive), length)
  inactive.vars <- names(tmp[tmp>0])
  tmp <- sapply(sapply(field$name, grep, active), length)
  active.vars <- names(tmp[tmp>0])
  
  # Now remove any which have any non-NA levels. This final list is
  # passed on as the definitive list of nonactive variables

  inactive <- setdiff(inactive.vars, active.vars)

  ylevels <- FALSE
  numfac <- 0
  for (i in 1:number.of.fields)
  {
    # If target variable is binomial, get its categories
    # otherwise stop
    if (field$class[[field$name[i]]] == "factor")
    {
      numfac <- numfac + 1
      if (field$name[i] == target)
      {
        if(length(levels(model$data[[field$name[i]]])) != 2)
          stop("binomial family with more than two target categories is not
                currently supported by PMML")
        ylevels <- TRUE
        field$levels[[field$name[i]]] <- levels(model$data[[field$name[i]]])
      } else
      {
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]

        #Tridi 2/16/12: remove any 'as.factor' from field names
        if (length(grep("^as.factor\\(", field$name[i])))
        {
          field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
          names(field$class)[i] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$class)[i])
          names(field$levels)[numfac] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$levels)[numfac])
        }
      }
    }
  }

  # PMML

  pmml <- .pmmlRootNode("4.1")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, weights=weights, transformed=transforms))

# determine the distribution and link function to add. quasi distributions cannot be
#  listed. Certain link functions are not supported and an error must be thrown. Certain
# link function can be recast as a power link and must be constructed separately
  # PMML -> GeneralRegressionModel
  add <- FALSE
  addl <- FALSE
  addl2 <- FALSE
  if (model$call[[1]] == "glm"){
    model.type <- model$family$family
    model.link <- model$family$link
  }
  else
    model.type <- "unknown"

# Only binary categorical cases can be handled. For multinomial cases, glm assumes the first
#  category as one and all the rest together as one. The output then is the probability of the 
#  first category NOT be true. This case is not implemented.
  categ <- FALSE
  if(ylevels)
  {
   if(model.type == "binomial")
   {
     categ <- TRUE
     add <- TRUE
   }
  }

  if(model.type == "binomial"){
    add <- TRUE
  }
  if(model.type == "Gamma") {
    model.type <- "gamma"
    add <- TRUE
  }
  if(model.type == "inverse.gaussian") {
    model.type <- "igauss"
    add <- TRUE
  }
  if(model.type == "gaussian") {
    model.type <- "normal"
    add <- TRUE
  }
  if(model.type == "poisson") { 
    add <- TRUE
  } 

  if(model.link == "cloglog") {
    addl <- TRUE
  } else
   if(model.link == "identity") {
    addl <- TRUE
  } else
  if(model.link == "log") {
    addl <- TRUE
  } else
  if(model.link == "logit") {
    addl <- TRUE
  } else
  if(model.link == "probit") {
    addl <- TRUE
  } else
  if(model.link == "inverse") {
    addl <- TRUE
    addl2 <- TRUE
    d <- "-1"
  } else
  if(model.link == "sqrt") {
    addl <- TRUE
    addl2 <- TRUE
    d <- "0.5"
  } else {
    stop("link function currently not supported by PMML")
  }

  if(categ)
  {
     the.model <- xmlNode("GeneralRegressionModel",
                        attrs=c(modelName=model.name,
                          modelType="generalizedLinear",
                          functionName="classification",
                          algorithmName="glm",
                          distribution=model.type,
                          linkFunction=model.link))

  } else if(add && addl && addl2)
  {
    the.model <- xmlNode("GeneralRegressionModel",
                         attrs=c(modelName=model.name,
                           modelType="generalizedLinear",
                           functionName="regression",
                           algorithmName="glm",
                           distribution=model.type,
                           linkFunction="power",
                            linkParameter=d))
  } else if(add && addl && !addl2)
  {
    the.model <- xmlNode("GeneralRegressionModel",
                         attrs=c(modelName=model.name,
                           modelType="generalizedLinear",
                           functionName="regression",
                           algorithmName="glm",
                           distribution=model.type,
                           linkFunction=model.link))
  } else if(!add && addl && addl2)
  {
    the.model <- xmlNode("GeneralRegressionModel",
                         attrs=c(modelName=model.name,
                           modelType="generalizedLinear",
                           functionName="regression",
                           algorithmName="glm",
                           linkFunction="power",
                            linkParameter=d))
  } else if(!add && addl && !addl2)
  {
    the.model <- xmlNode("GeneralRegressionModel",
                         attrs=c(modelName=model.name,
                           modelType="generalizedLinear",
                           functionName="regression",
                           algorithmName="glm",
                           linkFunction=model.link))
  } else 
    stop("model type not supported")



  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, inactive, transforms))

  if(categ)
  {
    outn <- xmlNode("Output")
    pname <- gsub(" ","",paste("Probability_",levels(model$data[[field$name[1]]])[2]))
    outpn <- xmlNode("OutputField",attrs=c(name=pname,targetField=target,feature="probability",value=levels(model$data[[field$name[1]]])[2]))
    outpn2 <- xmlNode("OutputField",attrs=c(name=gsub(" ","",paste("Predicted_",target)),feature="predictedValue"))
    outn <- append.XMLNode(outn,outpn)
    outn <- append.XMLNode(outn,outpn2)
  } else {
    outn <- xmlNode("Output")
    out <- xmlNode("OutputField",attrs=c(name=gsub(" ","",paste("Predicted_",target)),feature="predictedValue"))
    outn <- append.XMLNode(outn, out) 
  }
  the.model <- append.XMLNode(the.model, outn)


  # PMML -> TreeModel -> LocalTransforms

  if (.supportTransformExport(transforms))
    the.model <- append.XMLNode(the.model, .gen.transforms(transforms))

  # test of Zementis xform functions
  if(!is.null(transforms))
  {
    the.model <- append.XMLNode(the.model, pmmlLocalTransformations(field, transforms))
  }

 plNode <- xmlNode("ParameterList")
 num <- 0
 for(i in 1:length(names(coefficients(model)))){
   pname <- paste("p",num)
   pname <- gsub(" ","",pname)
   num <- num + 1
   pnode <- xmlNode("Parameter",attrs=c(name=pname,label=names(coefficients(model))[i]))
   plNode <- append.XMLNode(plNode,pnode)
 }

 the.model <- append.XMLNode(the.model,plNode)

  flNode <- xmlNode("FactorList")
# start from 2 as 1 is the predicted field
  for(i in 2:number.of.fields)
  {
    if(field$class[i] == "factor")
    {
      pdNode <- xmlNode("Predictor",attrs=c(name=field$name[i]))
      flNode <- append.XMLNode(flNode,pdNode)
    } 
  } 

  the.model <- append.XMLNode(the.model,flNode)

  cvNode <- xmlNode("CovariateList")
  for(i in 2:number.of.fields)
  {
    if(field$class[i] == "numeric")
    {
      pdNode <- xmlNode("Predictor",attrs=c(name=field$name[i]))
      cvNode <- append.XMLNode(cvNode,pdNode)
    }
  }

  the.model <- append.XMLNode(the.model,cvNode)

  ppm <- xmlNode("PPMatrix")
  for(j in 1:length(model$coefficients)){
   if(length(grep("Intercept",names(coefficients(model))[j])) == 1){
   } else if(length(grep(".+:.+",names(coefficients(model))[j])) == 1){
# interactive terms
       for(k in 1:length(strsplit(names(coefficients(model))[j],":")[[1]])){
         for(f in 2:number.of.fields){
           if(field$class[[field$name[f]]] == "factor"){
             if(length(grep(field$name[f],strsplit(names(coefficients(model))[j],":")[[1]][k])) == 1){
  	       modfield <- gsub(field$name[f],"",strsplit(names(coefficients(model))[j],":")[[1]][k])
               ppcell <- xmlNode("PPCell",attrs=c(value=modfield,predictorName=field$name[f],
                                                             parameterName=gsub(" ","",paste("p",j-1))))
               ppm <- append.XMLNode(ppm,ppcell) 
             } 
           } else{
              if(length(grep(field$name[f],strsplit(names(coefficients(model))[j],":")[[1]][k])) == 1){
              ppcell <- xmlNode("PPCell",attrs=c(value="1",predictorName=field$name[f],
                                                             parameterName=gsub(" ","",paste("p",j-1))))
              ppm <- append.XMLNode(ppm,ppcell)
           }
         }
       }
     }
   } else {
# categorical terms
       for(f in 2:number.of.fields){
         if(field$class[[field$name[f]]] == "factor"){
           if(length(grep(field$name[f],names(coefficients(model))[j])) == 1){
            if (length(grep("^as.factor\\(", names(coefficients(model))[j]))==1)
            {
             modfield <- sub("^as.factor\\((.*)\\)", "\\1", names(coefficients(model))[j])
             modfield <- gsub(field$name[f],"",modfield)
            } else {
              modfield <- gsub(field$name[f],"",names(coefficients(model))[j]) 
            }
            ppcell <- xmlNode("PPCell",attrs=c(value=modfield,predictorName=field$name[f],
                                                          parameterName=gsub(" ","",paste("p",j-1))))
            ppm <- append.XMLNode(ppm,ppcell)
           }
         } else{
# numerical terms
             if(length(grep(field$name[f],names(coefficients(model))[j])) == 1){
             ppcell <- xmlNode("PPCell",attrs=c(value="1",predictorName=field$name[f],
                                                           parameterName=gsub(" ","",paste("p",j-1))))
             ppm <- append.XMLNode(ppm,ppcell)
         }
       }
     }
   }
  }
  the.model <- append.XMLNode(the.model,ppm)

  pmNode <- xmlNode("ParamMatrix")
  for(i in 1:length(model$coefficients))
  {
   if( !is.na(coefficients(model)[i]) )
   {
    if(categ)
    {
     pcNode <- xmlNode("PCell",attrs=c(targetCategory=field$levels[[target]][2],
                                         parameterName=gsub(" ","",paste("p",i-1)), df="1",
                                          beta=as.numeric(coefficients(model)[i])))
     pmNode <- append.XMLNode(pmNode,pcNode)
    } else
    {
     pcNode <- xmlNode("PCell",attrs=c(parameterName=gsub(" ","",paste("p",i-1)), df="1",
					beta=as.numeric(coefficients(model)[i])))
     pmNode <- append.XMLNode(pmNode,pcNode)
    }
   } else
   {
     stop("Model coefficients did not converge")
   }
  }
  the.model <- append.XMLNode(the.model,pmNode)

  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, the.model)
  
  return(pmml)
}
