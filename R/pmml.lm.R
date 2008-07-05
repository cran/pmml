# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-06-21 14:49:52 Graham Williams>
#
# Copyright (c) 2008 Togaware Pty Ltd
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
# LM
#
# Author: rguha@indiana.edu
# Date: 28 May 2007
#
# Modified: 01 Feb 2008 by Zementis, Inc. (info@zementis.com) to add
# the capability to export binary logistic regression models using
# glm.

pmml.lm <- function(model,
                    model.name="Regression_model",
                    app.name="Rattle/PMML",
                    description="Regression Model",
                    copyright=NULL, ...)
{
  if (! inherits(model, "lm")) stop("Not a legitimate lm object")

  require(XML, quietly=TRUE)

  # Collect the required variables information. For a regression, all
  # variables will have been used.

  terms <- attributes(model$terms)
  field <- NULL
  field$name <- names(terms$dataClasses)
  number.of.fields <- length(field$name)
  field$class <- terms$dataClasses
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    # We don't need to bother with ylevels since lm doesn't do
    # factor predictions.
      
    if (field$class[[field$name[i]]] == "factor")
      field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
  }

  # PMML

  pmml <- pmmlRootNode("3.2")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> RegressionModel

  # Added by Zementis so that code can also export binary logistic
  # regression glm models built with binomial(logit)

  if (as.character(model$call[[3]])[1] == "binomial")
  {
    lm.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          normalizationMethod="softmax",
                          targetFieldName=target)) 
  }
  else # The original code for linear regression models
  {
    lm.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          algorithmName="least squares",
                          targetFieldName=target))
  }

  # PMML -> RegressionModel -> MiningSchema

  lm.model <- append.XMLNode(lm.model, pmmlMiningSchema(field, target))

  # PMML -> RegressionModel -> RegressionTable

  coeff <- coefficients(model)
  coeffnames <- names(coeff)
  
  # Added by Graham Williams so that code identifies a targetCategroy for binary
  # logistic regression glm models built with binomial(logit).

  if (as.character(model$call[[3]])[1] == "binomial")
  {
    # 080620 TODO The YES here should be the actual class value that
    # is being predicted.
    
    regTable <- xmlNode("RegressionTable",
                        attrs=c(targetCategory="YES",
                          intercept=as.numeric(coeff[1])))
  }
  else
  {
    regTable <- xmlNode("RegressionTable",
                        attrs=c(intercept=as.numeric(coeff[1])))
  }
  
  # 080620 GJW The PMML spec (at least the Zementis validator)
  # requires NumericPredictors first and then
  # CategoricalPreictors. Simplest approach is to loop twice!!
  # Hopefully, this is not a computational expense.

  for (i in 1:length(field$name))
  {
    name <- field$name[[i]]
    if (name == target) next
    klass <- field$class[[name]]
    if (klass == 'numeric')
    {
      predictorNode <- xmlNode("NumericPredictor",
                               attrs=c(name=name,
                                 exponent="1",
                                 coefficient=as.numeric(coeff[which(coeffnames==name)])))
      regTable <- append.XMLNode(regTable, predictorNode)
    }
  }
  for (i in 1:length(field$name))
  {
    name <- field$name[[i]]
    if (name == target) next
    klass <- field$class[[name]]
    if (klass == 'factor')
    {
      levs <- model$xlevels[[name]]
      for (l in levs[-1])
      {
        tmp <- paste(name, l, sep='')
        predictorNode <- xmlNode("CategoricalPredictor",
                                 attrs=c(name=name,
                                   value=l,
                                   coefficient=as.numeric(
                                     coeff[which(coeffnames == tmp) ])))
        regTable <- append.XMLNode(regTable, predictorNode)
      }
    }
  }
  
  lm.model <- append.XMLNode(lm.model, regTable)
  
  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, lm.model)
  
  return(pmml)
}

