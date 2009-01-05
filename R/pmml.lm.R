# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2009-01-05 10:34:19 Graham Williams>
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
# LM
#
# Implemented: 070528 rguha@indiana.edu based on Graham's template for
# handling rpart trees.
#
# Modified: 080201 by Zementis, Inc. (info@zementis.com) to add the
# capability to export binary logistic regression models using glm.
#
# Modified: 090103 by Graham Williams to add transforms framework.

pmml.lm <- function(model,
                    model.name="Linear_Regression_Model",
                    app.name="Rattle/PMML",
                    description="Linear Regression Model",
                    copyright=NULL,
                    transforms=NULL,
                    ...)
{
  if (! inherits(model, "lm")) stop("Not a legitimate lm object")
  require(XML, quietly=TRUE)

  # Collect the required information.

  # For a regression, all variables will have been used except those
  # with a NA coefficient indicating singularities. We mark
  # singularities as inactive.

  terms <- attributes(model$terms)
  
  field <- NULL
  field$name <- names(terms$dataClasses)
  orig.names <- field$name
  field$class <- terms$dataClasses
  orig.class <- field$class

  # 090103 gjw Support transforms if available.
  
  if (exists("pmml.transforms") && ! is.null(transforms))
  {
    field$name <- unifyTransforms(field$name, transforms)

    # 090102 Reset the field$class names to correspond to the new
    # variables.
    
    names(field$class) <- field$name
  }

  number.of.fields <- length(field$name)
  
  target <- field$name[1]
  
  inactive <- names(which(is.na(coef(model))))

  for (i in 1:number.of.fields)
  {
    # We don't need to bother with ylevels since lm doesn't do
    # factor predictions.
      
    if (field$class[[field$name[i]]] == "factor")
      # 081004 gjw Test if the data is available in the model, as it
      # would be for a glm (but not an lm), since if the target
      # variable is categoric then the levels are not recorded in
      # xlevels for the target variable, so we will need to get the
      # levels from the data itself.
      if (is.null(model$data))
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
      else
        field$levels[[field$name[i]]] <- levels(model$data[[field$name[i]]])
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
    the.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          algorithmName="glm",
                          normalizationMethod="softmax",
                          targetFieldName=target)) 
  }
  else if (as.character(model$call[[3]])[1] == "poisson")
  {
    the.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          algorithmName="glm",
                          normalizationMethod="exp",
                          targetFieldName=target)) 
  }
  else # The original code for linear regression models
  {
    the.model <- xmlNode("RegressionModel",
                        attrs=c(modelName=model.name,
                          functionName="regression",
                          algorithmName="least squares",
                          targetFieldName=target))
  }

  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, pmmlMiningSchema(field, target, inactive))

  # PMML -> TreeModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (exists("pmml.transforms") && ! is.null(transforms))
    the.model <- append.XMLNode(the.model, pmml.transforms(transforms))
  
  # PMML -> RegressionModel -> RegressionTable

  coeff <- coefficients(model)
  coeffnames <- names(coeff)
  
  # Added by Graham Williams so that code identifies a targetCategory for binary
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
  
  # 080620 gjw The PMML spec (at least the Zementis validator)
  # requires NumericPredictors first and then
  # CategoricalPreictors. Simplest approach is to loop twice!!
  # Hopefully, this is not a significant computational expense.

#  for (i in 1:length(field$name))
  for (i in 1:length(orig.names))
  {
#    name <- field$name[[i]]
    name <- orig.names[[i]]
    if (name == target) next
#    klass <- field$class[[name]]
    klass <- orig.class[[name]]
    if (klass == 'numeric')
    {
      predictorNode <- xmlNode("NumericPredictor",
                               attrs=c(name=name,
                                 exponent="1",
                                 coefficient=as.numeric(coeff[which(coeffnames==name)])))
      regTable <- append.XMLNode(regTable, predictorNode)
    }
  }
#  for (i in 1:length(field$name))
  for (i in 1:length(orig.names))
  {
#    name <- field$name[[i]]
    name <- orig.names[[i]]
    if (name == target) next
#    klass <- field$class[[name]]
    klass <- orig.class[[name]]
    if (klass == 'factor')
    {
      levs <- model$xlevels[[name]]
      # 081019 gjw Add in a zero coefficient for the base level. In
      # this way, we communicate through the PMML which level is the
      # base. Can be useful in then comparing with the full list of
      # levels available for this variable and determining levels that
      # are just missing from the training. Note that xlevels does not
      # include any levels that were not modelled (i.e., missing
      # levels from the training data). We do this by iterating over
      # all the modelled levels (levs, i.e., all values in xlevels)
      # instead of all but the first level (levs[-1], i.e., the base
      # level). When we have the first level, we simply note the
      # coefficient as 0.
      for (l in levs)
      {
        tmp <- paste(name, l, sep='')
        coefficient <- ifelse(l==levs[1], 0.00,
                              as.numeric(coeff[which(coeffnames == tmp)]))
        predictorNode <- xmlNode("CategoricalPredictor",
                                 attrs=c(name=name,
                                   value=l, coefficient=coefficient))
        regTable <- append.XMLNode(regTable, predictorNode)
      }
    }
  }
  
  the.model <- append.XMLNode(the.model, regTable)
  
  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, the.model)
  
  return(pmml)
}
