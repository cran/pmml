# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Handle lm and glm models.
#
# Time-stamp: <2009-03-07 09:42:48 Graham Williams>
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

  # 090103 Support transforms if available.
  
  if (supportTransformExport(transforms))
    field <- unifyTransforms(field, transforms)
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
  # regression glm models built with binomial(logit). 090303 gjw This
  # looks dangerous, assuming the third argument is the model
  # type. For now, go with it, but set a default model type in case
  # the call has less than two arguments. A general lm model has data
  # as the third part of the call, thus we need to accept that as a
  # genuine model and not an unknown model type! For now, default
  # to generating lm PMML.

  if (model$call[[1]] == "lm")
    model.type <- "lm"
  else if (model$call[[1]] == "glm" && length(model$call) > 2)
    model.type <- as.character(model$call[[3]])[1]
  else
    model.type <- "unknown"
  
  if (model.type == "binomial")
  {
    the.model <- xmlNode("RegressionModel",
                         attrs=c(modelName=model.name,
                           functionName="regression",
                           algorithmName="glm",
                           normalizationMethod="softmax",
                             targetFieldName=target)) 
  }
  else if (model.type == "poisson")
  {
    the.model <- xmlNode("RegressionModel",
                         attrs=c(modelName=model.name,
                           functionName="regression",
                             algorithmName="glm",
                           normalizationMethod="exp",
                           targetFieldName=target)) 
  }
  else if (model.type == "gaussian")
  {
    the.model <- xmlNode("RegressionModel",
                         attrs=c(modelName=model.name,
                           functionName="regression",
                           algorithmName="glm",
                           targetFieldName=target)) 
  }		
  else if (model.type == "lm")
  {
    # The original code for linear regression models.
    the.model <- xmlNode("RegressionModel",
                         attrs=c(modelName=model.name,
                           functionName="regression",
                           algorithmName="least squares",
                           targetFieldName=target))
  }
  else 
    stop("PMML.LM: Not a supported family object: ", model.type)

  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, pmmlMiningSchema(field, target, inactive))

  # PMML -> TreeModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (supportTransformExport(transforms))
    the.model <- append.XMLNode(the.model, pmml.transforms(transforms))
  
  # PMML -> RegressionModel -> RegressionTable

  coeff <- coefficients(model)
  coeffnames <- names(coeff)

  # 090306 Handle the case where the intercept is not in the
  # coefficients, and hence is 0?
  
  if (coeffnames[[1]] == "(Intercept)")
    intercept <- as.numeric(coeff[[1]])
  else
    intercept <- 0
  
  # Added by Graham Williams so that code identifies a targetCategory for binary
  # logistic regression glm models built with binomial(logit).

  if (model.type == "binomial")
  {
    # 090117 Identify the two possible values for the target variable,
    # and select the second as the target. Extend the PMML specs so I
    # can add the other value as well, since I need that when
    # generating C code to return a class rather than a probability.

    values <- sort(unique(model$data[[target]]))
    alternative.value <- as.character(values[1])
    target.value <- as.character(values[2])
    regTable <- xmlNode("RegressionTable",
                        attrs=c(targetCategory=target.value,
                          alternativeCategory=alternative.value,
                          intercept=intercept))
  }
  else
  {
    regTable <- xmlNode("RegressionTable",
                        attrs=c(intercept=intercept))
  }
  
  # 080620 gjw The PMML spec (at least the Zementis validator)
  # requires NumericPredictors first and then
  # CategoricalPredictors. Simplest approach is to loop twice!!
  # Hopefully, this is not a significant computational expense.

  for (i in 1:length(orig.names))
  {
    name <- orig.names[[i]]
    if (name == target) next
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

  for (i in 1:length(orig.names))
  {
    name <- orig.names[[i]]
    if (name == target) next
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
      # coefficient as 0. 090306 This was updated to remove the
      # assumption that the first level has a 0 coefficient. This is
      # not the case in simple lm models (e.g., exampe(lm);
      # pmml(lm.D90)).
      for (l in levs)
      {
        tmp <- paste(name, l, sep='')
        # 090306 Change this test from one that assumes a 0
        # coefficient for the first level, to one that has a 0
        # coefficient for any missing level.
        coefficient <- ifelse(!length(which(coeffnames == tmp)), 0.00,
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
