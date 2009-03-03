# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2009-02-16 06:34:53 Graham Williams>
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
# multinom PMML exporter
#
# 081103 Decide to target Regression (rather than GeneralRegression)
# to reuse as much as possible. This means it will have multiple
# RegressionTables.

# This might just become a variation of the lm code in which case
# there could be a possibility of merging it into that. But there are
# some sublte differences.

pmml.multinom <- function(model,
                          model.name="Multinom_Model",
                          app.name="Rattle/PMML",
                          description="Multinom nnet model",
                          copyright=NULL,
                          transforms=NULL,
                          ...)
{
  if (! inherits(model, "multinom")) stop("Not a legitimate multinom object")
  require(XML, quietly=TRUE)
  require(nnet, quietly=TRUE)

  # Collect the required variables information.

  terms <- attributes(model$terms)
  field <- NULL
  field$name <- names(terms$dataClasses)
  orig.names <- field$name
  number.of.fields <- length(field$name)
  field$class <- terms$dataClasses
  orig.class <- field$class

   # 090216 Support transforms if available.
  
  if (supportTransformExport(transforms))
    field <- unifyTransforms(field, transforms)
  number.of.fields <- length(field$name)
 
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    # We don't need to bother with ylevels since lm doesn't do
    # factor predictions.
      
    if (field$class[[field$name[i]]] == "factor")
    {
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- model$lab
      else
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
    }
  }

  # PMML

  pmml <- pmmlRootNode("3.2")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  # PMML -> DataDictionary
  
  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> RegressionModel

  # as.character(model$call[[1]]) == "multinom"
  
  the.model <- xmlNode("RegressionModel",
                       attrs=c(modelName=model.name,
                         functionName="classification",
                         algorithmName=as.character(model$call[[1]]),
                         normalizationMethod="softmax",
                         targetFieldName=target)) 

  # PMML -> RegressionModel -> MiningSchema
  
  the.model <- append.XMLNode(the.model, pmmlMiningSchema(field, target))

  # PMML -> TreeModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (supportTransformExport(transforms))
    the.model <- append.XMLNode(the.model, pmml.transforms(transforms))
  
  # PMML -> RegressionModel -> RegressionTable
  
  coeff <- coefficients(model)
  coeffnames <- colnames(coeff)
  targetnames <- rownames(coeff)

  for (k in 1:nrow(coeff))
  {
    reg.table <- xmlNode("RegressionTable",
                         attrs=c(intercept=as.numeric(coeff[k, 1]),
                           targetCategory=targetnames[k]))

  for (i in 1:length(orig.names))
    {
      name <- orig.names[[i]]

      if (name == target) next
      klass <- orig.class[[name]]
      if (klass == 'numeric')
      {
        predictor.node <-
          xmlNode("NumericPredictor",
                  attrs=c(name=name,
                    exponent="1",
                    coefficient=as.numeric(coeff[k, which(coeffnames==name)])))
        reg.table <- append.XMLNode(reg.table, predictor.node)
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
        for (l in levs)
        {
          tmp <- paste(name, l, sep='')
          coefficient <- ifelse(l==levs[1], 0.00,
                                as.numeric(coeff[k, which(coeffnames == tmp)]))
          predictor.node <- xmlNode("CategoricalPredictor",
                                   attrs=c(name=name,
                                     value=l, coefficient=coefficient))
          reg.table <- append.XMLNode(reg.table, predictor.node)
        }
      }
    }
    the.model <- append.XMLNode(the.model, reg.table)
  }

  # Add a regression table for the base case.

  basename <- setdiff(model$lab, targetnames)
  the.model <- append.XMLNode(the.model,
                              xmlNode("RegressionTable",
                                      attrs=c(intercept="0.0",
                                        targetCategory=basename)))

  # Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, the.model)
  
  return(pmml)
}
