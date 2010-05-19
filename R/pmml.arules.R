# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2010-03-25 06:54:14 Graham Williams>
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
#
# ARules Module
#
# Implements a PMML exporter for arules objects (Association Rules)
#
# Author: 
# E-mail: 
# Date: 
#
# Conform to PMML 3.2 Graham Williams 080622

pmml.rules <- function(model,
                       model.name="arules_Model",
                       app.name="Rattle/PMML",
                       description="arules association rules model",
                       copyright=NULL, ...)
{
  
  #if (!inherits(model, "rules")) stop("Not a legitimate arules rules object")
  #require(XML, quietly=TRUE)
  #require(arules, quietly=TRUE)
  
  # PMML

  pmml <- pmmlRootNode("3.2")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  data.dictionary <- xmlNode("DataDictionary", attrs=c(numberOfFields = 2L))
  data.dictionary <- append.xmlNode(data.dictionary, list(
      xmlNode("DataField", attrs=c(name="transaction",
              optype = "categorical", dataType = "string")),
      xmlNode("DataField", attrs=c(name="item",
              optype = "categorical", dataType = "string"))
  ))

  pmml <- append.XMLNode(pmml, data.dictionary)

  # Association rule model

  quality <- quality(model)
  is <- c(lhs(model),rhs(model))

  # fixme: why does the S4 dispatch not work?

  is.unique <- getMethod("unique", "itemMatrix")(is)

  association.model <- xmlNode("AssociationModel", 
      attrs=c(functionName="associationRules",
          ## fixme: this is currently a hack
          numberOfTransactions=attr(quality(model), "size.data"), 
          numberOfItems=length(itemLabels(model)),
          minimumSupport=min(quality$support),     
          minimumConfidence=min(quality$confidence),
          numberOfItemsets=length(is.unique),     
          numberOfRules=length(model)))

  ## mining schema
  mining.schema <- xmlNode("MiningSchema")
  mining.schema <- append.xmlNode(mining.schema, list(
      xmlNode("MiningField", attrs = c(name = "transaction")),
      xmlNode("MiningField", attrs = c(name = "item"))
  ))
  
  association.model <- append.xmlNode(association.model, mining.schema)

  ## items
  items <- list()
  il <- markupSpecials(itemLabels(model))
  for (i in 1:length(il)) 
  items[[i]] <- xmlNode("Item", attrs = list(id = i, value = il[i]))

  association.model <- append.xmlNode(association.model, items)

  ## itemsets
  itemsets <- list()
  sizes <- size(is.unique)
  isl <- LIST(is.unique, decode=FALSE)
  for (i in 1:length(isl)){
      itemsets[[i]] <- xmlNode("Itemset", attrs = list(id = i, 
              numberOfItems = sizes[i])) 
      ## we have no support: support 
      
      items <- list()
      if(sizes[i] >0)
      for (j in 1:sizes[i])  
      items[[j]] <- xmlNode("ItemRef", attrs = list(itemRef = isl[[i]][j]))

      itemsets[[i]] <- append.xmlNode(itemsets[[i]], items)  
  }
  
  association.model <- append.xmlNode(association.model, itemsets)
  
  ## rules
  ## fixme: why does the S4 dispatch not work?
  mlhs <- getMethod("match",c("itemMatrix","itemMatrix"))(lhs(model), is.unique)
  mrhs <- getMethod("match",c("itemMatrix","itemMatrix"))(rhs(model), is.unique)
  rules <- list()

  for (i in 1:length(model)) {
    rules[[i]] <- xmlNode("AssociationRule", attrs = list(
            support = quality$support[i], confidence = quality$confidence[i],
            lift = quality$lift[i],
        antecedent=mlhs[i], consequent=mrhs[i]))
  }
  
  association.model <- append.xmlNode(association.model, rules)

  pmml <- append.XMLNode(pmml, association.model)

  return(pmml)
}

pmml.itemsets <- function(model,
                       model.name="arules_Model",
                       app.name="Rattle/PMML",
                       description="arules frequent itemsets model",
                       copyright=NULL, ...)
{
  
  if (! inherits(model, "itemsets")) stop("Not a legitimate arules rules object")

  #require(XML, quietly=TRUE)
  require(arules, quietly=TRUE)

  
  ## PMML
  pmml <- pmmlRootNode()

  ## PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  ## PMML -> DataDictionary
  data.dictionary <- xmlNode("DataDictionary", attrs=c(numberOfFields = 2L))
  data.dictionary <- append.xmlNode(data.dictionary, list(
      xmlNode("DataField", attrs=c(name="transaction",
              optype = "categorical", dataType = "string")),
      xmlNode("DataField", attrs=c(name="item",
              optype = "categorical", dataType = "string"))
  ))

  pmml <- append.XMLNode(pmml, data.dictionary)


  ## model
  quality <- quality(model)
  is <- items(model)

  association.model <- xmlNode("AssociationModel", 
      attrs=c(functionName="associationRules",
          ## fixme: this is currently a hack
          numberOfTransactions=attr(quality(model), "size.data"), 
          numberOfItems=length(itemLabels(model)),
          minimumSupport=min(quality$support),     
          minimumConfidence=0L,
          numberOfItemsets=length(is),     
          numberOfRules=0L))

  ## mining schema
  mining.schema <- xmlNode("MiningSchema")
  mining.schema <- append.xmlNode(mining.schema, list(
      xmlNode("MiningField", attrs = c(name = "transaction")),
      xmlNode("MiningField", attrs = c(name = "item"))
  ))
  
  association.model <- append.xmlNode(association.model, mining.schema)

  ## items
  items <- list()
  il <- markup(itemLabels(model))
  for (i in 1:length(il)) 
  items[[i]] <- xmlNode("Item", attrs = list(id = i, value = il[i]))

  association.model <- append.xmlNode(association.model, items)

  ## itemsets
  itemsets <- list()
  sizes <- size(is)
  isl <- LIST(is, decode=FALSE)
  for (i in 1:length(isl)){
      itemsets[[i]] <- xmlNode("Itemset", attrs = list(id = i, 
              numberOfItems = sizes[i], support=quality$support[i])) 
      
      items <- list()
      if(sizes[i] >0)
      for (j in 1:sizes[i])  
      items[[j]] <- xmlNode("ItemRef", attrs = list(itemRef = isl[[i]][j]))

      itemsets[[i]] <- append.xmlNode(itemsets[[i]], items)  
  }
  
  association.model <- append.xmlNode(association.model, itemsets)
  
  ## no rules

  pmml <- append.XMLNode(pmml, association.model)

  return(pmml)
}
