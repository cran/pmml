### PMML: Predictive Modelling Markup Language
##

markup <- function(x)
  gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", x)))

pmml.rules <- function(model,
                       model.name="arules_Model",
                       app.name="Rattle/PMML",
                       description="arules association rules model",
                       copyright=NULL, ...)
{
  
  #if (!inherits(model, "rules")) stop("Not a legitimate arules rules object")
  #require(XML, quietly=TRUE)
  #require(arules, quietly=TRUE)

  
  ## PMML
  pmml <- pmmlRootNode()

  ## PMML -> Header
  if (is.null(copyright)) copyright <- ""
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

  ## association rule model
  quality <- quality(model)
  is <- c(lhs(model),rhs(model))

  ## fixme: why does the S4 dispatch not work?
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
  il <- markup(itemLabels(model))
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
  if (is.null(copyright)) copyright <- ""
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
