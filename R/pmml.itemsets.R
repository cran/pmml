pmml.itemsets <- function(model,
                       model.name="arules_Model",
                       app.name="Rattle/PMML",
                       description="arules frequent itemsets model",
                       copyright=NULL, ...)
{
  
  if (! inherits(model, "itemsets")) stop("Not a legitimate arules itemsets rules object")

  require(XML, quietly=TRUE)
  require(arules, quietly=TRUE)
  
  ## PMML
  pmml <- .pmmlRootNode("4.0")

  ## PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))

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
  il <- .markupSpecials(itemLabels(model))
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
