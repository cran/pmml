\name{AddAttributes}
\alias{AddAttributes}
\title{adds attribute values to an existing element in a given PMML file}
\description{

  This helper function allows one to add attributes to an arbitrary xml 
  element. This is an experimental function designed to be more general 
  than the 'addMSAttributes' or 'addDDAttributes' functions.

}
\usage{
AddAttributes(xmlmodel=NULL, xpath=NULL, attributes=NULL, 
              namespace="4_2",...)
}
\arguments{
  
  \item{xmlmodel}{the PMML model in a XML node format. If the model is a text file, it should be converted
		to an XML node, for example, using the fileToXMLNode function.}
  \item{xpath}{the XPath to the element to which the attributes are to be added.}
  \item{attributes}{the attributes to be added to the data fields. The user should make sure that the 
		attributes being added are allowed in the PMML schema.}
  \item{namespace}{the namespace of the PMML model. This is frequently also the PMML version the model is 
	represented as.}
  \item{\dots}{further arguments passed to or from other methods.}

}
\details{
  The attribute information can be provided as a vector. Multiple attribute
  names and values can be passes as vector elements to enable inserting 
  multiple attributes. However, this function overwrites any pre-existing 
  attribute values, so it must be used with care. This behavior is by design 
  as this feature is meant to help an user add new defined attribute values 
  at different times. The XPath has to include the namespace as shown in the 
  examples. 

}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package. This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\author{\email{tridivesh.jena@zementis.com}}

\examples{

 # make a sample model
 library(pmml)
 model0 <- lm(Sepal.Length~., data=iris[,-5])
 model <- pmml(model0)

 # The resulting PMML:
 # <PMML version="4.2" ... xmlns="http://www.dmg.org/PMML-4_2">
 # <Header ... description="Linear Regression Model"/>
 # <DataDictionary numberOfFields="4">
 # .
 # .
 # </DataDictionary>
 # <RegressionModel modelName="Linear_Regression_Model" 
 #     functionName="regression" 
 #     algorithmName="least squares">
 #  <MiningSchema>
 #  . 
 #  .
 #  </MiningSchema>
 # .
 # .
 #  <RegressionTable intercept="1.85599749291755">
 #   <NumericPredictor name="Sepal.Width" exponent="1" 
 #    coefficient="0.650837159313218"/>
 #   <NumericPredictor name="Petal.Length" exponent="1" 
 #    coefficient="0.709131959136729"/>
 #   <NumericPredictor name="Petal.Width" exponent="1" 
 #    coefficient="-0.556482660167024"/>
 #  </RegressionTable>
 # </RegressionModel>
 # </PMML>

 # Add arbitrary attributes to the 1st 'NumericPredictor' element. The 
 # attributes are for demostration only, they are not allowed under 
 # the PMML schema. The command assumes the default namespace.
 AddAttributes(model, "/p:PMML/descendant::p:NumericPredictor[1]", 
              attributes=c(a=1,b="b"))

 # add attributes to the NumericPredictor element which has 
 # 'Petal.Length' as the 'name' attribute.
 AddAttributes(model, 
    "/p:PMML/descendant::p:NumericPredictor[@name='Petal.Length']", 
    attributes=c(a=1,b="b"))

 # 3 NumericElements exist which have '1' as the 'exponent' attribute.
 # Add new attributes to the 3rd one.
 AddAttributes(model, 
    "/p:PMML/descendant::p:NumericPredictor[@exponent='1'][3]", 
    attributes=c(a=1,b="b"))

 # add attributes to the 1st element whose 'name' attribute contains 
 # 'Length'.
 AddAttributes(model,
    "/p:PMML/descendant::p:NumericPredictor[contains(@name,'Length')]", 
    attributes=c(a=1,b="b"))

}
\keyword{interface}


