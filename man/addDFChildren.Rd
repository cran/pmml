\name{addDFChildren}
\alias{addDFChildren}
\title{adds 'Interval' and 'Value' child elements to a given DataField element in a given PMML file}
\description{

  The PMML format allows a DataField element to have 'Interval' and 'Value' child elements which although
  useful, may not always be present in a PMML model. This function allows one to take an existing
  PMML file and add these elements to the DataFields.  

}
\usage{
addDFChildren(xmlmodel=NULL,field=NULL,intervals=NULL,
              values=NULL,namespace="4_3",...)
}
\arguments{
  
  \item{xmlmodel}{the PMML model in a XML node format. If the model is a text file, it should be converted
		to an XML node, for example, using the fileToXMLNode function.}
  \item{field}{The field to which the attributes are to be added. This is used when the attributes are a
                vector of name-value pairs, intended for this one field.}
  \item{intervals}{The 'Interval' elements given as a list}
  \item{values}{The 'Value' elements given as a list.}
  \item{namespace}{the namespace of the PMML model. This is frequently also the PMML version the model is 
	represented as.}
  \item{\dots}{further arguments passed to or from other methods.}

}
\details{
  The 'Interval' elements or the 'Value' elements can be typed in, but more conviniently created by using the 
  helper functions 'makeIntervals' and 'MakeValues'. This function can then add these extra information to 
  the PMML.  
}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package. This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\author{\email{tridivesh.jena@softwareag.com}}

\examples{

 # make a sample model
 library(pmml)
 model0 <- lm(Sepal.Length~., data=iris[,-5])
 model <- pmml(model0)

 # Resulting model has data fields but with no 'Interval' or Value' 
 # elements. This object is already an xml node, not an external text
 # file; so there is no need to convert it to an xml node object.

 # add an 'Interval' element node by typing it in 
 addDFChildren(model, field="Sepal.Length", 
              intervals=list(newXMLNode("Interval",
              attrs=c(closure="openClosed",rightMargin=3))))

 # use helper functions to create list of 'Interval' and 'Value' 
 # elements. We define the 3 Intervals as ,1]  (1,2)  and [2,  
 mi<-makeIntervals(list("openClosed","openOpen","closedOpen"),
                  list(NULL,1,2),list(1,2,NULL))
 
 # define 3 values, none with a 'displayValue' attribute and 1 value 
 # defined as 'invalid'. The 2nd one is 'valid' by default.
 mv<-makeValues(list(1.1,2.2,3.3),list(NULL,NULL,NULL),
                list("valid",NULL,"invalid"))
 
 # As an example, apply these to the Sepal.Length field.
 addDFChildren(model, field="Sepal.Length", intervals=mi, values=mv)
 # Only defined 'Interval's 
 addDFChildren(model, field="Sepal.Length", intervals=mi)

}
\keyword{interface}


