\name{addLT}
\alias{addLT}
\title{adds a LocalTransformations element to a given PMML file.}
\description{

  The pmmlTransformations package allows one to create a LocalTransformations 
  element describing the data manipulations desired. This function allows one to
  add this information to a given PMML file; thus combining the description of any
  data processing as well as the model using such transformed data.

}
\usage{
addLT(xmlmodel=NULL, transforms=NULL, namespace="4_2",...)
}
\arguments{
  
  \item{xmlmodel}{the PMML model in a XML node format. If the model is a text file, it should be converted
		to an XML node, for example, using the fileToXMLNode function.}
  \item{transforms}{the transformations performed on the initial data. This is the LocalTransformations
		element as an XML node object.}
  \item{namespace}{the namespace of the PMML model. This is frequently also the PMML version the model is 
	represented as.}
  \item{\dots}{further arguments passed to or from other methods.}

}
\details{
  The attribute information should be provided as a dataframe; each row corresponding to an attribute
  name and each column corresponding to a variable name. This way one can add as many attributes to as many 
  variables as one wants in one step. On the other extreme, a one-by-one data frame may be used to add
  one new attribute to one variable. This function may be used multiple times to add new attribute values
  step-by-step. This function overwrites any pre-existing attribute values, so it must be used with care.
  However, this is by design as this feature is meant to help an user defined new attribute values at 
  different times. For example, one may use this to impute missing values in a model at fifferent times.

}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package. This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\author{\email{tridivesh.jena@zementis.com}}

\examples{

\dontrun{
# make a sample model

library(pmml)
model <- pmml(lm(Sepal.Length~., data=iris[,-5]))

# Perform a z-score transform on the first variable of the data set. 
# As it is created and used in the same R session, this object is already an xml node, 
# not an external text file; so there is no need to convert it to an xml node object.

library(pmmlTransformations)
irisBox <- WrapData(iris)
irisBox <- ZScoreXform(irisBox,"1")
xforms <- pmml(,transforms=irisBox)

# add the LocalTransformations element to the initial PMML model
# since the model still uses the original fields, the usage envisioned for this function
# is to make it easy if the modeller forgot to add the transformations when using the pmml
# function initially. 

modified <- addLT(model,xforms,namespace="4_2")
}

}
\keyword{interface}


