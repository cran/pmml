\name{fileToXMLNode}
\alias{fileToXMLNode}
\title{Reads in a file and tries to parse it into an object of type XMLNode}
\description{
 
  This function can be used when the user wants to read in an external file and
  convert it into an XMLNode to be used subsequently by other R functions.

}
\usage{
fileToXMLNode(file)
}
\arguments{
  
  \item{file}{the external file to be read in. This file can be any file in PMML format,
	      regardless of the source or model type.} 

}
\details{
  This function reads in a file and attempts to parse it into an XML node. This format is the
  one that will be obtained when a model is constructed in R and output in PMML format.

  This function is mainly meant to be used to read in external files instead of depending on 
  models saved in R. As an example, the pmml package requires as input an object of type 
  XMLNode before its functions can be applied. Function 'fileToXMLNode' can be used to read
  in an existing PMML file, convert it to an XML node and then make it available for use by
  any of the pmml functions.

}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package. This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\author{\email{tridivesh.jena@softwareag.com}}

\examples{

\dontrun{
# define some transformations
 library(pmml)
 library(pmmlTransformations)

 irisBox <- WrapData(iris)
 irisBox <- ZScoreXform(irisBox,xformInfo = "column1->d1")
 irisBox <- ZScoreXform(irisBox,xformInfo = "column2->d2")

#make a LocalTransformations element and save it to an external file
 pmml_trans <- pmml(NULL, transforms=irisBox)
 write(toString(pmml_trans),file = "xform_iris.pmml")

 # Later, we may need to read in the PMML model into R
 # 'lt' below is now a XML Node, as opposed to a string

 lt <- fileToXMLNode("xform_iris.pmml")
}
}
\keyword{interface}


