\name{addLT}
\alias{addLT}
\title{Combines a given LocalTransformations Node with a preexisting PMML model}
\description{
 
  This function is called by the 'pmml' function when adding a given new 
  LocalTransformations node to a pre-existing PMML model. 

}
\usage{
addLT(xmlmodel=NULL, transforms=NULL)
}
\arguments{
  
  \item{xmlmodel}{the PMML model in as a XMLNode object.} 

  \item{transforms}{The wrapper object obtained by using the WrapData function
	from the 'pmmlTransformations' package. This object contains the 
	information on all the transformations to be performed.}

}
\details{

  This is an internal function that takes as an input a PMML model as a XMLNode object and a 
  'LocalTransformations' element as a XMLNode object. These are the formats in which models
  are output when using the 'pmml' function on the R models supported by the pmml package. 
  This function checks to see if any of the variables used in the model are actually derived 
  from another field described in the 'LocalTransformations' element; this is the PMML 
  element that is included in the wrapper object passed as the second argument. It then 
  replaces the fields in the 'DataDictionary' and the 'MiningSchema' elements with the original
  fields from which they were derived from and adds the 'LocalTransformations' element to the 
  existing PMML file. If a 'LocalTransformations' element already exists, the new 
  transformations are appended to it.

  It further checks the possible categories of the fields already derived and adds any new 
  possible values. It also checks for the correct data types of the new fields. The result 
  is a new consistent model file in PMML format which would appear as if it was made from 
  the original variables directly. The inputs are the original variables of the right data 
  types and the right categories, the right number of fields with no repetition and the 
  appropriate transformations. The functionality achieved by 'addLT' is described in the 
  book 'PMML in Action (Second Edition)' referenced to in this document (refer to Chapter 5 
  entitled 'The Making of a Predictive Solution').

  The scenario implemented here is that the model uses fields which are later realized 
  to be derivable from other more basic fields. The only condition
  is that the transformations may not contain an arbitrary transform; all
  transforms defined must either directly or eventually via a transformation
  chain end up as a field presently used by the model. Without
  remaking the entire model, this function is used to update the model. Consider the 
  example of the iris data set. 
  Some possible use cases are:
  1) many different variables are actually derived from the same original field. The variable
     'Species' may be mapped to various other values of a derived field 'd_Species'. Further,
     the same variable 'Species' may be discretized to 3 numerical variables:
     'Species_setosa', 'Species_versicolor' and 'Species_virginica'. Hence
     multiple fields are derived from the same original field.
  2) The field used in the model is derived from a chain of transformations. The example 
     shows that 'Sepal.Length' is first derived into 'd1' and then 'd1' is derived to
     'dd1'. The final field 'dd1' is hence derived from a chain of transformations starting
     from 'Sepal.Length'.

  It is a very lengthy and complicated process to combine two models; besides just replacing
  the variables, change in variable data types, change in variable statistics and change in 
  variable categories must be correctly implemented. This is not easily done by hand and this
  function does all this automatically for the user. 

}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package.  This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\author{\email{tridivesh.jena@zementis.com}}
\references{

  PMML home page: \url{http://www.dmg.org}

  A. Guazzelli, W. Lin, T. Jena (2012), \emph{PMML in Action: Unleashing the Power 
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace                
     (Second Edition - Available on Amazon.com - \url{http://www.amazon.com/dp/1470003244}.

  A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), \emph{PMML: An Open Standard for 
     Sharing Models}. The R journal, Volume 1/1, 60-65

  R pmmlTransformations package: 
     \url{http://cran.r-project.org/web/packages/pmmlTransformations/index.html}

}
\seealso{
  \code{\link{pmml}}.
}
\examples{
\dontrun{
## Not run:
# The following commands can be run from R given a text file "class_table.csv" which, 
# for example, may contain the 2 lines:
# setosa,1
# versicolor,2
#
# Naturally, the file does not contain the hash character. This file contains information 
# which maps the input field so that its value 'setosa' is mapped to the value 1 and its 
# value 'versicolor' is mapped to 2. See the manual of the 'pmmlTransformations' package for 
# details

# Create a pmmlTransformations object
library(pmmlTransformations)
irisBox <- WrapData(iris)

# Transform the 'Sepal.Length' variable to 'dd1' via a chain
irisBox <- ZScoreXform(irisBox,xformInfo = "Sepal.Length->d1")
irisBox <- MinMaxXform(irisBox,xformInfo = "d1->dd1")

#transform the same variable 'class' to many different derived variables
irisBox <- MapXform(irisBox,xformInfo = "[Species->d_Species][string->double]",
table="class_table3.csv",defaultValue="-1",mapMissingTo="1")

irisBox <- NormDiscreteXform(irisBox,inputVar="Species")

# make a clustering model calculating 'Species' using the derived fields, then convert 
# to PMML format
fit <- kmeans(irisBox$data[,c(6:11)],5)
pmml_fit <- pmml(fit, predictedField="Species")

# now we imagine that we realize the fields we used were actually derived from the basic iris
# data set. First we put the transformation information in PMML format.
pmml_trans <- pmml(NULL, transforms=irisBox)

# Finally, we can see the combined PMML file
pmml_combined <- pmml(pmml_fit, transforms=pmml_trans)

## End(Not run)
}
}
\keyword{interface}


