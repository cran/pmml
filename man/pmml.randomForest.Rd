\name{pmml.randomForest}
\alias{pmml.randomForest}
\title{Generate PMML for a Random Forest (randomForest) object}
\description{
  
  Generate the Predictive Model Markup Language (PMML) representation of
  a \pkg{randomForest} forest object.  In particular, this
  function gives the user the ability to save the geometry of a forest
  as a PMML XML document.

}
\usage{
\method{pmml}{randomForest}(model, model.name="randomForest_Model",
     app.name="Rattle/PMML",
     description="Random Forest Tree Model",
     copyright = NULL,
     transforms = NULL, ...)
}
\arguments{
  
  \item{model}{the \code{forest} object contained in an object of class
      \pkg{randomForest}, as that contained in the object returned by
      the function \code{randomForest}.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{transforms to represent within the PMML.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The Predictive Model Markup Language is an XML based language which
  provides a way for applications to define statistical and data mining
  models and to share models between PMML compliant applications.  More
  information about PMML and the Data Mining Group can be found at
  http://www.dmg.org.  

  Use of PMML and \code{pmml.randomForest} requires the \pkg{XML} package.  Be
  aware that XML is a very verbose data format.  Reasonably sized trees
  and data sets can lead to extremely large text files.  XML, while
  achieving interoperability, is not an efficient data storage mechanism
  in this case.

  This function is used to export the structure of the forest to other
  PMML compliant applications, including graphics packages that are
  capable of printing binary trees.

}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package.  This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\references{

  Breiman, L. (2001), /emph{Random Forests}, Machine Learning 45(1),5-32

  PMML home page: \url{http://www.dmg.org}

  A. Guazzelli, W. Lin, T. Jena (2012), /emph{PMML in Action: Unleashing the Power  
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace 
     (Second Edition - Available on Amazon.com - \url{http://www.amazon.com/dp/1470003244}.

  A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), /emph{PMML: An Open Standard for 
     Sharing Models}. The R journal, Volume 1/1, 60-65

}
\author{\email{info@zementis.com}}	
\seealso{
  \code{\link{pmml}}.
}
\examples{
# Build a simple randomForest model

library(randomForest)
(iris.rf <- randomForest(Species ~ ., data=iris, ntree=2))

# Convert to pmml

pmml(iris.rf)
}
\keyword{interface}
\keyword{tree}
