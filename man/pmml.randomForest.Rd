\name{pmml.randomForest}
\alias{pmml.randomForest}
\title{Generate PMML for a randomForest object}
\description{
  
  Generate the Predictive Model Markup Language (PMML) representation of
  a \pkg{randomForest} forest object.  In particular, this
  function gives the user the ability to save the geometry of a forest
  as a PMML XML document.

}
\usage{
\method{pmml}{randomForest}(model, model.name="randomForest_Model", app.name="Rattle/PMML",
     description="randomForest model", copyright, ...)
}
\arguments{
  
  \item{model}{the \code{forest} object contained in an object of class
      \pkg{randomForest}, as that contained in the object returned by
      the function \code{randomForest}.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

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

  H. Ishwaran and Udaya B. Kogalur (2006).  Random Survival Forests.
  \emph{Cleveland Clinic Technical Report}.

  PMML home page: \url{http://www.dmg.org}
}	
\author{

  Hemant Ishwaran \email{hemant.ishwaran@gmail.com} and Udaya B. Kogalur
  \email{ubk2101@columbia.edu} with incorporation into the pmml package
  by \email{Graham.Williams@togaware.com}

}
\seealso{
  \code{\link{pmml}}.
}
\keyword{interface}
\keyword{tree}
