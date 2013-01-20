\name{pmml.rsf}
\alias{pmml.rsf}
\title{Generate PMML for a Random Survival Forest (rsf) object}
\description{
  
  Generate the Predictive Model Markup Language (PMML) representation of
  a \pkg{randomSurvivalForest} forest object.  In particular, this
  function gives the user the ability to save the geometry of a forest
  as a PMML XML document.

}
\usage{

\method{pmml}{rsf}(model,
                   model.name="rsfForest_Model",
                   app.name="Rattle/PMML",
                   description="Random Survival Forest Tree Model",
                   copyright=NULL,
                   transforms=NULL,
                   ...)

}
\arguments{
  
  \item{model}{the \code{forest} object contained in an object of class
      \pkg{randomSurvivalForest}, as that contained in the object
      returned by the function \code{rsf} with the parameter
      \dQuote{forest=TRUE}.}

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

  Use of PMML and \code{pmml.rsf} requires the \pkg{XML} package.  Be
  aware that XML is a very verbose data format.  Reasonably sized trees
  and data sets can lead to extremely large text files.  XML, while
  achieving interoperability, is not an efficient data storage mechanism
  in this case.

  This function is used to export the geometry of the forest to other
  PMML compliant applications, including graphics packages that are
  capable of printing binary trees.  In addition, the user may wish to
  save the geometry of the forest for later retrieval and prediction on
  new data sets using \code{pmml.rsf} together with \code{pmml_to_rsf}.

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

  A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), /emph{PMML: An Open Standard for
     Sharing Models}. The R journal, Volume 1/1, 60-65

  PMML home page: \url{http://www.dmg.org}
}	
\author{\email{info@zementis.com}}
\seealso{
  \code{\link{pmml}}.
}
\examples{
# Until the rsf package is updated, do not run this.
# library(randomSurvivalForest)
# data(veteran, package = "randomSurvivalForest")
# veteran.out <- rsf(Survrsf(time, status)~.,
#         data = veteran,
#         ntree = 5,
#         forest = TRUE)
# veteran.forest <- veteran.out$forest
# pmml(veteran.forest)
}
\keyword{interface}
\keyword{survival}
\keyword{tree}
