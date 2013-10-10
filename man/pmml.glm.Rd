\name{pmml.glm}
\alias{pmml.glm}
\title{Generate PMML for glm objects}
\description{
  Generate the PMML representation for a glm object from package \pkg{stats}.
}
\usage{
\method{pmml}{glm}(model, model.name="General_Regression_Model", app.name="Rattle/PMML",
     description="Generalized Linear Regression Model", copyright=NULL,
     transforms=NULL, unknownValue=NULL, weights=NULL, ...)
}
\arguments{
  
  \item{model}{a \code{glm} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
  
  \item{weights}{the weights used for building the model.}

  \item{\dots}{further arguments passed to or from other methods.}
}

\details{  
  The function exports the glm model in the PMML GeneralRegressionModel format.
}    


\references{
  R project: \emph{\bold{Fitting Generalized Linear Models}} \cr
  \url{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}

}

\author{Zementis Inc. \email{info@zementis.com}}	
