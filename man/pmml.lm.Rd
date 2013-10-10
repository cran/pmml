\name{pmml.lm}
\alias{pmml.lm}
\title{Generate PMML for lm objects}
\description{
  
  Generate the PMML representation for a lm object from package \pkg{stats}. 
  
}
\usage{
\method{pmml}{lm}(model, model.name="Linear_Regression_Model", app.name="Rattle/PMML",
     description="Linear Regression Model", copyright=NULL,
     transforms=NULL, unknownValue=NULL, dataset=NULL, weights=NULL, \dots)
}
\arguments{
  
  \item{model}{a lm object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}

  \item{dataset}{the orginal training dataset, if available.}

  \item{weights}{the weights used for building the model.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
  Note that the resulting PMML representation will not encode interaction
  terms. Currently, only numeric regression is supported. 
}

\references{
  R project: \emph{\bold{Fitting Linear Models}} \cr
  \url{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html}

}

\author{\email{rguha@indiana.edu}}

\examples{
fit <- lm(Sepal.Length ~ ., data=iris)
pmml(fit)

rm(fit)

}
