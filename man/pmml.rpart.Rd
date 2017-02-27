\name{pmml.rpart}
\alias{pmml.rpart}
\title{Generate PMML for rpart objects}
\description{
  Generate the PMML representation for a rpart object from package \pkg{rpart}.
}

\usage{
\method{pmml}{rpart}(model, model.name="RPart_Model",
      app.name="Rattle/PMML",
      description="RPart Decision Tree Model",
      copyright=NULL, transforms=NULL,
      unknownValue=NULL, dataset=NULL, \dots)
}

\arguments{
  
  \item{model}{a \code{rpart} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}

  \item{dataset}{the original dataset used to train the model.}
 
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
   The \code{pmml} function supports regression tree as well as
   classification tree of a \code{rpart} object. The object is represented 
   in the PMML TreeModel format. 
}

\references{
  R project CRAN package: \emph{\bold{rpart}: Recursive Partitioning} \cr
  \url{https://CRAN.R-project.org/package=rpart}

}

\author{\email{Graham.Williams@togaware.com},   Zementis Inc.}

\examples{

library(rpart)
fit <- rpart(Species ~ ., data=iris)
pmml(fit)

rm(fit)

}

