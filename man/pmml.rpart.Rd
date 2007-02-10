\name{pmml.rpart}
\alias{pmml.rpart}
\title{
  Generate PMML for an rpart object (classification tree)
}
\description{
  
  Generate the PMML representation of an RPart object (classification tree).
  
}
\usage{
pmml.rpart(model, model.name="RPart Model", app.name="Rattle/PMML",
           description="RPart decision tree model", copyright)
}
\arguments{
  
  \item{model}{an rpart object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

}
\details{

  An rpart object (currently expected to be a classification tree) is
  converted into a PMML representation. The PMML should then be
  importable into other systems that accept PMML.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
library(rpart)
fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)
pmml.rpart(fit)
}
\keyword{interface}
