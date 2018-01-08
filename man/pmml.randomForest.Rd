\name{pmml.randomForest}
\alias{pmml.randomForest}
\title{Generate PMML for randomForest objects}
\description{
  
  Generate the PMML representation for a randomForest object from package \pkg{randomForest}.

}
\usage{
\method{pmml}{randomForest}(model, model.name="randomForest_Model",
      app.name="Rattle/PMML", 
      description="Random Forest Tree Model",
      copyright=NULL, transforms=NULL, unknownValue=NULL,
      parentInvalidValueTreatment="returnInvalid",
      childInvalidValueTreatment="asIs", ...)
}
\arguments{
  
  \item{model}{a \code{randomForest} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
  
  \item{parentInvalidValueTreatment}{invalid value treatment at the top MiningField level.}
  
  \item{childInvalidValueTreatment}{invalid value treatment at the model segment MiningField level.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  This function outputs a Random Forest in PMML format. The model will include 
  not just the forest but also any pre-processing applied to the training data. 
}

\references{
  R project CRAN package:\cr
  \emph{\bold{randomForest}: Breiman and Cutler's random forests for classification and regression} \cr
  \url{https://CRAN.R-project.org/package=randomForest}

}

\author{Zementis Inc.}	

\examples{

# Build a simple randomForest model

library(randomForest)
iris.rf <- randomForest(Species ~ ., data=iris, ntree=20)

# Convert to pmml

pmml(iris.rf)

rm(iris.rf)

}

