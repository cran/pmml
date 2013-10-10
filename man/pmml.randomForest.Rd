\name{pmml.randomForest}
\alias{pmml.randomForest}
\title{Generate PMML for randomForest objects}
\description{
  
  Generate the PMML representation for a randomForest object from package \pkg{randomForest}.

}
\usage{
\method{pmml}{randomForest}(model, model.name="randomForest_Model", app.name="Rattle/PMML",
     description="Random Forest Tree Model", copyright=NULL, transforms=NULL, ...)
}
\arguments{
  
  \item{model}{a \code{randomForest} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  This function outputs a Random Forest in PMML format. The model will include 
  not just the forest but also any pre-processing applied to the training data. 
}

\references{
  R project CRAN package:\cr
  \emph{\bold{randomForest}: Breiman and Cutler's random forests for classification and regression} \cr
  \url{http://cran.r-project.org/web/packages/randomForest/index.html}

}

\author{Zementis Inc. \email{info@zementis.com}}	

\examples{
# Build a simple randomForest model

library(randomForest)
iris.rf <- randomForest(Species ~ ., data=iris, ntree=20)

# Convert to pmml

pmml(iris.rf)
}

