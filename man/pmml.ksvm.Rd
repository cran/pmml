\name{pmml.ksvm}
\alias{pmml.ksvm}
\title{
  Generate PMML for ksvm objects
}
\description{
  
  Generate the PMML representation for a ksvm object from package \pkg{kernlab}.
}
\usage{
\method{pmml}{ksvm}(model, model.name="SVM_model",
      app.name="Rattle/PMML",
      description="Support Vector Machine PMML Model",
      copyright=NULL, transforms=NULL, unknownValue=NULL,
      dataset=NULL, \dots)
}
\arguments{
  
  \item{model}{a ksvm object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}

  \item{dataset}{required since the ksvm object does not record information about the used 
  categorical variable; the original dataset used to train the SVM model in ksvm.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

   Both classification (multi-class and binary) as well as regression cases are supported.
}

\references{
  R project CRAN package: \emph{\bold{kernlab}: Kernel-based Machine Learning Lab} \cr
  \url{http://cran.r-project.org/package=kernlab}
}

\author{Zementis Inc. \email{info@zementis.com}}

\examples{
# Train a support vector machine to perform classification.
library(kernlab)
model  <- ksvm(Species ~ ., data=iris)
p <- pmml(model, dataset=iris)

# To make predictions using this model, the new data must be given; 
# without it and by simply using the "predict" function without an 
# input dataset, the predicted value will not be the true predicted 
# value. It will be a raw predicted value which must be 
# post-processed to get the final correct predicted value.

# Make predictions using same iris input data. Even though it is the 
# same dataset, it must be provided as an input parameter for the 
# "predict" function. 

predict(model,iris[,1:4])

rm(model)
rm(p)

}
