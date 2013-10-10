\name{pmml.nnet}
\alias{pmml.nnet}
\title{
  Generate PMML for nnet objects
}
\description{ 
  Generate the PMML representation for a nnet object from package \pkg{nnet}. 
}
\usage{
\method{pmml}{nnet}(model, model.name="NeuralNet_model", app.name="Rattle/PMML",
     description="Neural Network PMML Model", copyright=NULL, transforms=NULL, \dots)
}
\arguments{
  
  \item{model}{a nnet object.}
  
  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
  The \code{pmml} function supports both regression and classification neural network
  models. The model is represented in the PMML NeuralNetwork format.
}

\references{
  R project CRAN package:\cr
  \emph{\bold{nnet}: Feed-forward Neural Networks and Multinomial Log-Linear Models} \cr
  \url{http://cran.r-project.org/web/packages/nnet/index.html}
}

\author{Zementis Inc. \email{info@zementis.com}}

\examples{
library(nnet)
fit <- nnet(Species ~ ., data=iris, size=4)
pmml(fit)
}