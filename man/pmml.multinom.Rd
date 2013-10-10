\name{pmml.multinom}
\alias{pmml.multinom}
\title{Generate PMML for multinom objects}
\description{
  
  Generate the PMML representation for a multinom object from package \pkg{nnet}.
}
\usage{
\method{pmml}{multinom}(model, model.name="multinom_Model", app.name="Rattle/PMML",
     description="Multinomial Logistic Model", copyright = NULL,
     transforms = NULL, unknownValue=NULL, ...)
}
\arguments{
  
  \item{model}{a \code{multinom} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  This function outputs the multinomial logistic model in the PMML RegressionModel format.
  It implements the use of numerical, categorical and multiplicative terms involving
  both numerical and categorical variables.
}    

\references{
  R project CRAN package:\cr
  \emph{\bold{nnet}: Feed-forward Neural Networks and Multinomial Log-Linear Models} \cr
  \url{http://cran.r-project.org/web/packages/nnet/index.html}
}

\author{Zementis Inc. \email{info@zementis.com}}	
