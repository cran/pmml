\name{pmml.coxph}
\alias{pmml.coxph}
\title{Generate PMML for coxph objects}
\description{
  Generate the PMML representation for a coxph object from package \pkg{survival}.
}
\usage{
\method{pmml}{coxph}(model, model.name="CoxPH_Survival_Regression_Model",
    app.name="Rattle/PMML", 
    description="CoxPH Survival Regression Model", 
    copyright=NULL, transforms=NULL, unknownValue=NULL, \dots)
}
\arguments{
  
  \item{model}{a coxph object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  A coxph object is the result of fitting a proportional hazards regression model, using the "coxph"
  function from the package \pkg{survival}. Although the \pkg{survival} package supports 
  special terms "cluster", "tt" and "strata", only the special term "strata" is supported by the
  \pkg{pmml} package. Note that special term "strata" cannot be a multiplicative variable and only numeric risk 
  regression is supported.

}

\references{
  R project CRAN package: \emph{\bold{survival}: Survival Analysis} \cr
  \url{https://CRAN.R-project.org/package=survival}

}

\author{\email{Graham.Williams@togaware.com},   Zementis Inc.}
