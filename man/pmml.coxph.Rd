% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmml.coxph.R
\name{pmml.coxph}
\alias{pmml.coxph}
\title{Generate the PMML representation for a coxph object from the package
\pkg{survival}.}
\usage{
\method{pmml}{coxph}(
  model,
  model_name = "CoxPH_Survival_Regression_Model",
  app_name = "SoftwareAG PMML Generator",
  description = "CoxPH Survival Regression Model",
  copyright = NULL,
  model_version = NULL,
  transforms = NULL,
  missing_value_replacement = NULL,
  ...
)
}
\arguments{
\item{model}{A coxph object.}

\item{model_name}{A name to be given to the PMML model.}

\item{app_name}{The name of the application that generated the PMML.}

\item{description}{A descriptive text for the Header element of the PMML.}

\item{copyright}{The copyright notice for the model.}

\item{model_version}{A string specifying the model version.}

\item{transforms}{Data transformations.}

\item{missing_value_replacement}{Value to be used as the 'missingValueReplacement'
attribute for all MiningFields.}

\item{...}{Further arguments passed to or from other methods.}
}
\description{
Generate the PMML representation for a coxph object from the package
\pkg{survival}.
}
\details{
A coxph object is the result of fitting a proportional hazards regression
model, using the \code{coxph} function from the package \pkg{survival}. Although
the \pkg{survival} package supports special terms "cluster", "tt" and
"strata", only the special term "strata" is supported by the \pkg{pmml}
package. Note that special term "strata" cannot be a multiplicative variable
and only numeric risk regression is supported.
}
\references{
\href{https://CRAN.R-project.org/package=survival}{coxph: Survival
Analysis}
}
\author{
Graham Williams
}
