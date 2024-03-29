% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmml.randomForest.R
\name{pmml.randomForest}
\alias{pmml.randomForest}
\title{Generate the PMML representation for a randomForest object from the package
\pkg{randomForest}.}
\usage{
\method{pmml}{randomForest}(
  model,
  model_name = "randomForest_Model",
  app_name = "SoftwareAG PMML Generator",
  description = "Random Forest Tree Model",
  copyright = NULL,
  model_version = NULL,
  transforms = NULL,
  missing_value_replacement = NULL,
  parent_invalid_value_treatment = "returnInvalid",
  child_invalid_value_treatment = "asIs",
  ...
)
}
\arguments{
\item{model}{A randomForest object.}

\item{model_name}{A name to be given to the PMML model.}

\item{app_name}{The name of the application that generated the PMML.}

\item{description}{A descriptive text for the Header element of the PMML.}

\item{copyright}{The copyright notice for the model.}

\item{model_version}{A string specifying the model version.}

\item{transforms}{Data transformations.}

\item{missing_value_replacement}{Value to be used as the 'missingValueReplacement'
attribute for all MiningFields.}

\item{parent_invalid_value_treatment}{Invalid value treatment at the top
MiningField level.}

\item{child_invalid_value_treatment}{Invalid value treatment at the model
segment MiningField level.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
PMML representation of the randomForest object.
}
\description{
Generate the PMML representation for a randomForest object from the package
\pkg{randomForest}.
}
\details{
This function outputs a Random Forest in PMML format.
}
\examples{
\dontrun{
# Build a randomForest model
library(randomForest)
iris_rf <- randomForest(Species ~ ., data = iris, ntree = 20)

# Convert to pmml
iris_rf_pmml <- pmml(iris_rf)

rm(iris_rf)
}
}
\references{
\href{https://CRAN.R-project.org/package=randomForest}{randomForest:
Breiman and Cutler's random forests for classification and regression}
}
\author{
Tridivesh Jena
}
