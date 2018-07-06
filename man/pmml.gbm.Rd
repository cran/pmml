\name{pmml.gbm}
\alias{pmml.gbm}
\title{Generate PMML for genralized boosting tree objects}
\description{
  
  Generate the PMML representation for a gbm object from package \pkg{gbm}.

}
\usage{
\method{pmml}{gbm}(model, model.name="gbm_Model", app.name="R/PMML",
     description="Generalized Boosted Tree Model", copyright=NULL, transforms=NULL, 
     unknownValue=NULL,  ...)
}

\arguments{
  
  \item{model}{a \code{gbm} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  This is an optimized function which outputs a gbm object produced by the \pkg{gbm} package in 
  PMML format. This output is processed to minimize the time and memory requirements.The model 
  will include not just the model but also any pre-processing applied to the training data.

  The 'gbm' function uses various distribution types to fit a model; currently only the "bernoulli", "poisson"
  and "multinomial" distribution types are supported. For all cases the model output includes the gbm prediction
  type "link" and "response".
 
}

\references{
  R project CRAN package:\cr
  \emph{\bold{gbm}: Generalized Boosted Regression Models} \cr
  \url{https://CRAN.R-project.org/package=gbm}

}

\author{\email{tridivesh.jena@softwareag.com}}	

\examples{

# Build a simple gbm model

library(gbm)
library(pmml)
data(audit)

mod<-gbm(Adjusted~.,data=audit[,-c(1,4,6,9,10,11,12)],n.trees=3,interaction.depth=4)
# since distribution type is not given, a bernoulli distribution will be assumed

# Convert to pmml
pmml(mod)

# now try a classification case
mod2<-gbm(Species~.,data=iris,n.trees=2,interaction.depth=3,distribution="multinomial")

# the PMML now will include a regression model to read the gbm object outputs 
# and convert to a "response" prediction type.
pmml(mod2)

}

