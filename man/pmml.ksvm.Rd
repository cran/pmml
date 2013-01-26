\name{pmml.ksvm}
\alias{pmml.ksvm}
\title{
  Generate PMML for a ksvm object
}
\description{
  
  Generate the PMML representation for a ksvm object (SVM). The PMML can
  then be imported into other systems that accept PMML.  With this code,
  a PMML representation can be obtained for SVMs implementing
  classification (multi-class and binary) as well as regression.
  
}
\usage{
\method{pmml}{ksvm}(model, model.name="SVM_model", app.name="Rattle/PMML",
     description="Support Vector Machine PMML Model", copyright=NULL,
     transforms=NULL, dataset=NULL, weights=NULL, \dots)
}
\arguments{
  
  \item{model}{a ksvm object.}

  \item{dataset}{the original dataset used to train the SVM
  model in ksvm - required since the ksvm object does not appear to
  record information about the used categorical variables.}
  
  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed.}

  \item{weights}{the weights used for building the model.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The generated PMML can be imported into any PMML consuming application
  that recognizes PMML 3.2. An example is ADAPA (Adaptive Decision and
  Predictive Analytics), a lightweight decision engine with batch and
  real-time scoring of predictive models (also supporting neural
  networks and linear and logistic regression).

}
\references{

  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

  Zementis' useful PMML convert: \url{http://www.zementis.com/pmml_converters.htm}
  
  ADAPA home page: \url{http://www.zementis.com/adapa.htm}

}
\author{\email{info@zementis.com}}
\seealso{
  \code{\link{pmml}}.
  \code{\link[kernlab]{ksvm}}.
}
\examples{
# Train a support vector machine to perform classification.

require(kernlab)
model  <- ksvm(Species ~ ., data=iris)
p <- pmml(model, dataset=iris)

# Save to file.

\dontrun{saveXML(p, "iris_svm.xml")}
}
\keyword{interface}
