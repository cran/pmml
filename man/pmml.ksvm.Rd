\name{pmml.ksvm}
\alias{pmml.ksvm}
\title{
  Generate PMML for a ksvm object
}
\description{
  
  Generate the Predictive Model Markup Language (PMML) 
  representation for a ksvm object (SVM). With this code,
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

  PMML is an XML based language which
  provides a way for applications to define statistical and data mining
  models and to share models between PMML compliant applications.  More
  information about PMML and the Data Mining Group can be found at
  http://www.dmg.org.  

  The generated PMML can be imported into any PMML consuming
  application, such as the Zementis ADAPA and UPPI scoring engines which allow for
  predictive models built in R to be deployed and executed on site, in the cloud 
  (Amazon, IBM, and FICO), in-database (IBM Netezza, Pivotal, Sybase IQ, Teradata and 
  Teradata Aster) or Hadoop (Datameer and Hive). 


}
\references{

  Rattle home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

  A. Guazzelli, W. Lin, T. Jena (2012), \emph{PMML in Action: Unleashing the Power  
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace 
     (Second Edition) - Available on Amazon.com - \url{http://www.amazon.com/dp/1470003244}.

  A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), PMML: An Open Standard for 
     Sharing Models. \emph{The R journal}, Volume 1/1, 60-65


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

# To make predictions using this model, the new data must be given; without it and by
# simply using the "predict" function without an input dataset, the predicted value 
# will not be the true predicted value. It will be a raw predicted value which must be
# post-processed to get the final correct predicted value
#
# Make predictions using same iris input data. Even though it is the same dataset, it
# must be provided as an input parameter for the "predict" function. 

predict(model,iris[,1:4])

# Save to file.

\dontrun{saveXML(p, "iris_svm.xml")}
}
\keyword{interface}
