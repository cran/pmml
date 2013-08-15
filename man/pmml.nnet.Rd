\name{pmml.nnet}
\alias{pmml.nnet}
\title{
  Generate PMML for a nnet object
}
\description{
  
  Generate the Predictive Model Markup Language (PMML) 
  representation for a nnet object (Neural Network). 
  With this code, a PMML representation can be obtained for Neural
  Networks implementing classification (multi-class and binary) as well
  as regression.
  
}
\usage{
\method{pmml}{nnet}(model, model.name="NeuralNet_model", app.name="Rattle/PMML",
     description="Neural Network PMML Model", copyright=NULL,
transforms=NULL, \dots)
}
\arguments{
  
  \item{model}{a nnet object.}
  
  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed.}

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
}
\keyword{interface}
