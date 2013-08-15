\name{pmml.coxph}
\alias{pmml.coxph}
\title{Generate PMML for a coxph object}
\description{
  
  Generate the PMML (Predictive Model Markup Language) representation of
  a \pkg{coxph} object. 
  
}
\usage{
\method{pmml}{coxph}(model, model.name="CoxPH_Survival_Regression_Model", app.name="Rattle/PMML",
     description="CoxPH Survival Regression Model", copyright=NULL, transforms=NULL, \dots)
}
\arguments{
  
  \item{model}{a coxph object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed, such as a WrapData object 
			from the pmmlTransformations package}

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

  Note that only numeric risk regression is supported currently.

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
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml}}.
}
\keyword{interface}
