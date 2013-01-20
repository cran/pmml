\name{pmml.coxph}
\alias{pmml.coxph}
\title{Generate PMML for a coxph object}
\description{
  
  Generate the PMML (Predictive Model Markup Language) representation of
  a \pkg{coxph} object. The PMML can then be imported into other systems
  that accept PMML.
  
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

  The generated PMML can be imported into any PMML consuming
  application, such as Zementis' ADAPA.

  Only numeric risk regression is supported currently.

}
\references{

  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}
  
  Zementis' useful PMML convert: \url{http://www.zementis.com/pmml_converters.htm}
}
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml}}.
}
\keyword{interface}
