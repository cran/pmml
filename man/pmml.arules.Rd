\name{pmml.rules}
\alias{pmml.rules}
\alias{pmml.itemsets}
\title{Generate PMML for arules objects}
\description{
  
  Generate the PMML (Predictive Model Markup Language) representation of
  a rules or an itemset object from package \pkg{arules}. 
  The PMML can then be imported into other systems that accept PMML.
  
}
\usage{
\method{pmml}{rules}(model, model.name="arules_Model", 
    app.name="Rattle/PMML",
    description="arules association rules model", copyright=NULL, \dots)
\method{pmml}{itemsets}(model, model.name="arules_Model", 
    app.name="Rattle/PMML",
    description="arules frequent itemsets model", copyright=NULL, \dots)
}
\arguments{
  
  \item{model}{an rules or itemsets object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The generated PMML can be imported into any PMML consuming
  application.
}
\references{
  Package \pkg{arules} home page: 
    \url{http://r-forge.r-project.org/projects/arules}
  
  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

}
\author{Michael Hahsler (\email{michael@hahsler.net})}
\seealso{
  \code{\link{pmml}}.
}
\keyword{interface}
