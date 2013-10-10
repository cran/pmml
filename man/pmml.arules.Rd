\name{pmml.rules}
\alias{pmml.rules}
\alias{pmml.itemsets}
\title{Generate PMML for arules objects}
\description{  
  Generate the PMML representation for a rules or an itemset object from package \pkg{arules}. 
}
\usage{
\method{pmml}{rules}(model, model.name="arules_Model", app.name="Rattle/PMML",
     description="arules association rules model", copyright=NULL, transforms = NULL, \dots)
\method{pmml}{itemsets}(model, model.name="arules_Model", app.name="Rattle/PMML",
     description="arules frequent itemsets model", copyright=NULL, transforms = NULL, \dots)
}
\arguments{
  
  \item{model}{a rules or itemsets object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{not used in present version.}

  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
  The model is represented in the PMML AssociationModel format.
}

\references{
  R project CRAN package: \emph{\bold{arules}: Mining Association Rules and Frequent Itemsets} \cr
  \url{http://cran.r-project.org/web/packages/arules/index.html}

}

\author{Michael Hahsler (\email{michael@hahsler.net})}
