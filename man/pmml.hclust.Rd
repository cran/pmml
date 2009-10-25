\name{pmml.hclust}
\alias{pmml.hclust}
\title{
  Generate PMML for a hclust object
}
\description{
  
  Generate a PMML representation for a hierarchical cluster object. The
  hclust object will be approximated by k centroids and is converted
  into a PMML representation for kmeans clusters. The PMML can then be
  imported into other systems that accept PMML.
  
}
\usage{
\method{pmml}{hclust}(model, model.name="HClust_Model", app.name="Rattle/PMML",
     description="Hierarchical cluster model", copyright=NULL,
transforms=NULL, dataset=NULL, centers, \dots)
}
\arguments{
  
  \item{model}{a hclust object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed.}
  
  \item{dataset}{the orginal training dataset, if available.}

  \item{centers}{a list of means to represent the clusters.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\references{

  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

}
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml}},
  \code{\link{hclust}}.
}
\keyword{interface}
