\name{pmml.hclust}
\alias{pmml.hclust}
\title{
  Generate PMML for hclust objects
}
\description{
  
  Generate the PMML representation for a hierarchical cluster object. The hclust object will 
  be approximated by k centroids and is converted into a PMML representation for kmeans clusters. 
  
}
\usage{
\method{pmml}{hclust}(model, model.name="HClust_Model", app.name="Rattle/PMML",
     description="Hierarchical cluster model", copyright=NULL,
     transforms=NULL, centers, \dots)
}
\arguments{
  
  \item{model}{a hclust object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}
  
  \item{centers}{a list of means to represent the clusters.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  A hclust object is a cluster model created hierarchically. The data is divided recursively until
  a criteria is met. This function then takes the final model and represents it as a standard
  k-means cluster model. This is possible since while the method of constructing the model is different, 
  the final model can be represented in the same way.

} 

\references{
  R project: \emph{\bold{Hierarchical Clustering}} \cr
  \url{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html}

}

\author{\email{Graham.Williams@togaware.com}}

