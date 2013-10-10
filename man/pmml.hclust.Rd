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
     transforms=NULL, unknownValue=NULL, centers, \dots)
}
\arguments{
  
  \item{model}{a hclust object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
  
  \item{centers}{a list of means to represent the clusters.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  This function converts a hclust object created by the 'hclusterpar' function from the 'amap' package. 
  A hclust object is a cluster model created hierarchically. The data is divided recursively until
  a criteria is met. This function then takes the final model and represents it as a standard
  k-means cluster model. This is possible since while the method of constructing the model is different, 
  the final model can be represented in the same way.

  To use this pmml function, therefore, one must pick the number of clusters desired and the coordinate 
  values at those cluster centers. This can be done using the 'hclusterpar' and 'centers.hclust' functions 
  from the 'amap' and 'rattle' packages repectively.
} 

\references{
  R project: \emph{\bold{Hierarchical Clustering}} \cr
  \url{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html}

}

\author{\email{Graham.Williams@togaware.com}}

\examples{

#\dontrun{
# cluster the 4 numeric variables of the iris dataset
library(amap)
model <- hclusterpar(iris[,-5])

# get the information about the cluster centers
# the last parameter of the function used is the number of clusters desired
 library(rattle)
centerInfo <- centers.hclust(iris[,-5],model,3)

# convert to pmml
library(pmml)
pmml(model,centers=centerInfo)
#}

}


