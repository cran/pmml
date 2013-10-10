\name{pmml.kmeans}
\alias{pmml.kmeans}
\title{
  Generate PMML for kmeans objects
}
\description{
  
  Generate the PMML representation for a kmeans object (cluster) from package \pkg{stats}. The
  kmeans object (a cluster described by k centroids) is converted into a
  PMML representation.
  
}
\usage{
\method{pmml}{kmeans}(model, model.name="KMeans_Model", app.name="Rattle/PMML",
     description="KMeans cluster model", copyright=NULL,
     transforms=NULL, algorithm.name="KMeans: Hartigan and Wong", \dots)
}
\arguments{
  
  \item{model}{a kmeans object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{algorithm.name}{the variety of kmeans used.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

   A kmeans object is obtained by applying the \code{kmeans} function from the \code{stats}
   package. This method typically requires the user to normalize all the variables, these
   operations can be done using the \code{pmmlTransformations} package so that the normalization
   information is included in the pmml model format.
} 

\references{
  R project: \emph{\bold{K-Means Clustering}} \cr
  \url{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html}

}

\author{\email{Graham.Williams@togaware.com}}

\examples{
ds <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(ds) <- c("Dimension1", "Dimension2")
cl <- kmeans(ds, 2)
pmml(cl)
}

