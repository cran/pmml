\name{pmml.kmeans}
\alias{pmml.kmeans}
\title{
  Generate PMML for a kmeans object
}
\description{
  
  Generate the PMML representation for a KMeans object (cluster). The
  kmeans object (a cluster described by k centroids) is converted into a
  PMML representation. The PMML can then be imported into other systems
  that accept PMML.
  
}
\usage{
pmml.kmeans(model, model.name="KMeans_Model", app.name="Rattle/PMML",
            description="KMeans cluster model", copyright)
}
\arguments{
  
  \item{model}{a kmeans object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}
}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
ds <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(ds) <- c("Dimension1", "Dimension2")
cl <- kmeans(ds, 2)
pmml.kmeans(cl)
}
\keyword{interface}
