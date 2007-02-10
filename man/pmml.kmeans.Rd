\name{pmml.kmeans}
\alias{pmml.kmeans}
\title{
  Generate PMML for a kmeans object (cluster)
}
\description{
  
  Generate the PMML representation of an KMeans object (cluster).
  
}
\usage{
pmml.kmeans(model, model.name="KMeans Model", app.name="Rattle/PMML",
            description="KMeans cluster model", copyright)
}
\arguments{
  
  \item{model}{a kmeans object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}
}
\details{

  A kmeans object (a cluster described by k centroids) is converted into
  a PMML representation. The PMML should then be importable into other
  systems that accept PMML.

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
