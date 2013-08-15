\name{pmml.kmeans}
\alias{pmml.kmeans}
\title{
  Generate PMML for a kmeans object
}
\description{
  
  Generate the Predictive Model Markup Language (PMML) 
  representation for a KMeans object (cluster). The
  kmeans object (a cluster described by k centroids) is converted into a
  PMML representation.
  
}
\usage{
\method{pmml}{kmeans}(model, model.name="KMeans_Model", app.name="Rattle/PMML",
     description="KMeans cluster model", copyright=NULL,
     transforms=NULL, dataset=NULL,
     algorithm.name="KMeans: Hartigan and Wong", \dots)
}
\arguments{
  
  \item{model}{a kmeans object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed.}

  \item{dataset}{not used for kmeans.}

  \item{algorithm.name}{the variety of kmeans used.}

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
  \code{\link{pmml}},
  \code{\link{kmeans}}.
}
\examples{
ds <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
                matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(ds) <- c("Dimension1", "Dimension2")
cl <- kmeans(ds, 2)
pmml(cl)
}
\keyword{interface}
