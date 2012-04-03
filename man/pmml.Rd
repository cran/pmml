\name{pmml}
\alias{pmml}
\title{Generate PMML for an R object}
\description{
  
  'pmml' is a generic function implementing S3 methods used to produce
  the PMML (Predictive Model Markup Language) representation of an R
  model.  The PMML can then be imported into other systems that accept
  PMML.
  
}
\usage{
pmml(model, model.name="Rattle_Model", app.name="Rattle/PMML",
     description=NULL, copyright=NULL, transforms=NULL, dataset=NULL,
     \dots)
}
\arguments{
  
  \item{model}{an object to be converted to PMML.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed.}

  \item{dataset}{the orginal training dataset, if available.}

  \item{\dots}{further arguments passed to or from other methods.}

}
\details{

  The generated PMML can be imported into any PMML consuming
  application, such as Teradata Warehouse Miner and DB2. Generally,
  these applications convert the PMML into SQL for execution across a
  database.

}
\references{

  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

}
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml.rules}},
  \code{\link{pmml.hclust}},
  \code{\link{pmml.kmeans}},
  \code{\link{pmml.ksvm}},
  \code{\link{pmml.glm}},
  \code{\link{pmml.lm}},
  \code{\link{pmml.nnet}},
  \code{\link{pmml.multinom}},
  \code{\link{pmml.rpart}},
  \code{\link{pmml.randomForest}},
  \code{\link{pmml.rsf}},
  \code{\link{pmmltoc}}.
}
\examples{
# Build a simple lm model
(iris.lm <- lm(Sepal.Length ~ ., data=iris))

# Convert to pmml
pmml(iris.lm)
}
\keyword{interface}
