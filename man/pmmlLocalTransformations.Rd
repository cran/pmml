\name{pmmlLocalTransformations}
\alias{pmmlLocalTransformations}
\title{Handle transforms for pmml export.}
\description{

  Generate and return a LocalTransformations element.
  
}
\usage{
pmmlLocalTransformations(field, transforms=NULL, LTelement=NULL)
}
\arguments{
  
  \item{field}{field$name is a vector of strings, and includes target;
    field$class is indexed by fields$name.}

  \item{transforms}{transforms.}

  \item{LTelement}{local transform element.}

}
\details{

  Generate and return a LocalTransformations element that incldues
  each supplied field.

}
\references{

  PMML home page: \url{http://www.dmg.org}

}
\author{\email{Graham.Williams@togaware.com}, Zementis Inc.}
\seealso{

  \code{\link{pmml}}.

}
