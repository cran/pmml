\name{pmmlCanExport}
\alias{pmmlCanExport}
\title{Can this installation export PMML variables (particularly transforms).}
\description{
  
  This function is designed to be overriden by other pacakges that
  implement pmml export, particularly of transforms.
  
}
\usage{
pmmlCanExport(vname)
}
\arguments{
  
  \item{vname}{a variable name to check whether it is exportable.}

}
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml}}.
}
\keyword{interface}
