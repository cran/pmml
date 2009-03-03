\name{pmmltoc}
\alias{pmmltoc}
\title{Generate C code from a PMML object - dummy function}
\description{
  
  This is a dummy function that does nothing. Plugins for Rattle are
  starting to appear which implement this for specific
  environments. This is experimental.
}
\usage{
pmmltoc(p, name=NULL, includePMML=TRUE, includeMetaData=TRUE, exportClass=TRUE)
}
\arguments{
  
  \item{p}{pmml.}

  \item{name}{a name to give to the model in the C code.}

  \item{includePMML}{include the actual PMML as comments.}

  \item{includeMetaData}{include model information as comments.}
  
  \item{exportClass}{whether to export class or probability.}

}
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml}}.
}
\keyword{interface}
