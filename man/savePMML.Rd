\name{savePMML}
\alias{savePMML}
\title{saves a xml object as an external PMML file.}
\description{

  A created pmml object can be saved for more efficient further processing via this function.

}
\usage{
savePMML(doc, name, version=4.3)
}
\arguments{
  
  \item{doc}{the XML model created in R.} 
  \item{name}{the name of the external file where the XML is to be saved.}
  \item{version}{the PMML version number the model is compliant with.}

}
\author{\email{tridivesh.jena@softwareag.net}}

\examples{

\dontrun{
 # make a sample model
 library(gbm)
 library(pmml)
 data(audit)

 mod<-gbm(Adjusted~.,data=audit[,-c(1,4,6,9,10,11,12)],n.trees=3,interaction.depth=4)
 pmod <- pmml(mod)
 # Save to an external file 
 savePMML(pmod, "GBMModel.pmml")
}

}
\keyword{interface}


