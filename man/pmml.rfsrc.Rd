\name{pmml.rfsrc}
\alias{pmml.rfsrc}
\title{Generate PMML for rsf objects}
\description{
  
  Generate the PMML representation for a \pkg{randomSurvivalForest} forest object.

}
\usage{
\method{pmml}{rfsrc}(model, model.name="rsf_Model", app.name="Rattle/PMML",
     description="Random Survival Forest Model", copyright=NULL, transforms=NULL, 
     unknownValue=NULL, ...)
}
\arguments{
  
  \item{model}{a \code{forest} object contained in an object of class
      \pkg{randomSurvivalForest}, as that contained in the object
      returned by the function \code{rsf} with the parameter
      \dQuote{forest=TRUE}.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}  

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
  
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  This function is used to export the geometry of the forest to other
  PMML compliant applications, including graphics packages that are
  capable of printing binary trees.  In addition, the user may wish to
  save the geometry of the forest for later retrieval and prediction on
  new data sets using \code{pmml.rfsrc} together with \code{pmml_to_rsf}.

}    

\references{
  \itemize{
    \item
     H. Ishwaran, U.B. Kogalur, E.H. Blackstone, M.S. Lauer (2008), /emph{RANDOM SURVIVAL FORESTS}.
     The Annals of Applied Statistics, Vol. 2, No. 3, 841-860

    \item
     H. Ishwaran and Udaya B. Kogalur (2006).  Random Survival Forests.
     \emph{Cleveland Clinic Technical Report}.
  }

}	
\author{Zementis Inc. \email{info@zementis.com}}

\examples{
 library(randomForestSRC)
 data(veteran)
 veteran.out <- rfsrc(Surv(time, status)~., data = veteran, ntree = 5, forest = TRUE)
 pmml(veteran.out)

}

