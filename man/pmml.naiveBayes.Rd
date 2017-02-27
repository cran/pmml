\name{pmml.naiveBayes}
\alias{pmml.naiveBayes}
\title{Generate PMML for naiveBayes objects}
\description{
  Generate the PMML representation for a naiveBayes object from package \pkg{e1071}.
}
\usage{
\method{pmml}{naiveBayes}(model, model.name="naiveBayes_Model", 
      app.name="Rattle/PMML", description="NaiveBayes Model",
      copyright=NULL, transforms=NULL, unknownValue=NULL,
      predictedField, ...)
}
\arguments{
  
  \item{model}{a \code{naiveBayes} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
 
  \item{predictedField}{Required parameter; the name of the predicted field.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The PMML representation of the NaiveBayes model implements the definition
  as specified by the Data Mining Group: intermediate probability values which
  are less than the threshold value are replaced by the threshold value. This
  is different from the prediction function of the \pkg{e1071} in which only
  probability values of 0 and standard deviations of continuous variables of 
  with the value 0 are replaced by the threshold value. The two values will
  therefore not match exactly for cases involving very small probabilities.  

}    

\references{
   \itemize{
    \item
       R project CRAN package:\cr
       \emph{\bold{e1071}: Misc Functions of the Department of Statistics (e1071), TU Wien} \cr
       \url{https://CRAN.R-project.org/package=e1071}
  
 
    \item
       A. Guazzelli, T. Jena, W. Lin, M. Zeller (2013). Extending the Naive Bayes Model Element
     in PMML: Adding Support for Continuous Input Variables. 
     In \emph{Proceedings of the 19th ACM SIGKDD Conference on Knowledge Discovery 
     and Data Mining}.
     
   }      
}

\author{Zementis Inc.}	

\examples{

# Build a simple Naive Bayes model

# Upload the required library
library(e1071)
library(pmml)

# download an example dataset
data(houseVotes84)
house <- na.omit(houseVotes84)

# Construct an example model defining a threshold value of 0.003
model<-naiveBayes(Class~V1+V2+V3,data=house,threshold=0.003)

# Output the PMML representation 
pmml(model,dataset=house,predictedField="Class")

rm(model)

}
