\name{pmml.naiveBayes}
\alias{pmml.naiveBayes}
\title{Generate PMML for a Naive Bayes (naiveBayes) object}
\description{
  
  Generate the Predictive Model Markup Language (PMML) representation of
  a naive bayes object from \pkg{e1071}.  This object is the result of
  the model created using the naiveBayes function of the package.

}
\usage{
\method{pmml}{naiveBayes}(model, model.name="naiveBayes_Model",
     app.name="Rattle/PMML",
     description="NaiveBayes Model",
     copyright = NULL,
     transforms = NULL,
     dataset = NULL,
     predictedField, ...)
}
\arguments{
  
  \item{model}{the \code{naiveBayes} object contained in an object of class
      \pkg{e1071}, as that contained in the object returned by
      the function \code{naiveBayes}.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{transforms to represent within the PMML.}

  \item{dataset}{the dataset name of the data used to construct the model.
      Required to get the field variable names}
 
  \item{predictedField}{Required; the name of the predicted field.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The Predictive Model Markup Language is an XML based language which
  provides a way for applications to define statistical and data mining
  models and to share models between PMML compliant applications.  More
  information about PMML and the Data Mining Group can be found at
  http://www.dmg.org. 

  The PMML representation of the NaiveBayes model implements the representation
  as defined by the Data Mining Group: intermediate probability values which
  are less than the threshold value are replaced by the threshold value. This
  is different from the prediction function of the \pkg{e1071} in which only
  probability values of 0 and standard deviations of continuous variables of 
  with the value 0 are replaced by the threshold value. The two values will
  therefore not match exactly for cases involving very small probability values.  

}    
\references{

  PMML home page: \url{http://www.dmg.org}

  A. Guazzelli, W. Lin, T. Jena (2012), /emph{PMML in Action: Unleashing the Power  
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace 
     (Second Edition - Available on Amazon.com - \url{http://www.amazon.com/dp/1470003244}.

  A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), /emph{PMML: An Open Standard for 
     Sharing Models}. The R journal, Volume 1/1, 60-65

}
\author{\email{info@zementis.com}}	
\seealso{
  \code{\link{pmml}}.
}
\examples{
# Build a simple Naive Bayes model

# Upload the required library
library(e1071)
library(pmml)
library(mlbench)

# download an example dataset
data(HouseVotes84)
house <- na.omit(HouseVotes84)

# Construct an example model defining a threshold value of 0.003
model<-naiveBayes(Class~V1+V2+V3,data=house,threshold=0.003)

# Output the PMML representation 
pmml(model,dataset=house,predictedField="Class")

}
\keyword{interface}
\keyword{tree}
