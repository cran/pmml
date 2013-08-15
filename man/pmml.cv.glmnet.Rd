\name{pmml.cv.glmnet}
\alias{pmml.cv.glmnet}
\title{Generate PMML for a glmnet object}
\description{
  
  Generate the Predictive Model Markup Language (PMML) representation of
  a \pkg{glmnet} elasticnet general linear regression object.  In particular,
  this gives the PMML representation of the object created by the cv.glmnet 
  function. 

}
\usage{
\method{pmml}{cv.glmnet}(model, model.name="Elasticnet_Model", app.name="Rattle/PMML",
     description="Generalized Linear Regression Model", 
     copyright = NULL, transforms = NULL, dataset = NULL, s = NULL, \dots)
}
\arguments{
  
  \item{model}{the \code{cv.glmnet} object contained in an object of class
      \pkg{glmnet}, as contained in the object returned by the function 
      \code{cv.glmnet}.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transforms.}

  \item{dataset}{the dataset using which the model was built.}

  \item{s}{the 'lambda' parameter at which to output the model. If not given,
	    the lambda.min parameter from the model is used.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The glmnet package expects the input and predicted values in a matrix 
  format; they cannot be used as arrays or data frames. As of now, it 
  will also accept numerical values only. As such, any string variables
  must be converted to numerical ones. One possible way to do so is to
  use data transformation functions, such as from the 'pmmlTransformations' 
  package. However the result is a data frame. In all cases, lists, arrays 
  and data frames can be converted to a matrix format using the data.matrix
  function from the base package. Given a data frame df, a matrix m can 
  thus be created by using m <- data.matrix(df).

  The PMML language requires variable names which will be read in as the 
  column names of the input matrix. If the matrix does not have variable 
  names, they will be given the default values of "X1", "X2", ...  
 
  PMML is an XML based language which
  provides a way for applications to define statistical and data mining
  models and to share models between PMML compliant applications.  More
  information about PMML and the Data Mining Group can be found at
  http://www.dmg.org.  

  Use of PMML and \code{pmml.cv.glmnet} requires the \pkg{XML} package.  Be
  aware that XML is a very verbose data format. 
  
  This function is used to export the structure of a general regression
  model to any PMML consuming applications, such as the Zementis ADAPA and UPPI 
  scoring engines which allow for predictive models built in R to be deployed and 
  executed on site, in the cloud (Amazon, IBM, and FICO), in-database (IBM Netezza, 
  Pivotal, Sybase IQ, Teradata and Teradata Aster) or Hadoop (Datameer and Hive). 
  
}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package.  This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\references{

  T. Hastie, R. Tibshirani, J. Friedman (2008), \emph{The Elements of Statistical
      Learning}. Springer Series in Statistics

  PMML home page: \url{http://www.dmg.org}

  A. Guazzelli, W. Lin, T. Jena (2012), \emph{PMML in Action: Unleashing the Power 
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace                
     (Second Edition) - Available on Amazon.com - \url{http://www.amazon.com/dp/1470003244}.

  A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), \emph{PMML: An Open Standard for 
     Sharing Models}. The R journal, Volume 1/1, 60-65

}
\author{\email{info@zementis.com}}	
\seealso{
  \code{\link{pmml}}.
}
\examples{

library(glmnet)

# create a simple predictor (x) and response(y) matrices
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)

# Build a simple gaussian model
model1 = cv.glmnet(x,y)
# Output the model in PMML format
pmml(model1)

# shift y between 0 and 1 to create a poisson response
y = y - min(y)
# give the predictor variables names (default values are V1,V2,...)
name <- NULL
for(i in 1:20){
  name <- c(name,paste("variable",i,sep=""))
}
colnames(x) <- name
# create a simple poisson model
model2 <- cv.glmnet(x,y,family="poisson")
# output in PMML format the regression model at the lambda parameter = 0.006
pmml(model2,s=0.006)

}
\keyword{interface}
\keyword{glm}
