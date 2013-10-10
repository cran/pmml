\name{pmml.cv.glmnet}
\alias{pmml.cv.glmnet}
\title{Generate PMML for glmnet objects}
\description{
  
  Generate the PMML representation for a glmnet (elasticnet general linear regression) object. In particular,
  this gives the PMML representation for an object created by the cv.glmnet 
  function. 

}
\usage{
\method{pmml}{cv.glmnet}(model, model.name="Elasticnet_Model", app.name="Rattle/PMML",
     description="Generalized Linear Regression Model", copyright=NULL, 
     transforms=NULL, unknownValue=NULL, dataset=NULL, s=NULL, \dots)
}
\arguments{
  
  \item{model}{a \code{cv.glmnet} object contained in an object of class
      \pkg{glmnet}, as contained in the object returned by the function 
      \code{cv.glmnet}.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via package \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}

  \item{dataset}{the dataset using which the model was built.}

  \item{s}{the 'lambda' parameter at which to output the model. If not given,
	    the lambda.min parameter from the model is used instead.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The \code{glmnet} package expects the input and predicted values in a matrix 
  format; not as arrays or data frames. As of now, it 
  will also accept numerical values only. As such, any string variables
  must be converted to numerical ones. One possible way to do so is to
  use data transformation functions, such as from the \pkg{pmmlTransformations} 
  package. However the result is a data frame. In all cases, lists, arrays 
  and data frames can be converted to a matrix format using the data.matrix
  function from the base package. Given a data frame df, a matrix m can 
  thus be created by using m <- data.matrix(df).

  The PMML language requires variable names which will be read in as the 
  column names of the input matrix. If the matrix does not have variable 
  names, they will be given the default values of "X1", "X2", ...  
 
  Use of PMML and \code{pmml.cv.glmnet} requires the \pkg{XML} package.  Be
  aware that XML is a very verbose data format. 

}    

\references{
  R project CRAN package:\cr
  \emph{\bold{glmnet}: Lasso and elastic-net regularized generalized linear models} \cr
  \url{http://cran.r-project.org/web/packages/glmnet/index.html}
}

\author{Zementis Inc. \email{info@zementis.com}}	

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
