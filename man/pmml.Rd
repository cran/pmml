\name{pmml}
\alias{pmml}
\title{Generate PMML for R objects}
\description{
  
  \code{pmml} is a generic function implementing S3 methods used to produce
  the PMML (Predictive Model Markup Language) representation of an R
  model. The resulting PMML file can then be imported into other systems that accept
  PMML. 
  
  The same function can also be used to output variable transformations
  in PMML format. In particular, it can be used as a transformations generator. Various 
  transformation operations can be implemented in R and those transformations
  can then be output in PMML format by calling the function with a NULL value 
  for the model input and a \code{pmmlTransformations} object as the transforms 
  input. Please see the R \pkg{pmmlTransformations} package for more information
  on how to create the \code{pmmlTransformations} object.

  In addition, the \code{pmml} function can also be called using a pre-existing PMML model
  as the first input and a \code{pmmlTransformations} object as the transforms input. 
  The result is a new PMML model with the transformation inserted as 
  a "LocalTransformations" element in the original model. If the original model
  already had a "LocalTransformations" element, the new information will be 
  appended to that element. If the model variables are derived directly from  
  a chain of transformations defined in the transforms input, the 
  field names in the model are replaced with the original field names with the
  correct data types to make a consistent model. The covered cases include
  model fields derived from an original field, model fields derived from a chain 
  of transformations starting from an original field and mutiple fields derived from 
  the same original field. 
  
  Please note that package \pkg{XML_3.95-0.1} or later is required to perform the
  full and correct functionality of the \pkg{pmml} package.
  
}


\usage{
pmml(model, model.name="Rattle_Model", app.name="Rattle/PMML",
     description=NULL, copyright=NULL, transforms=NULL, \dots)
}

\arguments{
  
  \item{model}{an object to be converted to PMML.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{a coded list of transforms performed.}

  \item{\dots}{further arguments passed to or from other methods.}

}
\details{

  PMML is an XML based language which
  provides a way for applications to define statistical and data mining
  models and to share models between PMML compliant applications. More
  information about PMML and the Data Mining Group can be found at
  http://www.dmg.org.  
  
  The generated PMML can be imported into any PMML consuming
  application, such as the Zementis ADAPA and UPPI scoring engines which allow for
  predictive models built in R to be deployed and executed on site, in the cloud 
  (Amazon, IBM, and FICO), in-database (IBM Netezza, Pivotal, Sybase IQ, Teradata and 
  Teradata Aster) or Hadoop (Datameer and Hive). 

}

\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package.  This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}

\references{

 \itemize{
    \item
       Rattle home page: \url{http://rattle.togaware.com}

     \item
      PMML home page: \url{http://www.dmg.org}
    
      \item
       A. Guazzelli, W. Lin, T. Jena (2012), \emph{PMML in Action: Unleashing the Power  
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace 
     (Second Edition) - Available on Amazon.com \url{http://www.amazon.com/dp/1470003244}.
    
     \item
     A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), PMML: An Open Standard for 
     Sharing Models. \emph{The R journal}, Volume 1/1, 60-65
     
     \item
     A. Guazzelli, T. Jena, W. Lin, M. Zeller (2013). Extending the Naive Bayes Model Element
     in PMML: Adding Support for Continuous Input Variables. 
     In \emph{Proceedings of the 19th ACM SIGKDD Conference on Knowledge Discovery 
     and Data Mining} \cr 
     \url{http://kdd13pmml.files.wordpress.com/2013/07/guazzelli_et_al.pdf}
     
     \item
     T. Jena, A. Guazzelli, W. Lin, M. Zeller (2013). The R pmmlTransformations Package. 
     In \emph{Proceedings of the 19th ACM SIGKDD Conference on Knowledge Discovery 
     and Data Mining} \cr 
     \url{http://kdd13pmml.files.wordpress.com/2013/07/jena_et_al.pdf}
  }
}

\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml.ada}},
  \code{\link{pmml.rules}},
  \code{\link{pmml.coxph}},
  \code{\link{pmml.cv.glmnet}},
  \code{\link{pmml.glm}},
  \code{\link{pmml.hclust}},
  \code{\link{pmml.kmeans}},
  \code{\link{pmml.ksvm}},
  \code{\link{pmml.lm}},
  \code{\link{pmml.multinom}},
  \code{\link{pmml.naiveBayes}},
  \code{\link{pmml.nnet}},
  \code{\link{pmml.randomForest}},
  \code{\link{pmml.rfsrc}},
  \code{\link{pmml.rpart}},
  \code{\link{pmml.svm}}
}
\examples{
# Build a simple lm model
(iris.lm <- lm(Sepal.Length ~ ., data=iris))

# Convert to pmml
pmml(iris.lm)

# Create a pmmlTransformations object
library(pmmlTransformations)
xo <- WrapData(iris)

# Transform the 'Sepal.Length' variable
xo <- MinMaxXform(xo,xformInfo="column1->d_sl")

# Output the tranformation in PMML format
pmml(NULL, transforms=xo)

}
