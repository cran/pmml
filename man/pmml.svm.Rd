\name{pmml.svm}
\alias{pmml.svm}
\title{Generate PMML for svm objects}

\description{
  Generate the PMML representation of a svm object from the \pkg{e1071} package.
}

\usage{
\method{pmml}{svm}(model, model.name="LIBSVM_Model",
      app.name="R-PMML",
      description="Support Vector Machine Model",
      copyright=NULL, transforms=NULL, 
      unknownValue=NULL, ...)
}

\arguments{
  \item{model}{a \code{svm} object from package \pkg{e1071}.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
      
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
 
  The model is represented in the PMML SupportVectorMachineModel format.
  
  Note that the sign of the coefficient of each support vector flips between the R object
  and the exported PMML file. This is due to the minor difference in the training/scoring
  formula between the LIBSVM algorithm and the DMG specification. Hence the output value of each
  support vector machine has a sign flip between the DMG definition and the svm prediction function.
  
  In a classification model, even though the output of the support vector machine has
  a sign flip, it does not affect the final predicted category. This is because in the 
  DMG definition, the winning category is defined as the left side of threshold 0 while 
  the LIBSVM defines the winning category as the right side of threshold 0.
  
  For a regression model, the exported PMML code has two OutputField elements. The first OutputField
  "predictedValue" shows the support vector machine output per DMG definition. The second one
  "svm_predict_function" gives the value corresponding to the R predict function for the svm
  model. This output should be the one to use when making model predictions.
}
  
\references{
   \itemize{
    \item
       R project CRAN package:\cr
       \emph{\bold{e1071}: Misc Functions of the Department of Statistics (e1071), TU Wien} \cr
       \url{https://CRAN.R-project.org/package=e1071}  
 
    \item
      Chang, Chih-Chung and Lin, Chih-Jen, \emph{LIBSVM: a library for Support Vector Machines}\cr
      \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}
   }      
}

\author{Zementis Inc.}

\examples{

library(e1071)
fit <- svm(Species ~ ., data=iris, kernel="polynomial")
pmml(fit)

rm(fit)

}
