\name{pmml.glm}
\alias{pmml.glm}
\title{Generate PMML for glm objects}
\description{
  Generate the PMML representation for a glm object from package \pkg{stats}.
}
\usage{
\method{pmml}{glm}(model, model.name="General_Regression_Model",
      app.name="Rattle/PMML",
      description="Generalized Linear Regression Model",
      copyright=NULL, transforms=NULL, unknownValue=NULL,
      weights=NULL, ...)
}
\arguments{
  
  \item{model}{a \code{glm} object.}

  \item{model.name}{a name to be given to the model in the PMML code.}

  \item{app.name}{the name of the application that generated the PMML code.}

  \item{description}{a descriptive text for the Header element of the PMML code.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{data transformations represented in PMML via \pkg{pmmlTransformations}.}

  \item{unknownValue}{value to be used as the 'missingValueReplacement' attribute for all MiningFields.}
  
  \item{weights}{the weights used for building the model.}

  \item{\dots}{further arguments passed to or from other methods.}
}

\details{  
  The function exports the glm model in the PMML GeneralRegressionModel format.
  
  Note on glm models for 2-class problems: a dataset where the target categorical variable has more than 2 classes may be turned into a 2-class problem by creating a new target variable that is TRUE for a particular class and FALSE for all other classes. While the R formula function allows such a transformation to be passed directly to it, this may cause issues when the model is converted to PMML. Therefore, it is advised to create a new 2-class separately, and then pass that variable to glm(). This is shown in an example below.
}    


\references{
  \href{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}{R project: \emph{\bold{Fitting Generalized Linear Models}}}

}

\author{Zementis Inc.}

\examples{
data(iris)
mod <- glm(Sepal.Length ~ ., data=iris, family = "gaussian")
mod_pmml <- pmml(mod)
rm(mod,mod_pmml)

data(audit)
mod <- glm(Adjusted ~ Age + Employment + Education + Income, data=audit,family=binomial(logit))
mod_pmml <- pmml(mod)
rm(mod,mod_pmml)

##  creating a new 2-class target from a 3-class variable
data(iris)
dat <- iris[,1:4]
# add a new 2-class target "Species_setosa" before passing it to glm()
dat$Species_setosa <- iris$Species=="setosa"
mod <- glm(Species_setosa ~ ., data=dat, family=binomial(logit))
mod_pmml <- pmml(mod)
rm(dat,mod,mod_pmml)
}

