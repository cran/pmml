\name{pmml.nnet}
\alias{pmml.nnet}
\title{
  Generate PMML for a nnet object
}
\description{
  
  Generate the PMML representation for a nnet object (Neural Network). 
  The nnet object is converted into a PMML representation. The PMML 
  can then be imported into other systems that accept PMML. 
  With this code, a PMML representation can be obtained for Neural
  Networks implementing classification (multi-class and binary) as well
  as regression.
  
}
\usage{
\method{pmml}{nnet}(model, model.name="NeuralNet_model", app.name="Rattle/PMML",
     description="Neural Network PMML Model", copyright=NULL, \dots)
}
\arguments{
  
  \item{model}{a nnet object.}
  
  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The generated PMML can be imported into any PMML consuming
  application that recognizes PMML 3.2. An example is ADAPA. 
  ADAPA (Adaptive Decision and Predictive Analytics) is a lightweight 
  decision engine that offers at its core batch and real-time scoring 
  of predictive models as well as fast execution of business rules. 
  ADAPA supports an extensive collection of PMML elements, including the 
  following predictive techniques: 1) Neural Networks (Backprogagation 
  and Neural Gas); 2) Support Vector Machines; 3) Linear and Logistic 
  Regression as well as all general regression PMML models: a) Multinomial 
  Logistic; b) General Linear; 3) Ordinal Multinomial; 4) Simple Regression; 
  and 5) Generalized Linear Model. ADAPA  provides a reliable and fast 
  way to manage, deploy, and execute a multitude of models and decision 
  strategies.

}
\references{

  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

  Zementis' useful PMML convert: \url{http://www.zementis.com/pmml_converters.htm}
  
  ADAPA home page: \url{http://www.zementis.com/adapa.htm}

}
\author{\email{info@zementis.com}}
\seealso{
  \code{\link{pmml}}.
  \code{\link{nnet}}.
}
\keyword{interface}
