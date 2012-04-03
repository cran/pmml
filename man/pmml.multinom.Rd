\name{pmml.multinom}
\alias{pmml.multinom}
\title{Generate PMML for a multinomial (multinom) object}
\description{
  
  Generate the Predictive Model Markup Language (PMML) representation of
  a \pkg{nnet} multinomial object.  In particular, this function 
  gives the user the ability to save a multinomial regression object 
  as a PMML XML document.

}
\usage{
\method{pmml}{multinom}(model, model.name="multinom_Model", app.name="Rattle/PMML",
     description="Multinomial Logistic Model", copyright = NULL, transforms = NULL, ...)
}
\arguments{
  
  \item{model}{the \code{multinom} object contained in an object of class
      \pkg{nnet}, as that contained in the object returned by
      the function \code{multinom}.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{transforms}{transforms.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The Predictive Model Markup Language is an XML based language which
  provides a way for applications to define statistical and data mining
  models and to share models between PMML compliant applications.  More
  information about PMML and the Data Mining Group can be found at
  http://www.dmg.org.  

  Use of PMML and \code{pmml.multinom} requires the \pkg{XML} package.  Be
  aware that XML is a very verbose data format.  Reasonably sized trees
  and data sets can lead to extremely large text files.  XML, while
  achieving interoperability, is not an efficient data storage mechanism
  in this case.

  This function is used to export the structure of the multinomial logistoc 
  regression model to other PMML compliant applications. 

}    
\value{
  
  An object of class \code{XMLNode} as that defined by the \pkg{XML}
  package.  This represents the top level, or root node, of the XML
  document and is of type PMML. It can be written to file with
  \code{saveXML}.
  
}
\references{

  Venables, W.N. and Ripley, B.D. (2002) /emph{Modern Applied Statistics with S}.
					Fourth edition. Springer.

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
\keyword{interface}
\keyword{tree}
