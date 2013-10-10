\name{audit}
\docType{data}
\alias{audit}
\title{Artificially constructed dataset}
\description{
    This is an artificial dataset consisting of fictional clients who have 
    been audited, perhaps for tax refund compliance. For each case an outcome 
    is recorded (whether the taxpayer's claims had to be adjusted or not) and 
    any amount of adjustment that resulted is also recorded.
}
\format{A data frame containing:
    \tabular{ll}{
        Age        \tab Numeric \cr
        Employment \tab Categorical string with 7 levels \cr 
        Education  \tab Categorical string with 16 levels \cr
        Marital    \tab Categorical string with 6 levels \cr
        Occupation \tab Categorical string with 14 levels \cr
        Income     \tab Numeric \cr
        Sex        \tab Categorical string with 2 levels \cr
        Deductions \tab Numeric \cr
	Hours      \tab Numeric \cr
	Accounts   \tab Categorical string with 32 levels \cr
	Adjustment \tab Numeric \cr
	Adjusted   \tab Numeric value 0 or 1 \cr
    }
  }

\references{
  Togaware rattle package : \emph{\bold{Audit dataset}} \cr
  \url{http://www.dmg.org/pmml_examples/index.html#Audit}
}

\examples{data(audit, package = "pmml")}
\keyword{datasets}

