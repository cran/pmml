\name{pmml.rpart}
\alias{pmml.rpart}
\title{Generate PMML for an rpart object}
\description{
  
  Generate the PMML (Predictive Model Markup Language) representation of
  an \pkg{rpart} object (classification tree).  The rpart object
  (currently expected to be a classification tree) is converted into a
  PMML representation. The PMML can then be imported into other systems
  that accept PMML.
  
}
\usage{
\method{pmml}{rpart}(model, model.name="RPart_Model", app.name="Rattle/PMML",
     description="RPart decision tree model", copyright=NULL, \dots)
}
\arguments{
  
  \item{model}{an rpart object.}

  \item{model.name}{a name to give to the model in the PMML.}

  \item{app.name}{the name of the application that generated the PMML.}

  \item{description}{a descriptive text for the header of the PMML.}
  
  \item{copyright}{the copyright notice for the model.}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{

  The generated PMML can be imported into any PMML consuming
  application, such as Teradata Warehouse Miner and DB2. Generally,
  these applications convert the PMML into SQL for execution across a
  database.

  Teradata, for example, generates a single SELECT statement to
  implement a decision tree. In the Examples section below, we use the
  rpart example to build a model stored in the variable fit. A segment
  of the PMML for this model is:

  \preformatted{
  <Node score="absent" recordCount="81">
   <True/>
   <Node score="absent" recordCount="62">
    <SimplePredicate field="Start" operator="greaterOrEqual"
                     value="8.5"/>
    <Node score="absent" recordCount="29">
     <SimplePredicate field="Start" operator="greaterOrEqual"
                      value="14.5"/>
    </Node>
    <Node score="absent" recordCount="33">
     <SimplePredicate field="Start" operator="lessThan"
                      value="14.5"/>
     <Node score="absent" recordCount="12">
      <SimplePredicate field="Age" operator="lessThan"
                       value="55"/>
     </Node>
     <Node score="absent" recordCount="21">
      <SimplePredicate field="Age" operator="greaterOrEqual" 
                      value="55"/>
      <Node score="absent" recordCount="14">
       <SimplePredicate field="Age" operator="greaterOrEqual"
                        value="111"/>
      </Node>
      <Node score="present" recordCount="7">
       <SimplePredicate field="Age" operator="lessThan"
                        value="111"/>
      </Node>
     </Node>
    </Node>
   </Node>
   <Node score="present" recordCount="19">
    <SimplePredicate field="Start" operator="lessThan"
                     value="8.5"/>
   </Node>
  </Node>
}

  The resulting SQL from Teradata includes:

  \preformatted{
  CREATE TABLE "MyScores" AS (
    SELECT "UserID",
      (CASE WHEN _node = 0 THEN 'absent'
            WHEN _node = 1 THEN 'absent'
            WHEN _node = 2 THEN 'absent'
            WHEN _node = 3 THEN 'present'
            WHEN _node = 4 THEN 'present'
            ELSE NULL END)
	    (VARCHAR(8)) AS "Kyphosis"
    FROM
      (SELECT "UserID",
        (CASE WHEN ("Start" >= 8.5) AND ("Start" >= 14.5)
              THEN 0
              WHEN ("Start" >= 8.5) AND ("Start" < 14.5)
	      AND ("Age" < 55)
              THEN 1
	      WHEN ("Start" >= 8.5) AND ("Start" < 14.5)
	      AND ("Age" >= 55) AND ("Age" >= 111)
	      THEN 2
	      WHEN ("Start" >= 8.5) AND ("Start" < 14.5)
	      AND ("Age" >= 55) AND ("Age" < 111)
	      THEN 3
	      WHEN ("Start" < 8.5)
	      THEN 4
              ELSE -1 END) AS _node
        FROM "MyData" WHERE _node IS NOT NULL) A
        WHERE "Kyphosis" IS NOT NULL)
    WITH DATA UNIQUE PRIMARY INDEX ("UserID");
  }
}
\references{

  Package home page: \url{http://rattle.togaware.com}

  PMML home page: \url{http://www.dmg.org}

}
\author{\email{Graham.Williams@togaware.com}}
\seealso{
  \code{\link{pmml}}.
}
\examples{
library(rpart)
fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)
pmml.rpart(fit)
}
\keyword{interface}
\keyword{tree}
