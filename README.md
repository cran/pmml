
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/logo.png" align="right" height="139" /> R package pmml - Generate PMML for Various Models

[![Package on
CRAN](https://www.r-pkg.org/badges/version/pmml)](https://CRAN.R-project.org/package=pmml)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/pmml)](https://CRAN.R-project.org/package=pmml)
![License](https://img.shields.io/cran/l/pmml)
[![Anaconda.org](https://anaconda.org/conda-forge/r-pmml/badges/version.svg)](https://anaconda.org/conda-forge/r-pmml)
[![r-universe
status](https://mhahsler.r-universe.dev/badges/pmml)](https://mhahsler.r-universe.dev/pmml)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-pmml%20R-orange.svg)](https://stackoverflow.com/questions/tagged/pmml%20R)

**This is the official home for the R package pmml (starting with
version pmml_2.6.0).** Previous versions can be found
[here.](https://github.com/Cumulocity-IoT/r-pmml)

## Overview

The pmml package lets you train machine learning and statistical models
in R and then export them using the [Predictive Model Markup Language
(PMML)](https://en.wikipedia.org/wiki/Predictive_Model_Markup_Language).
Many commercial and open data mining platforms support PMML to exchange
models. This includes Python’s scikit learn, Java, IBM SPSS Modeler,
KNIME, Microsoft SQL Server, and SAS Enterprise Miner (see [complete
list](https://dmg.org/pmml/products.html)).

Supported models include:

- Anomaly Detection
- Association Rules
- Clustering
- K Nearest Neighbors
- Linear Models
- Naive Bayes Classifiers
- Neural Networks
- Support Vector Machines
- Time Series
- Tree-based Models and Ensembles
- Survival analysis models

Supported packages: [ada](https://CRAN.R-project.org/package=ada),
[amap](https://CRAN.R-project.org/package=amap),
[arules](https://CRAN.R-project.org/package=arules),
[e1071](https://CRAN.R-project.org/package=e1071),
[forecast](https://CRAN.R-project.org/package=forecast),
[gbm](https://CRAN.R-project.org/package=gbm),
[glmnet](https://CRAN.R-project.org/package=glmnet),
[isofor](https://github.com/gravesee/isofor),
[kernlab](https://CRAN.R-project.org/package=kernlab),
[neighbr](https://CRAN.R-project.org/package=neighbr), stats,
[nnet](https://CRAN.R-project.org/package=nnet),
[randomForest](https://CRAN.R-project.org/package=randomForest),
[rpart](https://CRAN.R-project.org/package=rpart),
[survival](https://CRAN.R-project.org/package=survival),
[xgboost](https://CRAN.R-project.org/package=xgboost)

For a description of the supported packages, see the vignette:
[Supported Packages and Additional
Functions](https://mhahsler.r-universe.dev/articles/pmml/packages_and_functions.html).

To cite package ‘pmml’ in publications use:

> Hahsler M, Rodrigues B, Bolotov D, Jena T, Williams G, Lin W, Ishwaran
> H, Kogalur U, Guha R (2026). *pmml: Generate PMML for Various Models*.
> R package version 2.6.1, <https://github.com/mhahsler/r-pmml>.

    @Manual{,
      title = {pmml: Generate PMML for Various Models},
      author = {Michael Hahsler and Bruno Rodrigues and Dmitriy Bolotov and Tridivesh Jena and Graham Williams and Wen-Ching Lin and Hemant Ishwaran and Udaya B. Kogalur and Rajarshi Guha},
      year = {2026},
      note = {R package version 2.6.1},
      url = {https://github.com/mhahsler/r-pmml},
    }

## Installation

**Stable CRAN version:** Install from within R with

``` r
install.packages("pmml")
```

**Current development version:** Install from
[r-universe.](https://mhahsler.r-universe.dev/pmml)

``` r
install.packages("pmml",
    repos = c("https://mhahsler.r-universe.dev",
              "https://cloud.r-project.org/"))
```

## Example

``` r
library(pmml)

# Build an lm model
iris_lm <- lm(Sepal.Length ~ ., data=iris)

# Convert to pmml
iris_lm_pmml <- pmml(iris_lm)

# The PMML model
iris_lm_pmml
#> <PMML version="4.4.1" xmlns="http://www.dmg.org/PMML-4_4" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.dmg.org/PMML-4_4 http://www.dmg.org/pmml/v4-4/pmml-4-4.xsd">
#>  <Header copyright="Copyright (c) 2026 mhahsler" description="Linear Regression Model">
#>   <Extension name="user" value="mhahsler" extender="R PMML Generator - Package pmml"/>
#>   <Application name="R PMML Generator - Package pmml" version="2.6.1"/>
#>   <Timestamp>2026-07-09 13:23:15.792675</Timestamp>
#>  </Header>
#>  <DataDictionary numberOfFields="5">
#>   <DataField name="Sepal.Length" optype="continuous" dataType="double"/>
#>   <DataField name="Sepal.Width" optype="continuous" dataType="double"/>
#>   <DataField name="Petal.Length" optype="continuous" dataType="double"/>
#>   <DataField name="Petal.Width" optype="continuous" dataType="double"/>
#>   <DataField name="Species" optype="categorical" dataType="string">
#>    <Value value="setosa"/>
#>    <Value value="versicolor"/>
#>    <Value value="virginica"/>
#>   </DataField>
#>  </DataDictionary>
#>  <RegressionModel modelName="lm_Model" functionName="regression" algorithmName="least squares">
#>   <MiningSchema>
#>    <MiningField name="Sepal.Length" usageType="predicted" invalidValueTreatment="returnInvalid"/>
#>    <MiningField name="Sepal.Width" usageType="active" invalidValueTreatment="returnInvalid"/>
#>    <MiningField name="Petal.Length" usageType="active" invalidValueTreatment="returnInvalid"/>
#>    <MiningField name="Petal.Width" usageType="active" invalidValueTreatment="returnInvalid"/>
#>    <MiningField name="Species" usageType="active" invalidValueTreatment="returnInvalid"/>
#>   </MiningSchema>
#>   <Output>
#>    <OutputField name="Predicted_Sepal.Length" optype="continuous" dataType="double" feature="predictedValue"/>
#>   </Output>
#>   <RegressionTable intercept="2.17126629215507">
#>    <NumericPredictor name="Sepal.Width" exponent="1" coefficient="0.495888938388551"/>
#>    <NumericPredictor name="Petal.Length" exponent="1" coefficient="0.829243912234806"/>
#>    <NumericPredictor name="Petal.Width" exponent="1" coefficient="-0.315155173326473"/>
#>    <CategoricalPredictor name="Species" value="setosa" coefficient="0"/>
#>    <CategoricalPredictor name="Species" value="versicolor" coefficient="-0.72356195778073"/>
#>    <CategoricalPredictor name="Species" value="virginica" coefficient="-1.02349781449083"/>
#>   </RegressionTable>
#>  </RegressionModel>
#> </PMML>

# Write to file: 
# save_pmml(iris_lm_pmml,"iris_lm.pmml")
```

## Related Packages

### Java

The Java library [JMML](https://github.com/jpmml) can be used to read
and execute models stored in PMML format. An R interface to the library
to create PMML models is called
[r2pmml](https://CRAN.R-project.org/package=r2pmml) and available from
CRAN. An R interface for the evaluator, called
[jpmml](https://github.com/jpmml/jpmml-evaluator-r), can be installed
from Github.

### Python

[sklearn-pmml-model](https://github.com/iamDecode/sklearn-pmml-model)
can import models trained on R into Python’s scikit-learn framework.

## References

- [DMG PMML 4.4.1
  specification](https://dmg.org/pmml/v4-4-1/GeneralStructure.html)
