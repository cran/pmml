# Initialisations

library(pmml)

# Simple lm model

iris.lm <- lm(Sepal.Length ~ ., data=iris)
iris.lm.pmml <- pmml(iris.lm)
stopifnot(identical(xmlGetAttr(iris.lm.pmml[[3]][[3]], "intercept"),
                    coefficients(iris.lm)["(Intercept)"][[1]]))

# This example comes from lm which in turn comes from Annette Dobson
# (1990) "An Introduction to Generalized Linear Models".  Page 9:
# Plant Weight Data. The generated PMML misplaced the intercept prior
# to 2.1.9.

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
d90.lm <- lm(weight ~ group - 1)
d90.lm.pmml <- pmml(d90.lm)
stopifnot(identical(xmlGetAttr(d90.lm.pmml[[3]][[3]], "intercept"), 0.0))
