This is a **modified** version of the pmml package that brings some fixes to the [PMML cran package](https://cran.r-project.org/web/packages/pmml/index.html) under GPL licence. 

This version is still under development. Use at your own risk

To install the package use devtools: 

```{r}
devtools::install_github("guleatoma/pmml")
```

The package currently brings fixes targeted to the specific use cases listed below.

# Modifications brought to the package:

## Nested data transformations 

In the regular package, having nested data transformations would result in a failure:

Example: 
```{r}
dataBox = WrapData(reference_index)

dataBox <- FunctionXform(dataBox,
                           origFieldName="x1,x2",
                           newFieldName="x12",
                           formulaText="x1 + x2")

dataBox <- FunctionXform(dataBox,
                       origFieldName="x12,x2",
                       newFieldName="x122",
                       formulaText="x12 + x2")                         
```
would result in a failed transformation to PMML, because the regular package will list in the MiningSchema "x1,x2" and "x2" instead of "x1" and "x2".


## Important note on FunctionXform
`FunctionXform` can bee found in the [pmmlTransformations cran package](https://cran.r-project.org/web/packages/pmmlTransformations/index.html). This function comes with a small error that prevents you from applying FunctionXform several times to dataBox.

So make sure you download the fixed version:

```{r}
devtools::install_github("guleatoma/pmmlTransformations")
```

## Replacement values for missing values

The regulat package would allow to specify a unique replacement value, this package extends this feature to allow to specify a different replacement value for each variable.

```{r}
pmml.glm(glm, transforms = dataBox, unknownValue = list("x1" = 0, "x2" = 100))
```

