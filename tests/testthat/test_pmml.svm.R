data("audit")

# teardown({
#   detach("package:e1071", unload = TRUE)
# })

test_that("error when dataset is null for one-classification", {
  skip_if_not_installed("e1071")
  library(e1071)

  set.seed(123)
  ft_1 <- runif(100) * 10
  ft_2 <- runif(100) * 7
  df_1 <- data.frame(ft_1, ft_2, stringsAsFactors = TRUE)
  anom_rows <- sample(1:100, 5)
  df_1[anom_rows, 1] <- sample(20:30, 5)
  df_1[anom_rows, 2] <- sample(20:30, 5)

  svm_model_1 <- svm(df_1,
    y = NULL, type = "one-classification",
    nu = 0.10, scale = TRUE, kernel = "radial"
  )


  expect_error(
    pmml(svm_model_1),
    "dataset must not be null for one-classification."
  )
})

test_that("pmml.svm no error when model is one-class svm", {
  skip_if_not_installed("e1071")
  library(e1071)

  set.seed(123)
  ft_1 <- runif(100) * 10
  ft_2 <- runif(100) * 7
  df_1 <- data.frame(ft_1, ft_2, stringsAsFactors = TRUE)
  anom_rows <- sample(1:100, 5)
  df_1[anom_rows, 1] <- sample(20:30, 5)
  df_1[anom_rows, 2] <- sample(20:30, 5)

  svm_model_1 <- svm(df_1,
    y = NULL, type = "one-classification",
    nu = 0.10, scale = TRUE, kernel = "radial"
  )


  expect_silent(pmml(svm_model_1, dataset = df_1))
})

test_that("pmml.svm no error when model is one-class svm", {
  skip_if_not_installed("e1071")
  library(e1071)
  # set.seed(321)
  df_2 <- na.omit(audit)

  df_2 <- df_2[, c("Age", "Income", "Deductions", "Hours", "Adjustment", "Adjusted")]
  df_2$Age <- as.numeric(df_2$Age)
  df_2$Hours <- as.numeric(df_2$Hours)
  df_2$Adjustment <- as.numeric(df_2$Adjustment)
  df_2$Adjusted <- as.numeric(df_2$Adjusted)

  svm_model_2 <- svm(df_2,
    y = NULL, type = "one-classification",
    nu = 0.10, scale = FALSE, kernel = "radial"
  )

  # expect_silent(pmml(svm_model_2,feature.info=sapply(df_2,class)))
  expect_silent(pmml(svm_model_2, dataset = df_2))
})

test_that("pmml.svm error when model is one-class svm and data has integer", {
  skip_if_not_installed("e1071")
  library(e1071)
  # set.seed(311)
  df_3 <- na.omit(audit)
  df_3 <- df_3[, c("Age", "Income", "Deductions", "Hours", "Adjustment", "Adjusted")]
  df_3$Hours <- as.numeric(df_3$Hours)
  df_3$Adjustment <- as.numeric(df_3$Adjustment)
  df_3$Adjusted <- as.numeric(df_3$Adjusted)

  svm_model_3 <- svm(df_3,
    y = NULL, type = "one-classification",
    nu = 0.10, scale = FALSE, kernel = "radial"
  )

  expect_error(
    pmml(svm_model_3, dataset = df_3),
    "Features must be of numeric, and not integer."
  )
})

test_that("pmml.svm error when model is one-class svm and formula interface is used", {
  skip_if_not_installed("e1071")
  library(e1071)
  data(iris)
  df_4 <- iris[, 1:3]
  fit <- svm(Petal.Length ~ ., data = df_4, type = "one-classification")
  expect_error(
    pmml(fit, dataset = df_4),
    "Formula interface not supported for one-class svm. Please use the default S3 method to train."
  )
})

test_that("Output node is formatted correctly for SV regression", {
  skip_if_not_installed("e1071")
  library(e1071)
  data(iris)
  fit <- svm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
  fit_pmml <- pmml(fit)

  expect_equal(toString(fit_pmml[[3]][[2]]), "<Output>\n <OutputField name=\"predictedValue\" feature=\"predictedValue\" dataType=\"double\" optype=\"continuous\"/>\n <OutputField name=\"svm_predict_function\" feature=\"transformedValue\" dataType=\"double\" optype=\"continuous\">\n  <Apply function=\"+\">\n   <Apply function=\"*\">\n    <FieldRef field=\"predictedValue\"/>\n    <Constant>-0.828066127977863</Constant>\n   </Apply>\n   <Constant>5.84333333333333</Constant>\n  </Apply>\n </OutputField>\n</Output>")
})
