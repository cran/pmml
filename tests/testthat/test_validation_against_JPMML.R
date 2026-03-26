test_that("validation against JPMML evaluator works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("jpmml")

  library("jpmml")

  # Test against the JPMML Evaluator
  # https://github.com/jpmml/jpmml-evaluator-r
  #
  # Installation:
  # devtools::install_github("jpmml/jpmml-evaluator-r")

  ### lm
  data(iris)
  fit <- lm(Sepal.Length ~ ., data = iris)
  fit_pmml <- pmml(fit)
  save_pmml(fit_pmml, "model.pmml")

  evaluator <- newLoadingModelEvaluatorBuilder() |>
    loadFile("model.pmml") |>
    build()

  evaluator <- evaluator |>
    verify()

  val_R <- unname(predict(fit, as.list(iris[1, ])))

  arguments <- as.list(iris[1, ])
  arguments$Species <- as.character(arguments$Species)

  val_JPMML <- evaluator |>
    evaluate(arguments)

  expect_equal(val_R, val_JPMML$Predicted_Sepal.Length)

  ## Add tests for all other models supported by JPMML

  message("TODO: Implement more tests")

  ### cleanup
  file.remove("model.pmml")
})
