# =============================================================================
# Tests for CurveExtrapolatorGam
# =============================================================================

test_that("CurveExtrapolatorGam trains and predicts", {
  skip_if_not_installed("mgcv")

  set.seed(1)
  n <- 40L
  history <- data.table(
    n_evals = seq_len(n),
    metric = 1 / sqrt(seq_len(n)) + rnorm(n, sd = 0.02)
  )

  extrapolator <- CurveExtrapolatorGam$new(k = 8L)
  extrapolator$train(history)

  prediction <- extrapolator$predict(35:50)

  expect_data_table(prediction, nrows = 16)
  expect_true(all(prediction$q05 <= prediction$q50))
  expect_true(all(prediction$q50 <= prediction$q95))
})


test_that("CurveExtrapolatorGam checks package availability", {
  extrapolator <- CurveExtrapolatorGam$new(packages = "pkg_that_does_not_exist_celecx")
  history <- data.table(n_evals = 1:10, metric = 1 / (1:10))

  expect_error(
    extrapolator$train(history),
    "could not be loaded"
  )
})
