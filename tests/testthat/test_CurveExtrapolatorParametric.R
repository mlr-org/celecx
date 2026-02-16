# =============================================================================
# Tests for CurveExtrapolatorParametric
# =============================================================================

create_parametric_history <- function(n = 30L) {
  set.seed(1)
  x <- seq_len(n)
  y <- 1 / sqrt(x) + rnorm(n, sd = 0.01)
  data.table(n_evals = x, metric = y)
}


test_that("CurveExtrapolatorParametric trains and predicts", {
  history <- create_parametric_history(30L)

  extrapolator <- CurveExtrapolatorParametric$new(n_bootstrap = 50L)
  extrapolator$train(history)

  prediction <- extrapolator$predict(30:40)

  expect_data_table(prediction, nrows = 11)
  expect_true(all(prediction$q05 <= prediction$q50))
  expect_true(all(prediction$q50 <= prediction$q95))
  expect_true(all(is.finite(prediction$mean)))
})


test_that("CurveExtrapolatorParametric predict_target works", {
  history <- create_parametric_history(25L)

  extrapolator <- CurveExtrapolatorParametric$new(direction = "minimize", n_bootstrap = 40L)
  extrapolator$train(history)

  stats <- extrapolator$predict_target(target = 0.15, n_evals_budget = 40L)

  expect_true(is.list(stats))
  expect_true(all(c("p_reach_budget", "n_evals_q50", "n_evals_q90") %in% names(stats)))
  expect_true(is.numeric(stats$p_reach_budget))
})


test_that("CurveExtrapolatorParametric validates family names", {
  expect_error(
    CurveExtrapolatorParametric$new(families = c("inv_x", "unknown_family")),
    "elements"
  )
})
