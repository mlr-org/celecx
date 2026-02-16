# =============================================================================
# Tests for forecast_retro()
# =============================================================================

test_that("forecast_retro trains extrapolator clone and returns outputs", {
  metrics_data <- data.table(
    n_evals = 1:20,
    best_y = 1 / sqrt(1:20)
  )

  extrapolator <- CurveExtrapolatorParametric$new(n_bootstrap = 40L)

  result <- forecast_retro(
    metrics_data = metrics_data,
    extrapolator = extrapolator,
    metric_name = "best_y",
    x_col = "n_evals",
    target = 0.2,
    n_evals_budget = 30L
  )

  expect_type(result, "list")
  expect_true(all(c("extrapolator", "prediction", "target_stats") %in% names(result)))
  expect_r6(result$extrapolator, "CurveExtrapolator")
  expect_data_table(result$prediction)
  expect_true(result$extrapolator$is_trained)

  # Original object should remain untouched.
  expect_false(extrapolator$is_trained)
})
