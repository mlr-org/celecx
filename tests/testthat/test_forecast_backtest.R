# =============================================================================
# Tests for forecast_backtest()
# =============================================================================

test_that("forecast_backtest returns expected columns", {
  metrics_data <- data.table(
    n_evals = 1:25,
    best_y = 1 / sqrt(1:25)
  )

  result <- forecast_backtest(
    metrics_data = metrics_data,
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    x_col = "n_evals",
    min_prefix = 5L,
    horizon = 2L
  )

  expect_data_table(result)
  expect_true(nrow(result) > 0L)
  expect_names(names(result), must.include = c(
    "prefix_end",
    "horizon",
    "actual",
    "pred_mean",
    "pred_q50",
    "abs_error_mean",
    "abs_error_q50"
  ))
  expect_true(all(is.finite(result$abs_error_mean)))
})


test_that("forecast_backtest includes target diagnostics when requested", {
  metrics_data <- data.table(
    n_evals = 1:20,
    best_y = 1 / sqrt(1:20)
  )

  result <- forecast_backtest(
    metrics_data = metrics_data,
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    x_col = "n_evals",
    min_prefix = 5L,
    horizon = 1L,
    target = 0.2
  )

  expect_true(all(c("p_reach_budget", "reached_actual") %in% names(result)))
})
