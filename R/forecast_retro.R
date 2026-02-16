#' @title Retrospective Forecasting
#'
#' @description
#' Fits a `CurveExtrapolator` on completed run history and returns forecast
#' outputs.
#'
#' @param metrics_data (`data.table`)
#'   Metrics history.
#' @param extrapolator (`CurveExtrapolator`)
#'   Extrapolator to train.
#' @param metric_name (`character(1)`)
#'   Metric column in `metrics_data`.
#' @param x_col (`character(1)`)
#'   Evaluation-count column in `metrics_data`.
#' @param target (`numeric(1)` | `NULL`)
#'   Optional target value.
#' @param n_evals_budget (`integer(1)` | `NULL`)
#'   Optional forecast budget.
#' @param ...
#'   Extra arguments passed to `extrapolator$train()`.
#'
#' @return Named `list()` with entries:
#' - `extrapolator`
#' - `prediction`
#' - `target_stats`
#'
#' @export
forecast_retro <- function(metrics_data,
    extrapolator,
    metric_name,
    x_col = "n_evals",
    target = NULL,
    n_evals_budget = NULL,
    ...) {
  assert_data_table(metrics_data, min.rows = 2L)
  assert_r6(extrapolator, "CurveExtrapolator")
  assert_string(metric_name)
  assert_string(x_col)
  assert_names(names(metrics_data), must.include = c(metric_name, x_col))
  assert_number(target, null.ok = TRUE)
  assert_int(n_evals_budget, lower = 1L, null.ok = TRUE)

  extrapolator <- extrapolator$clone(deep = TRUE)

  history <- copy(metrics_data[, c(x_col, metric_name), with = FALSE])
  setnames(history, c(x_col, metric_name), c(extrapolator$x_col, extrapolator$metric_col))

  train_args <- c(list(history = history), list(...))
  do.call(extrapolator$train, train_args)

  n_current <- as.integer(history[[extrapolator$x_col]][[nrow(history)]])
  n_budget <- n_evals_budget %??% n_current
  n_budget <- max(n_budget, n_current)

  prediction <- extrapolator$predict(seq.int(n_current, n_budget))

  target_stats <- if (is.null(target)) {
    NULL
  } else {
    extrapolator$predict_target(target = target, n_evals_budget = n_budget)
  }

  list(
    extrapolator = extrapolator,
    prediction = prediction,
    target_stats = target_stats
  )
}
