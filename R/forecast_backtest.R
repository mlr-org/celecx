#' @title Forecast Backtesting
#'
#' @description
#' Evaluates forecast quality by repeatedly training on run prefixes and
#' predicting held-out future points.
#'
#' @param metrics_data (`data.table`)
#'   Metrics history ordered by progression.
#' @param extrapolator (`CurveExtrapolator`)
#'   Extrapolator used for backtesting.
#' @param metric_name (`character(1)`)
#'   Metric column in `metrics_data`.
#' @param x_col (`character(1)`)
#'   Evaluation-count column in `metrics_data`.
#' @param min_prefix (`integer(1)`)
#'   Minimum prefix length.
#' @param horizon (`integer(1)`)
#'   Horizon in rows (not absolute n_evals units).
#' @param target (`numeric(1)` | `NULL`)
#'   Optional target for probability backtests.
#' @param ...
#'   Extra arguments passed to `extrapolator$train()`.
#'
#' @return [data.table::data.table()] with one row per prefix/horizon pair.
#'
#' @export
forecast_backtest <- function(metrics_data,
    extrapolator,
    metric_name,
    x_col = "n_evals",
    min_prefix = 5L,
    horizon = 1L,
    target = NULL,
    ...) {
  assert_data_table(metrics_data, min.rows = 3L)
  assert_r6(extrapolator, "CurveExtrapolator")
  assert_string(metric_name)
  assert_string(x_col)
  assert_int(min_prefix, lower = 2L)
  assert_int(horizon, lower = 1L)
  assert_number(target, null.ok = TRUE)
  assert_names(names(metrics_data), must.include = c(metric_name, x_col))

  n_total <- nrow(metrics_data)
  if (n_total < (min_prefix + horizon)) {
    stopf("Need at least min_prefix + horizon rows (%i)", min_prefix + horizon)
  }

  rows <- list()
  row_id <- 1L

  for (prefix_end in seq.int(min_prefix, n_total - horizon)) {
    actual_idx <- prefix_end + horizon

    prefix_dt <- copy(metrics_data[seq_len(prefix_end), c(x_col, metric_name), with = FALSE])
    setnames(prefix_dt, c(x_col, metric_name), c(extrapolator$x_col, extrapolator$metric_col))

    model <- extrapolator$clone(deep = TRUE)
    train_args <- c(list(history = prefix_dt), list(...))
    do.call(model$train, train_args)

    n_eval_target <- as.integer(metrics_data[[x_col]][[actual_idx]])
    actual_metric <- as.numeric(metrics_data[[metric_name]][[actual_idx]])

    prediction <- model$predict(n_eval_target)
    pred_mean <- prediction$mean[[1L]]
    pred_q50 <- prediction$q50[[1L]]
    pred_q05 <- prediction$q05[[1L]]
    pred_q95 <- prediction$q95[[1L]]

    row <- data.table(
      prefix_end = as.integer(prefix_end),
      horizon = as.integer(horizon),
      n_evals_prefix = as.integer(metrics_data[[x_col]][[prefix_end]]),
      n_evals_target = n_eval_target,
      actual = actual_metric,
      pred_mean = pred_mean,
      pred_q50 = pred_q50,
      pred_q05 = pred_q05,
      pred_q95 = pred_q95,
      error_mean = pred_mean - actual_metric,
      abs_error_mean = abs(pred_mean - actual_metric),
      error_q50 = pred_q50 - actual_metric,
      abs_error_q50 = abs(pred_q50 - actual_metric)
    )

    if (!is.null(target)) {
      target_stats <- model$predict_target(target = target, n_evals_budget = n_eval_target)
      segment <- metrics_data[(prefix_end + 1L):actual_idx]
      reached_actual <- if (model$direction == "minimize") {
        any(segment[[metric_name]] <= target)
      } else {
        any(segment[[metric_name]] >= target)
      }

      row[, `:=`(
        p_reach_budget = target_stats$p_reach_budget,
        reached_actual = reached_actual
      )]
    }

    rows[[row_id]] <- row
    row_id <- row_id + 1L
  }

  rbindlist(rows, use.names = TRUE, fill = TRUE)
}
