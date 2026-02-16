# =============================================================================
# Tests for CallbackForecastTracker
# =============================================================================

test_that("CallbackForecastTracker updates on SearchInstance context", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$x - 0.3)^2),
    domain = ps(x = p_dbl(lower = 0, upper = 1)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  metrics_tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))
  forecast_tracker <- ForecastTracker$new(
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    min_points = 2L
  )

  callbacks <- list(
    CallbackMetricsTracker$new(metrics_tracker),
    CallbackForecastTracker$new(metrics_tracker, forecast_tracker)
  )

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 6L),
    callbacks = callbacks
  )

  instance$eval_batch(data.table(x = 0.1))
  instance$eval_batch(data.table(x = 0.2))
  instance$eval_batch(data.table(x = 0.4))

  expect_gte(forecast_tracker$n_updates, 1L)
})


test_that("CallbackForecastTracker updates on ContextBatch", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$x - 0.3)^2),
    domain = ps(x = p_dbl(lower = 0, upper = 1)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  metrics_tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))
  forecast_tracker <- ForecastTracker$new(
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    min_points = 2L
  )

  callbacks <- list(
    CallbackMetricsTracker$new(metrics_tracker),
    CallbackForecastTracker$new(metrics_tracker, forecast_tracker)
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5L),
    callbacks = callbacks
  )

  optimizer <- opt("random_search", batch_size = 1L)
  optimizer$optimize(instance)

  expect_gte(forecast_tracker$n_updates, 1L)
})
