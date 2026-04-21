#' @title Run Active Learning
#'
#' @description
#' Convenience function that constructs an active learning [OptimizerAL]
#' via [optimizer_active_learning()], runs it on a bbotk instance, and (optionally)
#' logs metrics via [CallbackMetricsTracker].
#'
#' @param objective ([bbotk::Objective])\cr
#'   Objective to evaluate. Typically has a single codomain target tagged `"learn"`.
#' @param search_space (`NULL` | [paradox::ParamSet])\cr
#'   Optional restricted search space. If `NULL`, the search space is derived from
#'   `objective$domain` (same logic as bbotk's `OptimInstanceBatch`).
#' @param n_evals (`NULL` | `integer(1)`)\cr
#'   Convenience evaluation budget used only if `terminator` is `NULL`.
#' @param terminator (`NULL` | [bbotk::Terminator])\cr
#'   Terminator for the outer active learning loop. If `NULL`, a
#'   `trm("evals", n_evals = n_evals)` is constructed.
#' @param metrics_tracker (`NULL` | [MetricsTracker])\cr
#'   Optional metrics tracker. If provided, a [CallbackMetricsTracker] is attached
#'   to the instance.
#' @param forecast_tracker (`NULL` | `ForecastTracker`)\cr
#'   Optional forecast tracker. If provided, `CallbackForecastTracker` is attached
#'   after [CallbackMetricsTracker]. Requires `metrics_tracker`.
#' @param forecast_terminator (`NULL` | [bbotk::Terminator])\cr
#'   Optional forecast-based terminator. If supplied, it is combined with the
#'   base terminator via `trm("combo", ..., any = TRUE)`.
#' @param callbacks (`NULL` | `list()` of [bbotk::CallbackBatch])\cr
#'   Additional user callbacks. These are appended after internal callbacks.
#' @param optimizer (`NULL` | [bbotk::OptimizerBatch])\cr
#'   Explicit optimizer to use. If `NULL`, constructs one via
#'   [optimizer_active_learning()]. Supply an optimizer from
#'   [optimizer_pool_al()] to use paper-style active learning methods.
#' @param ...
#'   Passed to [optimizer_active_learning()] when `optimizer` is `NULL`.
#'
#' @return `list()` with:
#' - `instance`: [SearchInstance]
#' - `optimizer`: configured optimizer
#' - `metrics_tracker`: the passed tracker (or `NULL`)
#'
#' @export
optimize_active <- function(objective,
    search_space = NULL,
    n_evals = NULL,
    terminator = NULL,
    metrics_tracker = NULL,
    forecast_tracker = NULL,
    forecast_terminator = NULL,
    callbacks = NULL,
    optimizer = NULL,
    ...) {

  assert_r6(objective, "Objective")
  if (!is.null(search_space)) {
    assert_param_set(search_space)
  }

  if (is.null(terminator)) {
    assert_int(n_evals, lower = 1L)
    terminator <- trm("evals", n_evals = n_evals)
  } else {
    assert_r6(terminator, "Terminator")
  }

  assert_r6(metrics_tracker, "MetricsTracker", null.ok = TRUE)
  assert_r6(forecast_tracker, "ForecastTracker", null.ok = TRUE)
  assert_r6(forecast_terminator, "Terminator", null.ok = TRUE)

  user_callbacks <- assert_callbacks(as_callbacks(callbacks))

  instance_callbacks <- list()
  if (!is.null(metrics_tracker)) {
    callback <- tryCatch(
      CallbackMetricsTracker$new(metrics_tracker = metrics_tracker),
      error = function(e) NULL
    )
    # TODO: Re-enable hard failure once tracker/callback integration is stable.
    if (!is.null(callback)) {
      instance_callbacks[[callback$id]] <- callback
    }
  }
  if (!is.null(forecast_tracker) && !is.null(metrics_tracker)) {
    callback <- tryCatch(
      CallbackForecastTracker$new(
        metrics_tracker = metrics_tracker,
        forecast_tracker = forecast_tracker
      ),
      error = function(e) NULL
    )
    # TODO: Re-enable hard failure once tracker/callback integration is stable.
    if (!is.null(callback)) {
      instance_callbacks[[callback$id]] <- callback
    }
  } else if (!is.null(forecast_tracker)) {
    # TODO: Re-enable explicit validation once forecast tracking is wired again.
  }
  if (length(user_callbacks) > 0L) {
    instance_callbacks <- c(instance_callbacks, user_callbacks)
  }

  if (!is.null(forecast_terminator)) {
    terminator <- trm("combo",
      terminators = list(terminator, forecast_terminator),
      any = TRUE
    )
  }

  if (is.null(optimizer)) {
    optimizer <- optimizer_active_learning(...)
  }

  search_instance <- SearchInstance$new(
    objective = objective,
    search_space = search_space,
    terminator = terminator,
    callbacks = instance_callbacks
  )

  optimizer$optimize(search_instance)

  list(
    instance = search_instance,
    optimizer = optimizer,
    metrics_tracker = metrics_tracker
  )
}
