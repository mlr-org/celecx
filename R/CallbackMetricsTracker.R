#' @title Metrics Tracker Callback
#'
#' @name celecx.metrics_tracker
#'
#' @description
#' Logs metrics after each batch evaluation using a [MetricsTracker].
#'
#' This callback is meant to be passed as `callbacks` to a bbotk
#' [bbotk::OptimInstanceBatch] / [bbotk::OptimInstanceAsync] (or via the
#' `callbacks` argument of `oi()` / `bb_optimize()`).
#'
#' @examples
#' \dontrun{
#' tracker <- MetricsTracker$new()
#' callback <- clbk("celecx.metrics_tracker", metrics_tracker = tracker)
#' }
NULL

#' @rdname celecx.metrics_tracker
#' @export
CallbackMetricsTracker <- R6Class("CallbackMetricsTracker",
  inherit = bbotk::CallbackBatch,

  public = list(
    #' @field metrics_tracker ([MetricsTracker])\cr
    #' Tracker that collects per-batch metrics.
    metrics_tracker = NULL,

    #' @description
    #' Creates a new CallbackMetricsTracker.
    #'
    #' @param metrics_tracker ([MetricsTracker])\cr
    #'   Tracker used for logging.
    initialize = function(metrics_tracker) {
      assert_r6(metrics_tracker, "MetricsTracker")
      super$initialize(
        id = "celecx.metrics_tracker",
        label = "Metrics Tracker",
        man = "celecx::celecx.metrics_tracker"
      )
      self$metrics_tracker <- metrics_tracker

      # CallbackBatch implements stages as public fields, not methods.
      self$on_optimizer_after_eval <- function(callback, context) {
        assert(check_r6(context, "ContextBatch"), check_r6(context, "ContextSearch"))

        archive <- context$instance$archive
        batch_nr <- archive$n_batch

        surrogate <- NULL
        if (!is.null(context$optimizer)) {
          surrogate <- tryCatch(
            context$optimizer$surrogate,
            error = function(e) NULL
          )
        }

        callback$metrics_tracker$log_batch(
          batch_nr = batch_nr,
          archive = archive,
          surrogate = surrogate
        )

        invisible(NULL)
      }
    }
  )
)

# Lazy loader function for dictionary registration
load_callback_metrics_tracker <- function(metrics_tracker) {
  CallbackMetricsTracker$new(metrics_tracker)
}
