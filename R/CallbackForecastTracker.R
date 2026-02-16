#' @title Forecast Tracker Callback
#'
#' @name celecx.forecast_tracker
#'
#' @description
#' Updates a `ForecastTracker` after each evaluated batch.
#'
#' @details
#' This callback expects that a [MetricsTracker] callback has already logged the
#' current batch in the same callback chain.
#'
#' @examples
#' \dontrun{
#' metrics_tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))
#' forecast_tracker <- ForecastTracker$new(
#'   extrapolator = CurveExtrapolatorParametric$new(),
#'   metric_name = "best_y"
#' )
#' callback <- clbk("celecx.forecast_tracker",
#'   metrics_tracker = metrics_tracker,
#'   forecast_tracker = forecast_tracker
#' )
#' }
NULL

#' @rdname celecx.forecast_tracker
#' @export
CallbackForecastTracker <- R6Class("CallbackForecastTracker",
  inherit = bbotk::CallbackBatch,

  public = list(
    #' @field metrics_tracker ([MetricsTracker])
    metrics_tracker = NULL,

    #' @field forecast_tracker (`ForecastTracker`)
    forecast_tracker = NULL,

    #' @description
    #' Creates a new CallbackForecastTracker.
    #'
    #' @param metrics_tracker ([MetricsTracker])
    #'   Tracker containing the metrics history.
    #' @param forecast_tracker (`ForecastTracker`)
    #'   Tracker to be updated.
    initialize = function(metrics_tracker, forecast_tracker) {
      assert_r6(metrics_tracker, "MetricsTracker")
      assert_r6(forecast_tracker, "ForecastTracker")

      super$initialize(
        id = "celecx.forecast_tracker",
        label = "Forecast Tracker",
        man = "celecx::celecx.forecast_tracker"
      )

      self$metrics_tracker <- metrics_tracker
      self$forecast_tracker <- forecast_tracker

      self$on_optimizer_after_eval <- function(callback, context) {
        assert(check_r6(context, "ContextBatch"), check_r6(context, "ContextSearch"))

        callback$forecast_tracker$update(
          metrics_data = callback$metrics_tracker$data
        )

        invisible(NULL)
      }
    }
  )
)

# Lazy loader function for dictionary registration
load_callback_forecast_tracker <- function(metrics_tracker, forecast_tracker) {
  CallbackForecastTracker$new(metrics_tracker, forecast_tracker)
}
