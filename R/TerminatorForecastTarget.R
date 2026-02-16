#' @title Forecast Target Terminator
#'
#' @name mlr_terminators_forecast_target
#'
#' @description
#' Terminator that stops based on forecasted target-reaching probabilities.
#'
#' @details
#' The terminator reads state from a `ForecastTracker` and supports two
#' stopping rules:
#' - stop when target is already reached (`stop_on_reached`)
#' - stop when success probability at budget is below threshold
#'   (`stop_on_unlikely` and `p_reach_threshold`)
#'
#' @section Parameters:
#' * `stop_on_unlikely` (`logical(1)`)
#' * `p_reach_threshold` (`numeric(1)`)
#' * `stop_on_reached` (`logical(1)`)
#'
#' @family Terminator
#'
#' @export
#' @examples
#' \dontrun{
#' tracker <- ForecastTracker$new(
#'   extrapolator = CurveExtrapolatorParametric$new(),
#'   metric_name = "best_y",
#'   target = 0.1
#' )
#' trm <- TerminatorForecastTarget$new(tracker)
#' }
TerminatorForecastTarget <- R6Class("TerminatorForecastTarget",
  inherit = bbotk::Terminator,

  public = list(
    #' @field forecast_tracker (`ForecastTracker`)
    forecast_tracker = NULL,

    #' @description
    #' Creates a new TerminatorForecastTarget.
    #'
    #' @param forecast_tracker (`ForecastTracker`)
    #'   Tracker used for forecast state.
    initialize = function(forecast_tracker) {
      assert_r6(forecast_tracker, "ForecastTracker")
      self$forecast_tracker <- forecast_tracker

      param_set <- ps(
        stop_on_unlikely = p_lgl(tags = "required"),
        p_reach_threshold = p_dbl(lower = 0, upper = 1, tags = "required"),
        stop_on_reached = p_lgl(tags = "required")
      )
      param_set$set_values(
        stop_on_unlikely = TRUE,
        p_reach_threshold = 0.05,
        stop_on_reached = TRUE
      )

      super$initialize(
        id = "forecast_target",
        param_set = param_set,
        properties = character(0),
        label = "Forecast Target",
        man = "celecx::mlr_terminators_forecast_target"
      )
    },

    #' @description
    #' Is `TRUE` iff forecast-based stop condition is met.
    #'
    #' @return `logical(1)`.
    is_terminated = function(archive) {
      assert_r6(archive, "Archive")

      if (self$forecast_tracker$n_updates == 0L) {
        return(FALSE)
      }

      latest <- self$forecast_tracker$latest
      pv <- self$param_set$values

      if (isTRUE(pv$stop_on_reached) && !is.null(self$forecast_tracker$target)) {
        target <- self$forecast_tracker$target
        metric_value <- latest$metric_value[[1L]]
        reached <- if (self$forecast_tracker$direction == "minimize") {
          metric_value <= target
        } else {
          metric_value >= target
        }
        if (isTRUE(reached)) {
          return(TRUE)
        }
      }

      if (isTRUE(pv$stop_on_unlikely)) {
        p_reach <- latest$p_reach_budget[[1L]]
        if (is.finite(p_reach) && !is.na(p_reach) && p_reach < pv$p_reach_threshold) {
          return(TRUE)
        }
      }

      FALSE
    }
  ),

  private = list(
    .status = function(archive) {
      if (self$is_terminated(archive)) {
        c(max_steps = 100L, current_steps = 100L)
      } else {
        c(max_steps = 100L, current_steps = 0L)
      }
    }
  )
)

#' @include aaa.R
terminators[["forecast_target"]] = TerminatorForecastTarget
