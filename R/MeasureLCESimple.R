#' @title Per-Batch LCE Loss Measures
#'
#' @name mlr_measures_lce
#' @include MeasureLCE.R
#'
#' @description
#' Common per-batch loss measures for [TaskLCE] / [LearnerLCE] evaluations.
#' Each measure aggregates predictions and truth to one value per batch (via
#' `lce_per_batch`) and then computes a standard regression loss on those
#' per-batch pairs.
#'
#' * `lce.mse`: mean squared error.
#' * `lce.rmse`: root mean squared error (the square root of the per-batch MSE).
#' * `lce.mae`: mean absolute error.
#'
#' All measures support observation weights from a `weights_measure` task
#' column; the weight applied to a batch is the sum of weights of its archive
#' rows.
NULL

MeasureLCEMSE <- R6Class("MeasureLCEMSE",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "lce.mse",
        range = c(0, Inf),
        minimize = TRUE,
        properties = "weights",
        label = "Mean Squared Error (per batch)",
        man = "celecx::mlr_measures_lce"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      agg <- lce_per_batch(prediction, task, weights)
      err <- agg$truth - agg$response
      if (is.null(agg$weight)) {
        mean(err^2)
      } else {
        sum(agg$weight * err^2) / sum(agg$weight)
      }
    }
  )
)

MeasureLCERMSE <- R6Class("MeasureLCERMSE",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "lce.rmse",
        range = c(0, Inf),
        minimize = TRUE,
        properties = "weights",
        label = "Root Mean Squared Error (per batch)",
        man = "celecx::mlr_measures_lce"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      agg <- lce_per_batch(prediction, task, weights)
      err <- agg$truth - agg$response
      mse <- if (is.null(agg$weight)) mean(err^2) else sum(agg$weight * err^2) / sum(agg$weight)
      sqrt(mse)
    }
  )
)

MeasureLCEMAE <- R6Class("MeasureLCEMAE",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "lce.mae",
        range = c(0, Inf),
        minimize = TRUE,
        properties = "weights",
        label = "Mean Absolute Error (per batch)",
        man = "celecx::mlr_measures_lce"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      agg <- lce_per_batch(prediction, task, weights)
      err <- abs(agg$truth - agg$response)
      if (is.null(agg$weight)) {
        mean(err)
      } else {
        sum(agg$weight * err) / sum(agg$weight)
      }
    }
  )
)

#' @include aaa.R
measures[["lce.mse"]] <- MeasureLCEMSE
measures[["lce.rmse"]] <- MeasureLCERMSE
measures[["lce.mae"]] <- MeasureLCEMAE
