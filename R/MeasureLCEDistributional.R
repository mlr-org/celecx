#' @title Distributional LCE Measures
#'
#' @name mlr_measures_lce_distributional
#' @include MeasureLCE.R
#' @include utils_lce_distr.R
#'
#' @description
#' Per-batch distributional measures for [TaskLCE] / [LearnerLCE] evaluations.
#' Each measure aggregates the prediction and truth to one value per batch and
#' then scores the per-batch predictive distribution against the realised
#' performance. The predictive is interpreted as Gaussian on the task's
#' [lce_link] scale, carried by the `se` predict type, except for `lce.pinball`,
#' which reads the `quantiles` predict type.
#'
#' * `lce.crps`: closed-form continuous ranked probability score of the
#'   Normal-on-link predictive against the link-transformed truth. The headline
#'   proper score for the forecast distribution.
#' * `lce.reach_brier`: Brier score of the predicted probability that the metric
#'   has reached `target` against whether the realised per-batch performance has.
#'   The direction (reach from above / below) is read from the task's measure.
#'   This is the proper, grid-free way to benchmark "batches-to-target".
#' * `lce.coverage`: empirical coverage of the central `level` predictive
#'   interval (ideally equal to `level`; reported, not optimised).
#' * `lce.interval_score`: Winkler interval score of the central `level`
#'   predictive interval (sharpness plus miscoverage penalty).
#' * `lce.pinball`: average pinball (quantile) loss at quantile `alpha`, which
#'   must be one of the probabilities the learner predicted. Unlike the other
#'   distributional measures it is scored on the natural scale, since it consumes
#'   the natural-scale `quantiles` predict type directly.
#'
#' All measures support observation weights from a `weights_measure` task column;
#' the weight of a batch is the sum of weights of its archive rows.
NULL

MeasureLCECRPS <- R6Class("MeasureLCECRPS",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "lce.crps",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "se",
        properties = "weights",
        label = "Continuous Ranked Probability Score (per batch)",
        man = "celecx::mlr_measures_lce_distributional"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      link <- lce_link(task$link)
      agg <- lce_per_batch_cols(prediction, task,
        list(response = prediction$response, se = prediction$se), weights)
      crps <- lce_gaussian_crps(link$transform(agg$truth),
        link$transform(agg$response), agg$se)
      lce_weighted_mean(crps, agg$weight)
    }
  )
)

MeasureLCEReachBrier <- R6Class("MeasureLCEReachBrier",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      param_set <- ps(target = p_dbl(tags = c("required", "score")))
      super$initialize(
        id = "lce.reach_brier",
        param_set = param_set,
        range = c(0, 1),
        minimize = TRUE,
        predict_type = "se",
        properties = "weights",
        label = "Reach-Target Brier Score (per batch)",
        man = "celecx::mlr_measures_lce_distributional"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      target <- self$param_set$values$target
      if (is.null(target)) {
        stopf("'%s' requires the 'target' parameter to be set", self$id)
      }
      assert_number(target)
      link <- lce_link(task$link)
      minimize <- lce_task_minimize(task, sprintf("Measure '%s'", self$id))
      agg <- lce_per_batch_cols(prediction, task,
        list(response = prediction$response, se = prediction$se), weights)
      reach <- lce_reach_prob(link$transform(target),
        link$transform(agg$response), agg$se, minimize)
      reached <- if (minimize) agg$truth <= target else agg$truth >= target
      lce_weighted_mean((reach - as.numeric(reached))^2, agg$weight)
    }
  )
)

MeasureLCECoverage <- R6Class("MeasureLCECoverage",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      param_set <- ps(level = p_dbl(lower = 0, upper = 1, tags = c("required", "score")))
      param_set$set_values(level = 0.9)
      super$initialize(
        id = "lce.coverage",
        param_set = param_set,
        range = c(0, 1),
        minimize = NA,
        predict_type = "se",
        properties = "weights",
        label = "Central-Interval Coverage (per batch)",
        man = "celecx::mlr_measures_lce_distributional"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      level <- self$param_set$values$level
      z <- stats::qnorm((1 + level) / 2)
      link <- lce_link(task$link)
      agg <- lce_per_batch_cols(prediction, task,
        list(response = prediction$response, se = prediction$se), weights)
      inside <- abs(link$transform(agg$truth) - link$transform(agg$response)) <= z * agg$se
      lce_weighted_mean(as.numeric(inside), agg$weight)
    }
  )
)

MeasureLCEIntervalScore <- R6Class("MeasureLCEIntervalScore",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      param_set <- ps(level = p_dbl(lower = 0, upper = 1, tags = c("required", "score")))
      param_set$set_values(level = 0.9)
      super$initialize(
        id = "lce.interval_score",
        param_set = param_set,
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "se",
        properties = "weights",
        label = "Winkler Interval Score (per batch)",
        man = "celecx::mlr_measures_lce_distributional"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      level <- self$param_set$values$level
      alpha <- 1 - level
      z <- stats::qnorm(1 - alpha / 2)
      link <- lce_link(task$link)
      agg <- lce_per_batch_cols(prediction, task,
        list(response = prediction$response, se = prediction$se), weights)
      # Scored on the link scale, consistent with lce.crps / lce.coverage: the
      # central interval of the Normal-on-link predictive is [mu +- z*se].
      mu <- link$transform(agg$response)
      y <- link$transform(agg$truth)
      lower <- mu - z * agg$se
      upper <- mu + z * agg$se
      # pmax() rather than the `(lower - y) * (y < lower)` indicator idiom, which
      # would evaluate Inf * FALSE = NaN when the link maps a boundary truth to
      # +/-Inf (e.g. log link, per-batch performance 0). With pmax() the score
      # correctly diverges to Inf there, consistent with lce.crps.
      score <- (upper - lower) +
        (2 / alpha) * pmax(0, lower - y) +
        (2 / alpha) * pmax(0, y - upper)
      lce_weighted_mean(score, agg$weight)
    }
  )
)

MeasureLCEPinball <- R6Class("MeasureLCEPinball",
  inherit = MeasureLCE,
  public = list(
    initialize = function() {
      param_set <- ps(alpha = p_dbl(lower = 0, upper = 1, tags = c("required", "score")))
      param_set$set_values(alpha = 0.5)
      super$initialize(
        id = "lce.pinball",
        param_set = param_set,
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "quantiles",
        properties = "weights",
        label = "Average Pinball Loss (per batch)",
        man = "celecx::mlr_measures_lce_distributional"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, weights = NULL, ...) {
      alpha <- self$param_set$values$alpha
      probs <- attr(prediction$data$quantiles, "probs")
      assert_choice(alpha, probs,
        .var.name = sprintf("alpha (must be a predicted quantile probability)"))
      qcol <- prediction$data$quantiles[, which(probs == alpha)]
      agg <- lce_per_batch_cols(prediction, task, list(q = qcol), weights)
      d <- agg$truth - agg$q
      lce_weighted_mean(pmax(alpha * d, (alpha - 1) * d), agg$weight)
    }
  )
)

#' @include aaa.R
measures[["lce.crps"]] <- MeasureLCECRPS
measures[["lce.reach_brier"]] <- MeasureLCEReachBrier
measures[["lce.coverage"]] <- MeasureLCECoverage
measures[["lce.interval_score"]] <- MeasureLCEIntervalScore
measures[["lce.pinball"]] <- MeasureLCEPinball
