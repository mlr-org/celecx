#' @title LCE Prediction Interval Coverage
#'
#' @name mlr_measures_lce.coverage
#'
#' @description
#' Measures the fraction of true values that fall within the predicted
#' quantile interval. Higher values indicate better calibration.
#'
#' @details
#' Given lower quantile \eqn{l_i} at level \eqn{\alpha/2} and upper
#' quantile \eqn{u_i} at level \eqn{1 - \alpha/2}:
#' \deqn{
#'   \text{Coverage} = \frac{1}{n} \sum_{i=1}^{n} I(l_i \le y_i \le u_i)
#' }
#'
#' A well-calibrated \eqn{(1 - \alpha)} interval should have coverage
#' close to \eqn{1 - \alpha}.
#'
#' @section Parameters:
#' * `alpha` (`numeric(1)`)\cr
#'   Significance level (default 0.1, yielding 90% intervals from
#'   quantiles 0.05 and 0.95).
#'
#' @export
MeasureLCECoverage = R6Class("MeasureLCECoverage",
  inherit = MeasureRegr,
  public = list(

    #' @description
    #' Creates a new instance.
    initialize = function() {
      param_set = ps(alpha = p_dbl(lower = 0, upper = 1, tags = "required"))
      param_set$set_values(alpha = 0.1)

      super$initialize(
        id = "lce.coverage",
        param_set = param_set,
        predict_type = "quantiles",
        range = c(0, 1),
        minimize = FALSE,
        label = "LCE Interval Coverage",
        man = "celecx::mlr_measures_lce.coverage"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      alpha = self$param_set$get_values()$alpha
      lower_prob = alpha / 2
      upper_prob = 1 - alpha / 2

      probs = attr(prediction$data$quantiles, "probs")
      assert_choice(lower_prob, probs)
      assert_choice(upper_prob, probs)

      truth = prediction$truth
      lower = prediction$data$quantiles[, which(probs == lower_prob)]
      upper = prediction$data$quantiles[, which(probs == upper_prob)]

      mean(truth >= lower & truth <= upper, na.rm = TRUE)
    }
  )
)

#' @include aaa.R
measures[["lce.coverage"]] = MeasureLCECoverage
