#' @title LCE Winkler Score
#'
#' @name mlr_measures_lce.winkler
#'
#' @description
#' Measures prediction interval quality by combining width with a penalty
#' for observations falling outside the interval. Smaller scores indicate
#' better calibrated and narrower intervals.
#'
#' @details
#' \deqn{
#'   W_i =
#'   \begin{cases}
#'     (u_i - l_i) + \frac{2}{\alpha}(l_i - y_i), & y_i < l_i \\
#'     (u_i - l_i), & l_i \le y_i \le u_i \\
#'     (u_i - l_i) + \frac{2}{\alpha}(y_i - u_i), & y_i > u_i
#'   \end{cases}
#' }
#'
#' The Winkler score is the mean of \eqn{W_i} over all observations.
#'
#' @section Parameters:
#' * `alpha` (`numeric(1)`)\cr
#'   Significance level (default 0.1, for the interval from quantile 0.05
#'   to 0.95).
#'
#' @export
MeasureLCEWinkler = R6Class("MeasureLCEWinkler",
  inherit = MeasureRegr,
  public = list(

    #' @description
    #' Creates a new instance.
    initialize = function() {
      param_set = ps(alpha = p_dbl(lower = 0, upper = 1, tags = "required"))
      param_set$set_values(alpha = 0.1)

      super$initialize(
        id = "lce.winkler",
        param_set = param_set,
        predict_type = "quantiles",
        range = c(0, Inf),
        minimize = TRUE,
        label = "LCE Winkler Score",
        man = "celecx::mlr_measures_lce.winkler"
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
      lt = prediction$data$quantiles[, which(probs == lower_prob)]
      ut = prediction$data$quantiles[, which(probs == upper_prob)]

      width = ut - lt
      score = fifelse(
        truth < lt,
        width + (2 / alpha) * (lt - truth),
        fifelse(truth > ut, width + (2 / alpha) * (truth - ut), width)
      )
      mean(score, na.rm = TRUE)
    }
  )
)

#' @include aaa.R
measures[["lce.winkler"]] = MeasureLCEWinkler
