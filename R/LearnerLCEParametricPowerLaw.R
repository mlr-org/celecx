#' @title Parametric Power-Law LCE Learner
#'
#' @name mlr_learners_lce.parametric_power_law
#'
#' @include LearnerLCEParametric.R
#'
#' @description
#' Fits a three-parameter power-law learning curve
#' \deqn{f(b) = c + a\,b^{-k}}
#' to the per-batch surrogate performance. Compared to
#' [LearnerLCEParametricExponential] the power-law family decays more slowly,
#' matching the heavier-tailed convergence often seen in sample-complexity
#' bounds.
#'
#' The curve is fit on the task's [lce_link] scale and standard errors and
#' quantiles are computed exactly as for [LearnerLCEParametricExponential]
#' (link-scale epistemic `se_epistemic` plus total predictive `se`).
#'
#' Training batches with `batch_nr <= 0` are not supported because \eqn{b^{-k}}
#' is then undefined; such tasks raise an error.
#'
#' @section Parameters:
#' * `asymptote_init`, `amplitude_init`, `rate_init` :: `numeric(1)`\cr
#'   Initial values for `c`, `a`, and `k`.
#' * `rate_lower` :: `numeric(1)`\cr
#'   Lower bound for the decay exponent `k`. Initialized to `1e-6`.
#' * `maxit` :: `integer(1)`\cr
#'   Maximum optim iterations. Initialized to `500`.
#'
#' @export
LearnerLCEParametricPowerLaw <- R6Class("LearnerLCEParametricPowerLaw",
  inherit = LearnerLCEParametric,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        asymptote_init = p_dbl(tags = "train"),
        amplitude_init = p_dbl(tags = "train"),
        rate_init = p_dbl(lower = 0, tags = "train")
      )

      super$initialize(
        id = "lce.parametric_power_law",
        param_set = param_set,
        label = "Parametric Power-Law LCE",
        man = "celecx::mlr_learners_lce.parametric_power_law"
      )
    }
  ),

  private = list(
    .coef_names = c("asymptote", "amplitude", "rate"),

    .curve = function(par, b) {
      par[[1L]] + par[[2L]] * b^(-par[[3L]])
    },

    .grad = function(par, b) {
      decay <- b^(-par[[3L]])
      cbind(1, decay, -par[[2L]] * decay * log(b))
    },

    .par_init = function(pb, value_vec, pv) {
      first_value <- value_vec[1L]
      last_value <- value_vec[pb$n_batches]
      c(
        pv$asymptote_init %??% last_value,
        pv$amplitude_init %??% (first_value - last_value),
        pv$rate_init %??% 1
      )
    },

    .par_lower = function(pv) {
      c(-Inf, -Inf, pv$rate_lower)
    },

    .check_batches = function(b) {
      if (any(b <= 0)) {
        stopf("'%s' requires positive batch numbers (got min(batch_nr) = %g)",
          self$id, min(b))
      }
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_power_law"]] <- LearnerLCEParametricPowerLaw
