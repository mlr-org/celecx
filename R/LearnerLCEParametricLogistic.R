#' @title Parametric Logistic LCE Learner
#'
#' @name mlr_learners_lce.parametric_logistic
#'
#' @include LearnerLCEParametric.R
#'
#' @description
#' Fits a four-parameter logistic learning curve
#' \deqn{f(b) = \ell + \frac{u - \ell}{1 + \exp(-k\,(b - b_0))}}
#' to the per-batch surrogate performance. Captures S-shaped trajectories with
#' a lower asymptote \eqn{\ell}, an upper asymptote \eqn{u}, a transition
#' midpoint \eqn{b_0}, and a steepness \eqn{k > 0}. Decreasing trajectories
#' are represented by `upper < lower` rather than by a negative rate.
#'
#' The curve is fit on the task's [lce_link] scale. When `predict_type = "se"`
#' the learner reports the epistemic Gauss-Newton delta-method standard error
#' `se_epistemic` and the total predictive standard error `se` (adding the
#' residual variance), both on the link scale; predictive quantiles are the
#' exact Normal quantiles of that predictive.
#'
#' @section Parameters:
#' * `lower_init`, `upper_init`, `midpoint_init`, `rate_init` :: `numeric(1)`\cr
#'   Initial values for `lower`, `upper`, `midpoint`, and `rate`. Defaults are
#'   derived from the training data when unset.
#' * `rate_lower` :: `numeric(1)`\cr
#'   Lower bound for `rate`. Initialized to `1e-6`.
#' * `maxit` :: `integer(1)`\cr
#'   Maximum optim iterations. Initialized to `500`.
#'
#' @export
LearnerLCEParametricLogistic <- R6Class("LearnerLCEParametricLogistic",
  inherit = LearnerLCEParametric,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        lower_init = p_dbl(tags = "train"),
        upper_init = p_dbl(tags = "train"),
        midpoint_init = p_dbl(tags = "train"),
        rate_init = p_dbl(lower = 0, tags = "train")
      )

      super$initialize(
        id = "lce.parametric_logistic",
        param_set = param_set,
        label = "Parametric Logistic LCE",
        man = "celecx::mlr_learners_lce.parametric_logistic"
      )
    }
  ),

  private = list(
    .coef_names = c("lower", "upper", "midpoint", "rate"),

    .curve = function(par, b) {
      sigm <- 1 / (1 + exp(-par[[4L]] * (b - par[[3L]])))
      par[[1L]] + (par[[2L]] - par[[1L]]) * sigm
    },

    .grad = function(par, b) {
      sigm <- 1 / (1 + exp(-par[[4L]] * (b - par[[3L]])))
      gap <- par[[2L]] - par[[1L]]
      bell <- sigm * (1 - sigm)
      cbind(
        1 - sigm,
        sigm,
        -par[[4L]] * gap * bell,
        (b - par[[3L]]) * gap * bell
      )
    },

    .par_init = function(pb, value_vec, pv) {
      first_value <- value_vec[1L]
      last_value <- value_vec[pb$n_batches]
      span <- max(pb$batch) - min(pb$batch)
      c(
        pv$lower_init %??% first_value,
        pv$upper_init %??% last_value,
        pv$midpoint_init %??% mean(pb$batch),
        pv$rate_init %??% if (span > 0) 4 / span else 1
      )
    },

    .par_lower = function(pv) {
      c(-Inf, -Inf, -Inf, pv$rate_lower)
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_logistic"]] <- LearnerLCEParametricLogistic
