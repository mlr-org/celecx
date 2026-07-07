#' @title Parametric Exponential LCE Learner
#'
#' @name mlr_learners_lce.parametric_exponential
#'
#' @include LearnerLCEParametric.R
#'
#' @description
#' Fits a three-parameter exponential learning curve
#' \deqn{f(b) = c + a \exp(-k\,b)}
#' to the per-batch surrogate performance. Suitable both for performance
#' measures that grow to an asymptote (`a` ends up negative) and for loss
#' measures that decay to an asymptote (`a` ends up positive).
#'
#' The curve is fit on the task's [lce_link] scale, so for a non-identity link
#' the family above describes `g(f(b))` and the natural-scale prediction is
#' `g^{-1}` of the fitted curve.
#'
#' When `predict_type = "se"` the learner reports two link-scale standard errors:
#' `se_epistemic`, the Gauss-Newton delta-method SD of the mean curve (parameter
#' covariance \eqn{\hat\sigma^2 (H/2)^{-1}} propagated through the gradient of
#' `f`), and `se`, the total predictive SD `sqrt(se_epistemic^2 + sigma2)` that
#' adds the residual variance back. If the Hessian is singular or the residual
#' degrees of freedom are non-positive, both are `NA`. Predictive quantiles
#' (`predict_type = "quantiles"`) are the exact Normal quantiles of that
#' predictive, back-transformed to the natural scale.
#'
#' Multiple archive rows belonging to the same batch are collapsed via the
#' batch-wise mean of `target` before fitting; ties always agree by
#' construction when the upstream task was produced by
#' [CallbackSurrogatePerformance].
#'
#' @section Parameters:
#' * `asymptote_init`, `amplitude_init`, `rate_init` :: `numeric(1)`\cr
#'   Initial values for `c`, `a`, and `k`. Defaults derived from the training
#'   data when unset.
#' * `rate_lower` :: `numeric(1)`\cr
#'   Lower bound for the decay rate `k`. Initialized to `1e-6` so the curve
#'   stays monotone and the model identifiable.
#' * `maxit` :: `integer(1)`\cr
#'   Maximum optim iterations. Initialized to `500`.
#'
#' @export
LearnerLCEParametricExponential <- R6Class("LearnerLCEParametricExponential",
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
        id = "lce.parametric_exponential",
        param_set = param_set,
        label = "Parametric Exponential LCE",
        man = "celecx::mlr_learners_lce.parametric_exponential"
      )
    }
  ),

  private = list(
    .coef_names = c("asymptote", "amplitude", "rate"),

    .curve = function(par, b) {
      par[[1L]] + par[[2L]] * exp(-par[[3L]] * b)
    },

    .grad = function(par, b) {
      decay <- exp(-par[[3L]] * b)
      cbind(1, decay, -par[[2L]] * b * decay)
    },

    .par_init = function(pb, value_vec, pv) {
      first_value <- value_vec[1L]
      last_value <- value_vec[pb$n_batches]
      span <- max(pb$batch) - min(pb$batch)
      c(
        pv$asymptote_init %??% last_value,
        pv$amplitude_init %??% (first_value - last_value),
        pv$rate_init %??% if (span > 0) log(2) / span else 1
      )
    },

    .par_lower = function(pv) {
      c(-Inf, -Inf, pv$rate_lower)
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_exponential"]] <- LearnerLCEParametricExponential
