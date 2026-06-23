#' @title Parametric Exponential LCE Learner
#'
#' @name mlr_learners_lce.parametric_exponential
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
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
#' degrees of freedom are non-positive, both are `NA`.
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
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        asymptote_init = p_dbl(tags = "train"),
        amplitude_init = p_dbl(tags = "train"),
        rate_init = p_dbl(lower = 0, tags = "train"),
        rate_lower = p_dbl(lower = 0, init = 1e-6, tags = c("train", "required")),
        maxit = p_int(lower = 1L, init = 500L, tags = c("train", "required"))
      )

      super$initialize(
        id = "lce.parametric_exponential",
        param_set = param_set,
        predict_types = c("response", "se", "target_reached"),
        feature_types = "integer",
        label = "Parametric Exponential LCE",
        man = "celecx::mlr_learners_lce.parametric_exponential"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      link <- lce_link(task$link)
      pb <- lce_train_per_batch(task, link)
      if (pb$n_batches < 2L) {
        stopf("Need at least two distinct batches to fit '%s'", self$id)
      }

      batch_vec <- pb$batch
      value_vec <- link$transform(pb$value)
      first_value <- value_vec[1L]
      last_value <- value_vec[pb$n_batches]
      span <- max(pb$batch) - min(pb$batch)
      par_init <- c(
        pv$asymptote_init %??% last_value,
        pv$amplitude_init %??% (first_value - last_value),
        pv$rate_init %??% if (span > 0) log(2) / span else 1
      )

      objective <- function(par) {
        sum((value_vec - (par[1L] + par[2L] * exp(-par[3L] * batch_vec)))^2)
      }

      fit <- lce_fit_parametric(
        par_init = par_init,
        lower = c(-Inf, -Inf, pv$rate_lower),
        upper = c(Inf, Inf, Inf),
        fn = objective,
        maxit = pv$maxit,
        hessian = TRUE
      )

      cov_info <- lce_param_cov(fit$hessian, fit$sse, pb$n_batches, 3L)
      coefs <- c(asymptote = fit$coefficients[1L], amplitude = fit$coefficients[2L],
        rate = fit$coefficients[3L])

      list(
        coefficients = coefs,
        sigma2 = cov_info$sigma2,
        Sigma = cov_info$Sigma,
        n_batches = pb$n_batches,
        convergence = fit$convergence,
        link = task$link,
        minimize = lce_model_minimize(task)
      )
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      coefs <- m$coefficients
      bb <- lce_predict_batches(task)
      decay <- exp(-coefs[["rate"]] * bb)
      mu <- coefs[["asymptote"]] + coefs[["amplitude"]] * decay
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      # gradient of f(b) wrt (c, a, k) at each prediction
      grad_rows <- cbind(1, decay, -coefs[["amplitude"]] * bb * decay)
      se <- lce_se_components(grad_rows, m$Sigma, m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se$se_total, se$se_epi, link,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_exponential"]] <- LearnerLCEParametricExponential
