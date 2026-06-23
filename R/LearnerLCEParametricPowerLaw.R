#' @title Parametric Power-Law LCE Learner
#'
#' @name mlr_learners_lce.parametric_power_law
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Fits a three-parameter power-law learning curve
#' \deqn{f(b) = c + a\,b^{-k}}
#' to the per-batch surrogate performance. Compared to
#' [LearnerLCEParametricExponential] the power-law family decays more slowly,
#' matching the heavier-tailed convergence often seen in sample-complexity
#' bounds.
#'
#' The curve is fit on the task's [lce_link] scale and standard errors are
#' computed exactly as for [LearnerLCEParametricExponential] (link-scale
#' epistemic `se_epistemic` plus total predictive `se`).
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
        id = "lce.parametric_power_law",
        param_set = param_set,
        predict_types = c("response", "se", "target_reached"),
        feature_types = "integer",
        label = "Parametric Power-Law LCE",
        man = "celecx::mlr_learners_lce.parametric_power_law"
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
      if (min(pb$batch) <= 0) {
        stopf("'%s' requires positive batch numbers (got min(batch_nr) = %g)",
          self$id, min(pb$batch))
      }

      batch_vec <- pb$batch
      value_vec <- link$transform(pb$value)
      first_value <- value_vec[1L]
      last_value <- value_vec[pb$n_batches]
      par_init <- c(
        pv$asymptote_init %??% last_value,
        pv$amplitude_init %??% (first_value - last_value),
        pv$rate_init %??% 1
      )

      objective <- function(par) {
        sum((value_vec - (par[1L] + par[2L] * batch_vec^(-par[3L])))^2)
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
      if (any(bb <= 0)) {
        stopf("'%s' requires positive batch numbers (got min(batch_nr) = %g)",
          self$id, min(bb))
      }
      decay <- bb^(-coefs[["rate"]])
      mu <- coefs[["asymptote"]] + coefs[["amplitude"]] * decay
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      grad_rows <- cbind(1, decay, -coefs[["amplitude"]] * decay * log(bb))
      se <- lce_se_components(grad_rows, m$Sigma, m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se$se_total, se$se_epi, link,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_power_law"]] <- LearnerLCEParametricPowerLaw
