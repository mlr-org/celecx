#' @title Parametric Logistic LCE Learner
#'
#' @name mlr_learners_lce.parametric_logistic
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
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
#' residual variance), both on the link scale.
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
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        lower_init = p_dbl(tags = "train"),
        upper_init = p_dbl(tags = "train"),
        midpoint_init = p_dbl(tags = "train"),
        rate_init = p_dbl(lower = 0, tags = "train"),
        rate_lower = p_dbl(lower = 0, init = 1e-6, tags = c("train", "required")),
        maxit = p_int(lower = 1L, init = 500L, tags = c("train", "required"))
      )

      super$initialize(
        id = "lce.parametric_logistic",
        param_set = param_set,
        predict_types = c("response", "se", "target_reached"),
        feature_types = "integer",
        label = "Parametric Logistic LCE",
        man = "celecx::mlr_learners_lce.parametric_logistic"
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
        pv$lower_init %??% first_value,
        pv$upper_init %??% last_value,
        pv$midpoint_init %??% mean(pb$batch),
        pv$rate_init %??% if (span > 0) 4 / span else 1
      )

      objective <- function(par) {
        sigm <- 1 / (1 + exp(-par[4L] * (batch_vec - par[3L])))
        sum((value_vec - (par[1L] + (par[2L] - par[1L]) * sigm))^2)
      }

      fit <- lce_fit_parametric(
        par_init = par_init,
        lower = c(-Inf, -Inf, -Inf, pv$rate_lower),
        upper = c(Inf, Inf, Inf, Inf),
        fn = objective,
        maxit = pv$maxit,
        hessian = TRUE
      )

      cov_info <- lce_param_cov(fit$hessian, fit$sse, pb$n_batches, 4L)
      coefs <- c(lower = fit$coefficients[1L], upper = fit$coefficients[2L],
        midpoint = fit$coefficients[3L], rate = fit$coefficients[4L])

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
      sigm <- 1 / (1 + exp(-coefs[["rate"]] * (bb - coefs[["midpoint"]])))
      mu <- coefs[["lower"]] + (coefs[["upper"]] - coefs[["lower"]]) * sigm
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      gap <- coefs[["upper"]] - coefs[["lower"]]
      bell <- sigm * (1 - sigm)
      grad_rows <- cbind(
        1 - sigm,
        sigm,
        -coefs[["rate"]] * gap * bell,
        (bb - coefs[["midpoint"]]) * gap * bell
      )
      se <- lce_se_components(grad_rows, m$Sigma, m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se$se_total, se$se_epi, link,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_logistic"]] <- LearnerLCEParametricLogistic
