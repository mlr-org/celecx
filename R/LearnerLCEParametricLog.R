#' @title Parametric Log LCE Learner
#'
#' @name mlr_learners_lce.parametric_log
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Fits a two-parameter logarithmic learning curve
#' \deqn{f(b) = c + a \log b}
#' to the per-batch surrogate performance. Since the model is linear in
#' \eqn{(c, a)} the fit reduces to ordinary least squares on \eqn{(\log b, y)}
#' (via [stats::lsfit()]) and standard errors come from the usual
#' linear-regression covariance \eqn{\hat\sigma^2 (X^\top X)^{-1}} rather than
#' from the Gauss-Newton delta method used by the nonlinear parametric LCE
#' learners.
#'
#' Training batches with `batch_nr <= 0` are rejected because \eqn{\log b}
#' is undefined.
#'
#' @export
LearnerLCEParametricLog <- R6Class("LearnerLCEParametricLog",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      super$initialize(
        id = "lce.parametric_log",
        param_set = ps(),
        predict_types = c("response", "se", "target_reached"),
        feature_types = "integer",
        label = "Parametric Log LCE",
        man = "celecx::mlr_learners_lce.parametric_log"
      )
    }
  ),

  private = list(
    .train = function(task) {
      link <- lce_link(task$link)
      pb <- lce_train_per_batch(task, link)
      if (pb$n_batches < 2L) {
        stopf("Need at least two distinct batches to fit '%s'", self$id)
      }
      if (min(pb$batch) <= 0) {
        stopf("'%s' requires positive batch numbers (got min(batch_nr) = %g)",
          self$id, min(pb$batch))
      }
      value_vec <- link$transform(pb$value)
      X <- cbind(1, log(pb$batch))
      fit <- stats::lsfit(log(pb$batch), value_vec)
      residuals <- value_vec - (fit$coefficients[["Intercept"]] +
        fit$coefficients[["X"]] * log(pb$batch))
      df <- pb$n_batches - 2L
      sigma2 <- if (df > 0L) sum(residuals^2) / df else NA_real_
      # Linear-model parameter covariance Sigma = sigma2 * (X'X)^{-1}; NULL (not an
      # all-NA matrix) when the residual variance is undefined.
      Sigma <- if (df > 0L) {
        tryCatch(sigma2 * solve(crossprod(X)), error = function(e) NULL)
      } else {
        NULL
      }

      list(
        coefficients = c(intercept = unname(fit$coefficients[["Intercept"]]),
          slope = unname(fit$coefficients[["X"]])),
        sigma2 = sigma2,
        Sigma = Sigma,
        n_batches = pb$n_batches,
        link = task$link,
        minimize = lce_model_minimize(task)
      )
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      bb <- lce_predict_batches(task)
      if (any(bb <= 0)) {
        stopf("'%s' requires positive batch numbers (got min(batch_nr) = %g)",
          self$id, min(bb))
      }
      coefs <- m$coefficients
      log_bb <- log(bb)
      mu <- coefs[["intercept"]] + coefs[["slope"]] * log_bb
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      grad_rows <- cbind(1, log_bb)
      se <- lce_se_components(grad_rows, m$Sigma, m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se$se_total, se$se_epi, link,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.parametric_log"]] <- LearnerLCEParametricLog
