#' @title Rolling Slope LCE Learner
#'
#' @name mlr_learners_lce.rolling_slope
#'
#' @include LearnerLCE.R
#'
#' @description
#' Baseline [LearnerLCE] that fits a straight line through the most recent
#' `window` per-batch training performances and linearly extrapolates it to
#' the requested test batches.
#'
#' This is a stronger "no-curvature" baseline than the constant predictors of
#' [LearnerLCEFeatureless] and matches the "rolling slope" baseline from the
#' celecx research plan. The line is fit on the task's [lce_link] scale.
#'
#' When `predict_type = "se"`, the standard errors come from the ordinary
#' least-squares fit: `se_epistemic` is the SD of the fitted mean (so it *grows*
#' as the requested batch moves away from the window, reflecting extrapolation
#' uncertainty) and `se` adds the residual variance back to form the total
#' predictive SD. Both are on the link scale and are `NA` when fewer than three
#' window batches make the residual variance undefined.
#'
#' @section Parameters:
#' * `window` :: `integer(1)`\cr
#'   Number of most recent batches used for the slope fit. Initialized to `5`.
#'   The learner uses `min(window, n_training_batches)` batches.
#'
#' @export
LearnerLCERollingSlope <- R6Class("LearnerLCERollingSlope",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        window = p_int(lower = 2L, init = 5L, tags = c("train", "required"))
      )

      super$initialize(
        id = "lce.rolling_slope",
        param_set = param_set,
        predict_types = c("response", "se", "quantiles", "target_reached"),
        feature_types = "integer",
        label = "Rolling Slope LCE",
        man = "celecx::mlr_learners_lce.rolling_slope"
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
      idx <- seq.int(from = max(1L, pb$n_batches - pv$window + 1L), to = pb$n_batches)
      fit <- lce_fit_ols(pb$batch[idx], link$transform(pb$value[idx]))

      list(
        coefficients = fit$coefficients,
        sigma2 = fit$sigma2,
        Sigma = fit$Sigma,
        window_used = length(idx),
        link = task$link,
        minimize = lce_model_minimize(task),
        last_train_batch = max(as.numeric(task$batch_nrs))
      )
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      bb <- lce_predict_batches(task)
      coefs <- m$coefficients
      mu <- coefs[["intercept"]] + coefs[["slope"]] * bb
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      grad_rows <- cbind(1, bb)
      se <- lce_se_components(grad_rows, m$Sigma, m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se$se_total, se$se_epi, link,
        probs = pv$quantile_probs %??% lce_default_probs,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.rolling_slope"]] <- LearnerLCERollingSlope
