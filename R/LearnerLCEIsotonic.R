#' @title Isotonic LCE Learner
#'
#' @name mlr_learners_lce.isotonic
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Fits a monotone, piecewise-constant learning curve via isotonic regression
#' ([stats::isoreg()]) to the per-batch surrogate performance. With
#' `direction = "auto"` (the default), the direction is inferred from the task's
#' measure: utility-style targets (R², accuracy, ...) increase and loss-style
#' targets (MAE, RMSE, ...) decrease. Set `direction = "increasing"` or
#' `"decreasing"` to override this; the implementation negates the target before
#' fitting for the decreasing case.
#'
#' Predictions outside the observed batch range are extrapolated as
#' constants of the closest endpoint fit, matching the monotone-shape
#' constraint. Within the observed range either piecewise-constant
#' (step function) or piecewise-linear interpolation is used, controlled
#' by `interpolation`. The fit is performed on the task's [lce_link] scale.
#'
#' When `predict_type = "se"` the epistemic `se_epistemic` is a constant
#' link-scale residual standard deviation divided by `sqrt(n_batches)`, and the
#' total predictive `se` adds that residual standard deviation back as the
#' aleatoric spread. The residual standard deviation uses `n_batches - 1` degrees
#' of freedom (it does not discount the degrees of freedom the monotone fit
#' itself consumes) and is constant across batches, so the uncertainty is a
#' coarse estimate that ignores how the fit degrades away from the observed
#' batches.
#'
#' @section Parameters:
#' * `direction` :: `character(1)`\cr
#'   `"auto"` (default), `"increasing"`, or `"decreasing"`.
#' * `interpolation` :: `character(1)`\cr
#'   `"linear"` (default) or `"constant"` (step). Controls within-range
#'   interpolation; out-of-range extrapolation is always constant.
#'
#' @export
LearnerLCEIsotonic <- R6Class("LearnerLCEIsotonic",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        direction = p_fct(c("auto", "increasing", "decreasing"), init = "auto",
          tags = c("train", "required")),
        interpolation = p_fct(c("linear", "constant"), init = "linear",
          tags = c("predict", "required"))
      )

      super$initialize(
        id = "lce.isotonic",
        param_set = param_set,
        predict_types = c("response", "se", "target_reached"),
        feature_types = "integer",
        packages = "stats",
        label = "Isotonic LCE",
        man = "celecx::mlr_learners_lce.isotonic"
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
      value_link <- link$transform(pb$value)
      direction <- lce_resolve_monotone_direction(pv$direction, task, self$id)
      sign <- if (direction == "increasing") 1 else -1
      iso <- stats::isoreg(pb$batch, sign * value_link)
      # isoreg may permute its input; map yf back to the original batch order.
      ord <- iso$ord %??% seq_len(pb$n_batches)
      fitted_in_order <- numeric(pb$n_batches)
      fitted_in_order[ord] <- iso$yf
      fitted_link <- sign * fitted_in_order
      residuals <- value_link - fitted_link
      dispersion <- if (pb$n_batches >= 2L) {
        sqrt(sum(residuals^2) / (pb$n_batches - 1L))
      } else {
        0
      }
      se_epi <- if (pb$n_batches >= 2L) dispersion / sqrt(pb$n_batches) else 0
      list(
        batch = pb$batch,
        fitted_link = fitted_link,
        fitted = link$inverse(fitted_link),
        dispersion = dispersion,
        # Total predictive SD = aleatoric residual spread plus the (coarse,
        # batch-constant) standard error of the fit.
        se_total = sqrt(dispersion^2 + se_epi^2),
        se_epi = se_epi,
        n_batches = pb$n_batches,
        direction = direction,
        link = task$link,
        minimize = lce_model_minimize(task)
      )
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      pv <- self$param_set$get_values(tags = "predict")
      bb <- lce_predict_batches(task)
      # rule = 2: clamp out-of-range x to the nearest endpoint fit (matches
      # the monotone-shape extrapolation we want). Interpolate on the link scale.
      mu <- stats::approx(
        x = m$batch, y = m$fitted_link, xout = bb,
        method = pv$interpolation, rule = 2L, ties = "ordered"
      )$y
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      n <- length(bb)
      lce_distr_predict(self$predict_type, mu, rep(m$se_total, n),
        rep(m$se_epi, n), link, reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.isotonic"]] <- LearnerLCEIsotonic
