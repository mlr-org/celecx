#' @title GAM LCE Learner
#'
#' @name mlr_learners_lce.gam
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Fits an unconstrained smoothing spline to the per-batch performance via
#' [mgcv::gam()] with a single smooth `s(batch_nr, k = k, bs = bs)`, estimated
#' by REML. This is the classic "GAM extrapolation with confidence bands"
#' workflow for learning-curve extrapolation. Unlike
#' [LearnerLCESplineMonotone] it imposes no monotonicity, so it can also track
#' non-monotone curves -- at the price of less stable extrapolation.
#'
#' The curve is fit on the task's [lce_link] scale. Extrapolation beyond the
#' training-batch range extends the spline basis (linearly beyond the boundary
#' knots for the `"cr"` basis), so long-horizon forecasts can drift; the
#' pointwise standard errors grow accordingly.
#'
#' When `predict_type = "se"` the learner returns the spline's pointwise
#' standard errors (as reported by [mgcv::predict.gam()]) as the epistemic
#' `se_epistemic` and adds the residual variance back to form the total
#' predictive `se`, both on the link scale.
#'
#' At least four distinct batches are required; `k` is clamped to at most
#' `n_batches - 1` to keep the fit identifiable.
#'
#' @section Parameters:
#' * `k` :: `integer(1)` | `NULL`\cr
#'   Basis dimension passed to `mgcv::s()`. Defaults to `NULL`, which uses
#'   `min(6, n_batches - 1)`. Floored at `3` and clamped to at most
#'   `n_batches - 1` at training time.
#' * `bs` :: `character(1)`\cr
#'   Spline basis: `"cr"` (cubic regression spline, default; extrapolates
#'   linearly beyond the boundary knots) or `"tp"` (thin plate).
#'
#' @export
LearnerLCEGam <- R6Class("LearnerLCEGam",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        k = p_int(lower = 3L, special_vals = list(NULL), default = NULL,
          tags = "train"),
        bs = p_fct(c("cr", "tp"), init = "cr", tags = c("train", "required"))
      )

      super$initialize(
        id = "lce.gam",
        param_set = param_set,
        predict_types = c("response", "se", "quantiles", "target_reached"),
        feature_types = "integer",
        packages = c("celecx", "mgcv"),
        label = "GAM LCE",
        man = "celecx::mlr_learners_lce.gam"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      link <- lce_link(task$link)
      pb <- lce_train_per_batch(task, link)
      if (pb$n_batches < 4L) {
        stopf("Need at least four distinct batches to fit '%s' (got %i)",
          self$id, pb$n_batches)
      }
      k <- pv$k %??% min(6L, pb$n_batches - 1L)
      k <- min(max(k, 3L), pb$n_batches - 1L)
      df <- data.frame(batch = pb$batch, value = link$transform(pb$value))
      formula <- stats::as.formula(sprintf("value ~ s(batch, k = %i, bs = \"%s\")",
        as.integer(k), pv$bs))
      model <- invoke(mgcv::gam, formula = formula, data = df, method = "REML")
      list(gam = model, sig2 = model$sig2, link = task$link,
        minimize = lce_model_minimize(task),
        last_train_batch = max(as.numeric(task$batch_nrs)))
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      newdata <- data.frame(batch = lce_predict_batches(task))
      if (self$predict_type == "response") {
        mu <- invoke(mgcv::predict.gam, object = m$gam, newdata = newdata)
        return(list(response = link$inverse(as.numeric(mu))))
      }
      pred <- invoke(mgcv::predict.gam, object = m$gam, newdata = newdata,
        se.fit = TRUE)
      mu <- as.numeric(pred$fit)
      se_epi <- as.numeric(pred$se.fit)
      se_total <- sqrt(se_epi^2 + m$sig2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se_total, se_epi, link,
        probs = pv$quantile_probs %??% lce_default_probs,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.gam"]] <- LearnerLCEGam
