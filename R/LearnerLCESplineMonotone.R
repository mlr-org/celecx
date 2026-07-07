#' @title Monotone Spline LCE Learner
#'
#' @name mlr_learners_lce.spline_monotone
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Fits a shape-constrained smoothing spline (monotone increasing or monotone
#' decreasing) to the per-batch surrogate performance via
#' [scam::scam()] with a monotone P-spline basis (`"mpi"` / `"mpd"`).
#' With `direction = "auto"` (the default), the direction is inferred from the
#' task's measure: utility-style targets increase and loss-style targets
#' decrease.
#' This is the smoother, lower-bias analogue of [LearnerLCEIsotonic].
#'
#' Extrapolation beyond the training-batch range linearly extends the
#' last fitted spline segment, so very long-horizon forecasts can drift; use
#' [LearnerLCEParametricExponential] or one of the parametric families when
#' a hard asymptote is needed. The spline is fit on the task's [lce_link] scale.
#'
#' At least five distinct batches are required: `scam`'s monotone basis needs a
#' basis dimension `k` of at least 4, and `k` is clamped to at most
#' `n_batches - 1` to keep the fit identifiable.
#'
#' When `predict_type = "se"` the learner returns the spline's pointwise standard
#' errors (as reported by [scam::predict.scam()]) as the epistemic `se_epistemic`
#' and adds the spline's residual variance back to form the total predictive
#' `se`, both on the link scale.
#'
#' @section Parameters:
#' * `direction` :: `character(1)`\cr
#'   `"auto"` (default), `"increasing"`, or `"decreasing"`. Selects the monotone
#'   P-spline basis (`mpi` / `mpd`) after resolution.
#' * `k` :: `integer(1)` | `NULL`\cr
#'   Basis dimension passed to `scam::s()`. Defaults to `NULL`, which uses
#'   `min(10, n_batches - 1)`. Floored at `4` (the smallest `k` the monotone
#'   basis supports) and clamped to at most `n_batches - 1` at training time to
#'   keep the fit identifiable.
#' * `bs` :: `character(1)`\cr
#'   Spline basis. Restricted to `"mpi"` (monotone increasing) and `"mpd"`
#'   (monotone decreasing) and is set automatically from `direction`.
#'
#' @export
LearnerLCESplineMonotone <- R6Class("LearnerLCESplineMonotone",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        direction = p_fct(c("auto", "increasing", "decreasing"), init = "auto",
          tags = c("train", "required")),
        k = p_int(lower = 4L, special_vals = list(NULL), default = NULL,
          tags = "train")
      )

      super$initialize(
        id = "lce.spline_monotone",
        param_set = param_set,
        predict_types = c("response", "se", "quantiles", "target_reached"),
        feature_types = "integer",
        packages = c("celecx", "scam"),
        label = "Monotone Spline LCE",
        man = "celecx::mlr_learners_lce.spline_monotone"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      link <- lce_link(task$link)
      pb <- lce_train_per_batch(task, link)
      if (pb$n_batches < 5L) {
        # scam's monotone mpi/mpd basis needs k >= 4, and k is clamped to at
        # most n_batches - 1, so at least five distinct batches are required.
        stopf("Need at least five distinct batches to fit '%s' (got %i)",
          self$id, pb$n_batches)
      }
      direction <- lce_resolve_monotone_direction(pv$direction, task, self$id)
      bs <- if (direction == "increasing") "mpi" else "mpd"
      k <- pv$k %??% min(10L, pb$n_batches - 1L)
      k <- min(max(k, 4L), pb$n_batches - 1L)
      df <- data.frame(batch = pb$batch, value = link$transform(pb$value))
      formula <- stats::as.formula(sprintf("value ~ s(batch, k = %i, bs = \"%s\")",
        as.integer(k), bs))
      model <- invoke(scam::scam, formula = formula, data = df)
      list(scam = model, sig2 = model$sig2, direction = direction, link = task$link,
        minimize = lce_model_minimize(task),
        last_train_batch = max(as.numeric(task$batch_nrs)))
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      bb <- lce_predict_batches(task)
      newdata <- data.frame(batch = bb)
      if (self$predict_type == "response") {
        mu <- invoke(scam::predict.scam, object = m$scam, newdata = newdata)
        return(list(response = link$inverse(as.numeric(mu))))
      }
      pred <- invoke(scam::predict.scam, object = m$scam, newdata = newdata,
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
learners[["lce.spline_monotone"]] <- LearnerLCESplineMonotone
