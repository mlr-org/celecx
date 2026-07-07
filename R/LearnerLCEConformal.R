#' @title Split-Conformal LCE Learner Wrapper
#'
#' @name mlr_learners_lce.conformal
#'
#' @include LearnerLCEWrapper.R
#'
#' @description
#' Wraps an arbitrary base [LearnerLCE] with a split-conformal procedure
#' that calibrates an absolute-residual prediction band on a hold-out
#' suffix of the training batches.
#'
#' Training proceeds as:
#' 1. The last `n_calibration_batches` batches of the training task are set
#'    aside as the calibration set. The base learner is fit on the remaining
#'    prefix (the "proper" training set).
#' 2. The base learner predicts on the calibration set and per-batch absolute
#'    residuals \eqn{|y_b - \hat y_b|} are collected.
#' 3. The conformal half-width \eqn{q} is set to the
#'    \eqn{\lceil (n_{cal}+1)(1-\alpha)\rceil / n_{cal}} sample quantile of the
#'    calibration residuals (the standard finite-sample correction).
#'
#' At predict time the base learner trained on the proper-train subset is
#' used for the point prediction. Calibration residuals are measured on the
#' task's [lce_link] scale. The `se` column, when requested, is the constant
#' per-row value \eqn{q / z_{1-\alpha/2}} (with \eqn{z} the standard normal
#' quantile and \eqn{q} the link-scale conformal half-width). Multiplying back by
#' \eqn{z_{1-\alpha/2}} recovers the \eqn{(1-\alpha)} conformal half-width on the
#' link scale, so the downstream interpretation `g(response) ± z * se` produces
#' an interval with the chosen conformal coverage. Since the split-conformal band
#' is a total predictive band that does not separate epistemic from aleatoric
#' uncertainty, `se_epistemic` is reported equal to `se`.
#'
#' The `target_reached` predict type reports the realised-reach probability
#' (whether the *observed* `y_b` reaches the target), reading the same total band
#' as a Gaussian on the link scale. This matches the other learners'
#' `target_reached` and [mlr_measures_lce_distributional]'s `lce.reach_brier`.
#'
#' Two consequences of the band not separating epistemic from aleatoric
#' uncertainty: a distributional measure scoring at a `level` other than
#' `1 - alpha` rescales `se` and only attains its nominal coverage when
#' `level == 1 - alpha`; and [lce_batches_to_target] with `crossing = "expected"`
#' (which reads `se_epistemic`) conflates the two and so behaves like an
#' observed-crossing forecast for this learner.
#'
#' Because the LCE batches are naturally ordered, the calibration split is
#' deterministic: the most recent batches go to calibration, matching the
#' "use the freshest history to calibrate the forecaster" intuition.
#'
#' @section Parameters:
#' Combined parameter set merges the wrapper's own parameters with those of
#' the base learner via [paradox::ParamSetCollection]; the base learner's
#' parameters carry the `base.` prefix (e.g. `base.rate_lower`).
#'
#' Own parameters:
#' * `n_calibration_batches` :: `integer(1)`\cr
#'   Number of trailing batches used as the calibration set. Initialized to
#'   `3`. The proper-train set must contain at least one batch.
#' * `alpha` :: `numeric(1)`\cr
#'   Miscoverage level. Initialized to `0.1` (90% conformal coverage).
#'
#' @export
LearnerLCEConformal <- R6Class("LearnerLCEConformal",
  inherit = LearnerLCEWrapper,
  public = list(
    #' @description
    #' Creates a new conformal-wrapped LCE learner.
    #'
    #' @param learner ([LearnerLCE])\cr
    #'   Base LCE learner to wrap.
    initialize = function(learner) {
      super$initialize(
        learner = learner,
        id_prefix = "lce.conformal",
        param_set = ps(
          n_calibration_batches = p_int(lower = 1L, init = 3L,
            tags = c("train", "required")),
          alpha = p_dbl(lower = 1e-6, upper = 1 - 1e-6, init = 0.1,
            tags = c("train", "required"))
        ),
        predict_types = c("response", "se", "quantiles", "target_reached"),
        label = "Split-Conformal LCE",
        man = "celecx::mlr_learners_lce.conformal"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv <- private$.own_param_set$get_values(tags = "train")
      link <- lce_link(task$link)
      n_cal <- pv$n_calibration_batches
      alpha <- pv$alpha

      pb <- lce_train_per_batch(task)
      if (pb$n_batches < n_cal + 1L) {
        stopf("'%s' needs at least %i + 1 batches (%i for calibration plus one for training); got %i",
          self$id, n_cal, n_cal, pb$n_batches)
      }

      calibration_batches <- pb$batch[seq.int(pb$n_batches - n_cal + 1L, pb$n_batches)]
      proper_train_batches <- pb$batch[seq_len(pb$n_batches - n_cal)]

      row_batches <- task$data(cols = task$col_roles$feature)[[1L]]
      row_ids <- task$row_ids
      proper_rows <- row_ids[row_batches %in% proper_train_batches]
      cal_rows <- row_ids[row_batches %in% calibration_batches]

      proper_task <- task$clone(deep = TRUE)
      proper_task$filter(proper_rows)
      cal_task <- task$clone(deep = TRUE)
      cal_task$filter(cal_rows)

      trained <- with_learner_state(private$.base_learner_obj,
        function(l) {
          l$train(proper_task)
          list(state = l$state, cal_pred = l$predict(cal_task))
        }, predict_type = "response")

      cal_pred <- trained$cal_pred
      cal_dt <- data.table(
        batch = cal_task$data(cols = cal_task$col_roles$feature)[[1L]],
        truth = cal_pred$truth,
        response = cal_pred$response
      )
      cal_per_batch <- cal_dt[,
        list(truth = mean(truth), response = mean(response)),
        by = "batch"]
      # Calibrate the band on the link scale, so the reported se is a link-scale SD.
      cal_residuals <- abs(link$transform(cal_per_batch$truth) -
        link$transform(cal_per_batch$response))

      n_cal_actual <- length(cal_residuals)
      q_level <- min(ceiling((n_cal_actual + 1L) * (1 - alpha)) / n_cal_actual, 1)
      q <- unname(stats::quantile(cal_residuals, probs = q_level, type = 1, names = FALSE))

      list(
        base_state = trained$state,
        q = q,
        alpha = alpha,
        n_cal = n_cal_actual,
        n_proper = length(proper_train_batches),
        link = task$link,
        minimize = lce_model_minimize(task),
        last_train_batch = max(as.numeric(task$batch_nrs))
      )
    },

    .predict = function(task) {
      m <- self$model
      response <- with_learner_state(private$.base_learner_obj,
        function(l) l$predict(task),
        state = m$base_state, predict_type = "response")$response
      if (self$predict_type == "response") {
        return(list(response = response))
      }
      z <- stats::qnorm(1 - m$alpha / 2)
      se <- rep(m$q / z, length(response))
      if (self$predict_type == "se") {
        return(list(response = response, se = se, se_epistemic = se))
      }
      # target_reached: realised-reach probability, reading the conformal band as
      # a Gaussian on the link scale (se is the total predictive SD).
      link <- lce_link(m$link)
      pv <- private$.own_param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, link$transform(response), se, se, link,
        probs = pv$quantile_probs %??% lce_default_probs,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.conformal"]] <- LearnerLCEConformal
