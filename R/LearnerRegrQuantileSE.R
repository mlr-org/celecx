#' @title Quantile Regression Learner with SE Prediction
#'
#' @name mlr_learners_regr.quantile_se
#'
#' @include LearnerRegrWrapper.R
#'
#' @description
#' Wraps a quantile regression learner and converts quantile predictions to SE.
#'
#' @details
#' This learner:
#' 1. Trains a base learner that supports quantile predictions
#' 2. predicts lower and upper quantiles
#' 3. SE prediction is the inter-quantile range multiplied by a factor
#'
#' @section Parameters:
#' The base learner's parameters are exposed with the `base.` prefix.
#'
#' Own parameters:
#' * `quantile_response` :: `numeric(1)`\cr
#'   Quantile response to use for the prediction. Initialized to 0.5 (median).
#' * `quantile_lower` :: `numeric(1)`\cr
#'   Lower quantile for SE estimation. Initialized to 0.1 (10th percentile).
#' * `quantile_upper` :: `numeric(1)`\cr
#'   Upper quantile for SE estimation. Initialized to 0.9 (90th percentile).
#' * `se_factor` :: `numeric(1)` | `NULL`\cr
#'   Factor to multiply the inter-quantile range to get the SE. The default
#'   `NULL` uses the normal-consistent factor
#'   `1 / (qnorm(quantile_upper) - qnorm(quantile_lower))`, so that under an
#'   approximately Gaussian predictive the SE estimates the predictive SD.
#'
#' @section Fields:
#' * `$wrapped` :: [mlr3::LearnerRegr]\cr
#'   Read-only access to the wrapped base learner.
#'
#' @export
LearnerRegrQuantileSE <- R6Class("LearnerRegrQuantileSE",
  inherit = LearnerRegrWrapper,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr])\cr
    #'   Base quantile learner. Must support `predict_type = "quantiles"`.
    initialize = function(learner) {
      assert_learner(learner, task_type = "regr")
      if (!"quantiles" %in% learner$predict_types) {
        stopf("Learner '%s' does not support quantile predictions", learner$id)
      }

      super$initialize(
        learner = learner,
        id_prefix = "regr.quantile_se",
        param_set = ps(
          quantile_response = p_dbl(lower = 0, upper = 1, init = 0.5, tags = c("train", "required")),
          quantile_lower = p_dbl(lower = 0, upper = 1, init = 0.1, tags = c("train", "required")),
          quantile_upper = p_dbl(lower = 0, upper = 1, init = 0.9, tags = c("train", "required")),
          se_factor = p_dbl(lower = 0, special_vals = list(NULL), default = NULL, tags = "predict")
        ),
        # the wrapper trains the base learner on the original task, so
        # data-related base properties survive; accessor-style properties
        # (importance, oob_error, hotstarting) are not forwarded
        properties = intersect(learner$properties, c("missings", "featureless", "weights")),
        label = "Quantile SE",
        man = "celecx::mlr_learners_regr.quantile_se"
      )
    }
  ),

  private = list(
    # Method for parent's base_learner() active binding: reassemble the trained
    # member from the shared base learner and the stored state.
    .base_learner = function(recursive = Inf) {
      if (recursive <= 0) return(self)
      if (!is.null(self$model)) {
        base <- private$.base_learner_obj$clone(deep = TRUE)
        base$predict_type <- "quantiles"
        base$quantiles <- self$model$quantiles
        base$quantile_response <- self$model$quantile_response
        base$state <- self$model$base_state
        return(base)
      }
      private$.base_learner_obj$base_learner(recursive - 1)
    },

    .train = function(task) {
      pv <- private$.own_param_set$get_values(tags = "train")
      if (pv$quantile_lower >= pv$quantile_upper) {
        stopf("quantile_lower must be less than quantile_upper")
      }

      base_state <- with_learner_state(private$.base_learner_obj,
        function(l) l$train(task)$state,
        predict_type = "quantiles",
        quantiles = c(pv$quantile_lower, pv$quantile_upper),
        quantile_response = pv$quantile_response)

      structure(
        list(
          quantiles = c(pv$quantile_lower, pv$quantile_upper),
          quantile_response = pv$quantile_response,
          base_state = base_state
        ),
        class = "learner_regr_quantile_se_state"
      )
    },

    # Predict using quantiles and convert to SE. The stored state is injected
    # into the shared base learner (re-applying the quantile configuration it
    # was trained with) and queried via predict_newdata_fast(), so this also
    # works when mlr3 hands the wrapper a lightweight fake task
    # (predict_newdata_fast on the wrapper itself, as used by mlr3mbo
    # surrogates).
    .predict = function(task) {
      m <- self$model
      pv <- private$.own_param_set$get_values(tags = "predict")

      pred <- with_learner_state(private$.base_learner_obj,
        function(l) l$predict_newdata_fast(task$data()),
        state = m$base_state, predict_type = "quantiles",
        quantiles = m$quantiles, quantile_response = m$quantile_response)
      q_matrix <- pred$quantiles
      # select by probability, not position: mlr3 unions quantile_response into
      # the predicted quantiles, so the column layout is not c(lower, upper)
      probs <- attr(q_matrix, "probs")
      lower_q <- q_matrix[, match(m$quantiles[[1L]], probs)]
      upper_q <- q_matrix[, match(m$quantiles[[2L]], probs)]
      response <- pred$response %??% q_matrix[, match(m$quantile_response, probs)]

      se_factor <- pv$se_factor %??%
        (1 / (stats::qnorm(m$quantiles[[2L]]) - stats::qnorm(m$quantiles[[1L]])))

      list(
        response = response,
        se = (upper_q - lower_q) * se_factor
      )
    }
  )
)

#' @include aaa.R
learners[["regr.quantile_se"]] = LearnerRegrQuantileSE
