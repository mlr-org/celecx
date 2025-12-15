#' @title Quantile Regression Learner with SE Prediction
#'
#' @name mlr_learners_regr.quantile_se
#'
#' @description
#' Wraps a quantile regression learner and converts quantile predictions to SE.
#' Assumes approximate normality to map inter-quantile range to standard error.
#'
#' @details
#' This learner:
#' 1. Trains a base learner that supports quantile predictions
#' 2. predicts lower and upper quantiles
#' 3. SE prediction is the inter-quantile range multiplied by a given factor
#'
#' @section Parameters:
#' * `quantile_response` :: `numeric(1)`\cr
#'   Quantile response to use for the prediction. Initialized to 0.5 (median).
#' * `quantile_lower` :: `numeric(1)`\cr
#'   Lower quantile for SE estimation. Initialized to 0.1 (10th percentile).
#' * `quantile_upper` :: `numeric(1)`\cr
#'   Upper quantile for SE estimation. Initialized to 0.9 (90th percentile).
#' * `se_factor` :: `numeric(1)`\cr
#'   Factor to multiply the inter-quantile range to get the SE. Initialized to 0.5.
#'
#' The initial setup forwards the wrapped learner's response prediction and calculates
#' the SE as half the range from predicted 0.1 to 0.9 quantiles.
#'
#' @section Fields:
#' * `$wrapped` :: [mlr3::LearnerRegr]\\cr
#'   Read-only access to the wrapped base learner.
#'
#' @examples
#' \dontrun{
#' # Requires a quantile regression learner (e.g., from mlr3extralearners)
#' # base_learner <- lrn("regr.ranger")  # Assuming it supports quantiles
#' # learner <- lrn("regr.quantile_se", learner = base_learner)
#' # learner$param_set$set_values(quantile_lower = 0.1, quantile_upper = 0.9)
#'
#' # Train and predict
#' # task <- tsk("mtcars")
#' # learner$train(task)
#' # pred <- learner$predict(task)
#' # pred$se  # Standard errors derived from quantiles
#' }
#'
#' @export
LearnerRegrQuantileSE <- R6Class("LearnerRegrQuantileSE",
  inherit = LearnerRegr,
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

      # Store base learner
      private$.base_learner_obj <- learner$clone(deep = TRUE)

      private$.own_param_set <- ps(
        quantile_response = p_dbl(lower = 0, upper = 1, init = 0.5, tags = c("train", "required")),
        quantile_lower = p_dbl(lower = 0, upper = 1, init = 0.1, tags = c("train", "required")),
        quantile_upper = p_dbl(lower = 0, upper = 1, init = 0.9, tags = c("train", "required")),
        se_factor = p_dbl(lower = 0, init = 0.5, tags = c("predict", "required"))
      )

      super$initialize(
        id = sprintf("regr.quantile_se.%s", learner$id),
        feature_types = learner$feature_types,
        predict_types = c("response", "se"),
        param_set = ps(),  # Temporary, will be replaced by active binding
        packages = c(learner$packages, "mlr3"),
        properties = learner$properties,
        man = "celecx::mlr_learners_regr.quantile_se"
      )

      # Force param_set construction
      private$.param_set <- NULL
    }
  ),

  active = list(
    #' @field wrapped (`LearnerRegr`)\\cr
    #'   Read-only access to the wrapped base learner.
    wrapped = function(val) {
      if (!missing(val) && !identical(val, private$.base_learner_obj)) {
        stop("$wrapped is read-only.")
      }
      private$.base_learner_obj
    },

    #' @field param_set ([paradox::ParamSet])\\cr
    #'   The combined parameter set.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set <- ParamSetCollection$new(list(
          private$.own_param_set,
          private$.base_learner_obj$param_set
        ))
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .base_learner_obj = NULL,
    .own_param_set = NULL,
    .param_set = NULL,

    .base_learner = function(recursive = Inf) {
      if (recursive <= 0) return(self)
      if (!is.null(self$model)) {
        base_model <- private$.base_learner_obj$clone(deep = TRUE)
        base_model$state <- self$model$state
        return(base_model)
      }
      private$.base_learner_obj$base_learner(recursive - 1)
    },

    .train = function(task) {
      base_learner <- private$.base_learner_obj

      base_learner_config <- list(
        predict_type = base_learner$predict_type,
        quantiles = base_learner$quantiles,
        quantile_response = base_learner$quantile_response
      )
      on.exit({
        base_learner$predict_type <- base_learner_config$predict_type
        if (!is.null(base_learner_config$quantiles)) {
          base_learner$quantiles <- base_learner_config$quantiles
        }
        if (!is.null(base_learner_config$quantile_response)) {
          base_learner$quantile_response <- base_learner_config$quantile_response
        }
        base_learner$state <- NULL
      })

      pv <- self$param_set$get_values(tags = "train")
      q_lower <- pv$quantile_lower
      q_upper <- pv$quantile_upper
      q_response <- pv$quantile_response
      if (q_lower >= q_upper) {
        stopf("quantile_lower must be less than quantile_upper")
      }

      base_learner$predict_type <- "quantiles"
      base_learner$quantiles <- c(q_lower, q_upper)
      base_learner$quantile_response <- q_response
      base_learner$train(task)

      # Return base-learner's state plus extra information
      structure(
        list(
          quantiles = c(q_lower, q_upper),
          quantile_response = q_response,
          state = base_learner$state
        ),
        class = "learner_regr_quantile_se_state"
      )
    },

    # Predict using quantiles and convert to SE
    .predict = function(task) {
      base_learner <- private$.base_learner_obj

      pv <- self$param_set$get_values(tags = "predict")
      se_factor <- pv$se_factor

      base_learner_config <- list(
        predict_type = base_learner$predict_type,
        quantiles = base_learner$quantiles,
        quantile_response = base_learner$quantile_response
      )
      on.exit({
        base_learner$predict_type <- base_learner_config$predict_type
        if (!is.null(base_learner_config$quantiles)) {
          base_learner$quantiles <- base_learner_config$quantiles
        }
        if (!is.null(base_learner_config$quantile_response)) {
          base_learner$quantile_response <- base_learner_config$quantile_response
        }
        base_learner$state <- NULL
      })

      base_learner$state <- self$model$state
      base_learner$predict_type <- "quantiles"
      base_learner$quantiles <- self$model$quantiles
      base_learner$quantile_response <- self$model$quantile_response
      pred <- get_private(base_learner)$.predict(task)
      q_matrix <- pred$quantiles

      lower_q <- q_matrix[, 1]
      upper_q <- q_matrix[, 2]
      response <- pred$response

      se <- (upper_q - lower_q) * se_factor

      list(
        response = response,
        se = se
      )
    },

    # Deep clone implementation following FilterEnsemble pattern
    deep_clone = function(name, value) {
      switch(name,
        .base_learner_obj = {
          # Clone base learner deeply and reset param_set to force reconstruction
          private$.param_set <- NULL
          value$clone(deep = TRUE)
        },
        .own_param_set = {
          # Clone own param_set and reset param_set to force reconstruction
          private$.param_set <- NULL
          value$clone(deep = TRUE)
        },
        .param_set = {
          # Don't clone param_set - it will be reconstructed
          NULL
        },
        # Default: just return the value
        value
      )
    }
  )
)

#' @include aaa.R
learners[["regr.quantile_se"]] = LearnerRegrQuantileSE
