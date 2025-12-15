#' @title Bootstrap Ensemble Learner with SE Prediction
#'
#' @name mlr_learners_regr.bootstrap_se
#'
#' @description
#' Wraps any regression learner and trains a bootstrap ensemble.
#' Predictions return mean and SE across bootstrap samples.
#'
#' @details
#' This learner creates a bootstrap ensemble by:
#' 1. Taking `n_bootstrap` bootstrap samples (sampling with replacement)
#' 2. Training the base learner on each sample and storing the trained state
#' 3. During prediction, restoring each state and computing predictions
#' 4. Computing mean and SD of predictions across the ensemble
#'
#' The standard deviation across bootstrap predictions serves as the standard error estimate.
#'
#' The wrapped base learner (`$wrapped`) remains untrained after training the wrapper.
#' Use `$base_learner()` to get a trained clone of the base learner.
#'
#' @section Fields:
#' * `$wrapped` :: [mlr3::LearnerRegr]\cr
#'   Read-only access to the wrapped base learner.
#'
#' @examples
#' \dontrun{
#' # Wrap ranger with bootstrap SE
#' learner <- lrn("regr.bootstrap_se", learner = lrn("regr.ranger"))
#' learner$param_set$set_values(n_bootstrap = 10)
#'
#' # Train on a task
#' task <- tsk("mtcars")
#' learner$train(task)
#'
#' # Predict with SE
#' pred <- learner$predict(task)
#' pred$se  # Standard errors
#' }
#'
#' @export
LearnerRegrBootstrapSE <- R6Class("LearnerRegrBootstrapSE",
  inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr])\cr
    #'   Base learner to bootstrap.
    initialize = function(learner) {
      assert_learner(learner, task_type = "regr")
      private$.base_learner_obj <- learner$clone(deep = TRUE)

      private$.own_param_set <- ps(
        n_bootstrap = p_int(lower = 2L, init = 30L, tags = c("train", "required"))
      )

      super$initialize(
        id = sprintf("regr.bootstrap_se.%s", learner$id),
        feature_types = learner$feature_types,
        predict_types = c("response", "se"),
        param_set = ps(),  # Temporary, will be replaced by active binding
        packages = c(learner$packages, "mlr3"),
        properties = learner$properties,
        man = "celecx::mlr_learners_regr.bootstrap_se"
      )

      private$.param_set <- NULL
    }
  ),

  active = list(
    #' @field wrapped (`LearnerRegr`)\cr
    #'   Read-only access to the wrapped base learner.
    wrapped = function(val) {
      if (!missing(val) && !identical(val, private$.base_learner_obj)) {
        stop("$wrapped is read-only.")
      }
      private$.base_learner_obj
    },

    #' @field param_set ([paradox::ParamSet])\cr
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

    # Method for parent's base_learner() active binding
    .base_learner = function(recursive = Inf) {
      if (recursive <= 0) return(self)
      if (!is.null(self$model)) {
        base_model <- private$.base_learner_obj$clone(deep = TRUE)
        base_model$state <- self$model$base_state
        return(base_model)
      }
      private$.base_learner_obj$base_learner(recursive - 1)
    },

    # Train on bootstrap samples
    .train = function(task) {
      base_learner <- private$.base_learner_obj

      base_learner_config <- list(
        predict_type = base_learner$predict_type
      )
      on.exit({
        base_learner$predict_type <- base_learner_config$predict_type
        base_learner$state <- NULL
      })

      n_bootstrap <- self$param_set$values$n_bootstrap
      train_data <- task$data()
      n_obs <- nrow(train_data)
      target_name <- task$target_names

      # Train bootstrap ensemble and collect states
      bootstrap_states <- map(seq_len(n_bootstrap), function(i) {
        # Bootstrap sample (with replacement)
        boot_idx <- sample.int(n_obs, n_obs, replace = TRUE)
        boot_data <- train_data[boot_idx]

        # Create task for this bootstrap sample
        boot_task <- TaskRegr$new(
          id = sprintf("bootstrap_%d", i),
          backend = boot_data,
          target = target_name
        )

        # Train base learner on bootstrap sample
        base_learner$predict_type <- "response"  # Bootstrap doesn't need SE from base
        base_learner$train(boot_task)

        # Extract and store state
        state <- base_learner$state
        base_learner$state <- NULL
        state
      })

      # Return model with bootstrap states
      structure(
        list(
          n_bootstrap = n_bootstrap,
          base_learner_id = base_learner$id,
          bootstrap_states = bootstrap_states,
          base_state = bootstrap_states[[1]]  # First bootstrap for base_learner()
        ),
        class = "learner_regr_bootstrap_se_state"
      )
    },

    # Predict using bootstrap ensemble
    .predict = function(task) {
      base_learner <- private$.base_learner_obj

      base_learner_config <- list(
        predict_type = base_learner$predict_type
      )
      on.exit({
        base_learner$predict_type <- base_learner_config$predict_type
        base_learner$state <- NULL
      })

      base_learner$predict_type <- "response"
      # Get predictions from each bootstrap state
      predictions <- map(self$model$bootstrap_states, function(state) {
        base_learner$state <- state
        pred <- get_private(base_learner)$.predict(task)
        base_learner$state <- NULL
        pred$response
      })

      # Stack predictions into matrix (rows = observations, cols = bootstrap samples)
      pred_matrix <- do.call(cbind, predictions)

      # Compute mean and SE across bootstrap samples
      mean_pred <- rowMeans(pred_matrix)
      se_pred <- apply(pred_matrix, 1, sd)

      list(
        response = mean_pred,
        se = se_pred
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
learners[["regr.bootstrap_se"]] = LearnerRegrBootstrapSE
