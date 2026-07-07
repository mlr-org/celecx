#' @title Bootstrap Ensemble Learner with SE Prediction
#'
#' @name mlr_learners_regr.bootstrap_se
#'
#' @include LearnerRegrWrapper.R
#'
#' @description
#' Wraps any regression learner and trains a bootstrap ensemble.
#' Predictions return mean and SE across bootstrap samples.
#'
#' @details
#' This learner creates a bootstrap ensemble by:
#' 1. Taking `n_bootstrap` bootstrap samples (sampling with replacement)
#' 2. Training the base learner on each sample, storing the trained state
#' 3. During prediction, restoring each state and querying its model
#' 4. Computing mean and SD of predictions across the ensemble
#'
#' The standard deviation across bootstrap predictions serves as the standard error estimate.
#'
#' The bootstrap tasks are rebuilt from the task's feature/target data, so
#' task properties beyond that (observation weights, strata, groups) are not
#' forwarded to the ensemble members and the corresponding base-learner
#' properties are not advertised by the wrapper.
#'
#' The wrapped base learner (`$wrapped`) remains untrained after training the wrapper.
#' Use `$base_learner()` to get a trained clone of the base learner.
#'
#' @section Parameters:
#' The base learner's parameters are exposed with the `base.` prefix
#' (e.g. `base.maxdepth`).
#'
#' Own parameters:
#' * `n_bootstrap` :: `integer(1)`\cr
#'   Number of bootstrap samples. Initialized to `30`.
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
  inherit = LearnerRegrWrapper,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr])\cr
    #'   Base learner to bootstrap.
    initialize = function(learner) {
      super$initialize(
        learner = learner,
        id_prefix = "regr.bootstrap_se",
        param_set = ps(
          n_bootstrap = p_int(lower = 2L, init = 30L, tags = c("train", "required"))
        ),
        # the bootstrap tasks are rebuilt from task$data(), which keeps only
        # features and target; do not advertise properties the wrapper cannot
        # honor (weights, importance, hotstarting, ...)
        properties = intersect(learner$properties, c("missings", "featureless")),
        label = "Bootstrap SE",
        man = "celecx::mlr_learners_regr.bootstrap_se"
      )
    }
  ),

  private = list(
    # Method for parent's base_learner() active binding: reassemble a trained
    # member from the shared base learner and the first stored state.
    .base_learner = function(recursive = Inf) {
      if (recursive <= 0) return(self)
      if (!is.null(self$model)) {
        base <- private$.base_learner_obj$clone(deep = TRUE)
        base$predict_type <- "response"
        base$state <- self$model$bootstrap_states[[1L]]
        return(base)
      }
      private$.base_learner_obj$base_learner(recursive - 1)
    },

    # Train on bootstrap samples, storing only the member states.
    .train = function(task) {
      pv <- private$.own_param_set$get_values(tags = "train")
      n_bootstrap <- pv$n_bootstrap
      train_data <- task$data()
      n_obs <- nrow(train_data)
      target_name <- task$target_names

      bootstrap_states <- map(seq_len(n_bootstrap), function(i) {
        boot_idx <- sample.int(n_obs, n_obs, replace = TRUE)
        boot_task <- TaskRegr$new(
          id = sprintf("bootstrap_%d", i),
          backend = train_data[boot_idx],
          target = target_name
        )
        # bootstrap doesn't need SE from the base learner
        with_learner_state(private$.base_learner_obj,
          function(l) l$train(boot_task)$state, predict_type = "response")
      })

      structure(
        list(
          n_bootstrap = n_bootstrap,
          bootstrap_states = bootstrap_states
        ),
        class = "learner_regr_bootstrap_se_state"
      )
    },

    # Predict using the bootstrap ensemble: each stored state is injected into
    # the shared base learner and queried via predict_newdata_fast(), which
    # also works when mlr3 hands the wrapper a lightweight fake task
    # (predict_newdata_fast on the wrapper itself, as used by mlr3mbo
    # surrogates).
    .predict = function(task) {
      newdata <- task$data()
      predictions <- map(self$model$bootstrap_states, function(state) {
        with_learner_state(private$.base_learner_obj,
          function(l) l$predict_newdata_fast(newdata)$response,
          state = state, predict_type = "response")
      })

      # Stack predictions into matrix (rows = observations, cols = bootstrap samples)
      pred_matrix <- do.call(cbind, predictions)

      list(
        response = rowMeans(pred_matrix),
        se = apply(pred_matrix, 1L, stats::sd)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.bootstrap_se"]] = LearnerRegrBootstrapSE
