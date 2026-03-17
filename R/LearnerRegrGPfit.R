#' @title GPfit Regression Learner
#'
#' @name mlr_learners_regr.gpfit
#'
#' @description
#' Gaussian process regression via [GPfit::GP_fit()].
#'
#' This learner predicts the posterior mean and returns the square root of the
#' package's predictive mean squared error as `"se"`.
#'
#' `GPfit` assumes that inputs are scaled to the unit hypercube.
#'
#' @export
LearnerRegrGPfit <- R6Class("LearnerRegrGPfit",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        control = p_uty(
          default = quote(c(200 * d, 80 * d, 2 * d)),
          tags = "train"
        ),
        nug_thres = p_dbl(lower = 0, default = 20, tags = "train"),
        trace = p_lgl(default = FALSE, tags = "train"),
        maxit = p_int(lower = 1L, default = 100L, tags = "train"),
        corr_type = p_fct(
          levels = c("exponential", "matern"),
          default = "exponential",
          tags = "train"
        ),
        corr_power = p_dbl(
          lower = 1,
          upper = 2,
          default = 1.95,
          tags = "train",
          depends = quote(corr_type == "exponential")
        ),
        corr_nu = p_dbl(
          lower = 0,
          default = 2.5,
          tags = "train",
          depends = quote(corr_type == "matern")
        ),
        optim_start = p_uty(default = NULL, tags = "train"),
        M = p_int(lower = 1L, default = 1L, tags = "predict")
      )

      super$initialize(
        id = "regr.gpfit",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "GPfit"),
        label = "GPfit",
        man = "celecx::mlr_learners_regr.gpfit"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      train_args <- remove_named(pv, c("corr_type", "corr_power", "corr_nu"))

      corr_args <- pv[c("corr_type", "corr_power", "corr_nu")]
      if (any(!vapply(corr_args, is.null, logical(1L)))) {
        corr_type <- corr_args$corr_type %??%
          self$param_set$default$corr_type
        corr <- list(type = corr_type)

        if (corr_type == "exponential") {
          corr$power <- corr_args$corr_power %??%
            self$param_set$default$corr_power
        } else {
          corr$nu <- corr_args$corr_nu %??%
            self$param_set$default$corr_nu
        }

        train_args$corr <- corr
      }

      invoke(
        GPfit::GP_fit,
        X = task_feature_matrix(task),
        Y = task$truth(),
        .args = train_args
      )
    },

    .predict = function(task) {
      prediction <- invoke(
        stats::predict,
        self$model,
        xnew = task_feature_matrix(task),
        .args = self$param_set$get_values(tags = "predict")
      )

      list(
        response = prediction$Y_hat,
        se = prediction_se_from_variance(prediction$MSE)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.gpfit"]] <- LearnerRegrGPfit
