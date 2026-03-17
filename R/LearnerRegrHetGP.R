#' @title hetGP Regression Learner
#'
#' @name mlr_learners_regr.hetgp
#'
#' @description
#' Gaussian process regression via [hetGP::mleHetGP()].
#'
#' Predictions return the posterior mean together with the square root of the
#' full predictive variance `sd2 + nugs`.
#'
#' @export
LearnerRegrHetGP <- R6Class("LearnerRegrHetGP",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        lower = p_uty(default = NULL, tags = "train"),
        upper = p_uty(default = NULL, tags = "train"),
        noiseControl = p_uty(
          default = list(
            k_theta_g_bounds = c(1, 100),
            g_max = 100,
            g_bounds = c(1e-06, 1)
          ),
          tags = "train"
        ),
        settings = p_uty(
          default = list(
            linkThetas = "joint",
            logN = TRUE,
            initStrategy = "residuals",
            checkHom = TRUE,
            penalty = TRUE,
            trace = 0,
            return.matrices = TRUE,
            return.hom = FALSE,
            factr = 1e+09
          ),
          tags = "train"
        ),
        covtype = p_fct(
          levels = c("Gaussian", "Matern5_2", "Matern3_2"),
          default = "Gaussian",
          tags = "train"
        ),
        maxit = p_int(lower = 1L, default = 100L, tags = "train"),
        known = p_uty(default = NULL, tags = "train"),
        init = p_uty(default = NULL, tags = "train"),
        eps = p_dbl(
          lower = 0,
          default = sqrt(.Machine$double.eps),
          tags = "train"
        ),
        noise.var = p_lgl(default = FALSE, tags = "predict")
      )

      super$initialize(
        id = "regr.hetgp",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "hetGP"),
        label = "hetGP",
        man = "celecx::mlr_learners_regr.hetgp"
      )
    }
  ),
  private = list(
    .train = function(task) {
      invoke(
        hetGP::mleHetGP,
        X = task_feature_matrix(task),
        Z = task$truth(),
        .args = self$param_set$get_values(tags = "train")
      )
    },

    .predict = function(task) {
      prediction <- invoke(
        stats::predict,
        self$model,
        x = task_feature_matrix(task),
        .args = self$param_set$get_values(tags = "predict")
      )

      list(
        response = prediction$mean,
        se = prediction_se_from_variance(prediction$sd2 + prediction$nugs)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.hetgp"]] <- LearnerRegrHetGP
