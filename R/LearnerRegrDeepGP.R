#' @title deepgp Regression Learner
#'
#' @name mlr_learners_regr.deepgp
#'
#' @description
#' Two-layer deep Gaussian process regression via [deepgp::fit_two_layer()].
#'
#' Predictions use [stats::predict()] for fitted `"dgp2"` objects and return
#' the posterior predictive mean together with the square root of the reported
#' predictive variance.
#'
#' @details
#' If `D` is left unset, `deepgp::fit_two_layer()` defaults it to the number of
#' input columns.
#'
#' @export
LearnerRegrDeepGP <- R6Class("LearnerRegrDeepGP",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        dydx = p_uty(default = NULL, tags = "train"),
        nmcmc = p_int(lower = 1L, default = 10000L, tags = "train"),
        D = p_int(lower = 1L, tags = "train"),
        monowarp = p_uty(default = FALSE, tags = "train"),
        pmx = p_lgl(default = FALSE, tags = "train"),
        verb = p_lgl(default = TRUE, init = FALSE, tags = "train"),
        w_0 = p_uty(default = NULL, tags = "train"),
        theta_y_0 = p_dbl(lower = 0, default = 0.01, tags = "train"),
        theta_w_0 = p_uty(default = 0.1, tags = "train"),
        g_0 = p_dbl(lower = 0, default = 0.001, tags = "train"),
        true_g = p_dbl(lower = 0, tags = "train"),
        v = p_dbl(
          lower = 0,
          default = 2.5,
          tags = "train",
          depends = quote(cov == "matern")
        ),
        settings = p_uty(default = NULL, tags = "train"),
        cov = p_fct(c("matern", "exp2"), default = "matern", tags = "train"),
        vecchia = p_lgl(default = FALSE, tags = "train"),
        m = p_int(lower = 1L, tags = "train"),
        ord = p_uty(default = NULL, tags = "train"),
        train_cores = p_int(lower = 1L, tags = "train"),
        grad = p_lgl(default = FALSE, tags = "predict"),
        store_latent = p_lgl(default = FALSE, tags = "predict"),
        mean_map = p_lgl(default = TRUE, tags = "predict"),
        EI = p_lgl(default = FALSE, tags = "predict"),
        entropy_limit = p_dbl(tags = "predict"),
        predict_cores = p_int(lower = 1L, default = 1L, tags = "predict")
      )

      super$initialize(
        id = "regr.deepgp",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "deepgp"),
        label = "deepgp Two-Layer DGP",
        man = "celecx::mlr_learners_regr.deepgp"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      train_args <- remove_named(pv, "train_cores")
      if (!is.null(pv$train_cores)) {
        train_args$cores <- pv$train_cores
      }

      invoke(
        deepgp::fit_two_layer,
        x = task_feature_matrix(task),
        y = task$truth(),
        .args = train_args
      )
    },

    .predict = function(task) {
      pv <- self$param_set$get_values(tags = "predict")
      predict_args <- remove_named(pv, "predict_cores")
      if (!is.null(pv$predict_cores)) {
        predict_args$cores <- pv$predict_cores
      }
      predict_args$lite <- TRUE
      predict_args$return_all <- TRUE

      prediction <- invoke(
        stats::predict,
        self$model,
        x_new = task_feature_matrix(task),
        .args = predict_args
      )

      list(
        response = prediction$mean,
        se = prediction_se_from_variance(deepgp_prediction_variance(prediction))
      )
    }
  )
)

#' @include aaa.R
learners[["regr.deepgp"]] <- LearnerRegrDeepGP
