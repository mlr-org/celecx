#' @title tgp Regression Learner
#'
#' @name mlr_learners_regr.tgp
#'
#' @description
#' Bayesian Gaussian process regression via [tgp::bgp()].
#'
#' The learner uses the package's prediction routine [stats::predict()] for
#' fitted `"tgp"` objects and returns the posterior predictive mean together
#' with the square root of the predictive variance.
#'
#' @export
LearnerRegrTGP <- R6Class("LearnerRegrTGP",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        meanfn = p_fct(
          levels = c("linear", "constant"),
          default = "linear",
          tags = "train"
        ),
        bprior = p_fct(
          levels = c("bflat", "b0", "bmle", "b0not", "bmzt", "bmznot"),
          default = "bflat",
          tags = "train"
        ),
        corr = p_fct(
          levels = c("expsep", "exp", "matern", "sim"),
          default = "expsep",
          tags = "train"
        ),
        BTE = p_uty(default = c(1000L, 4000L, 2L), tags = "train"),
        R = p_int(lower = 1L, default = 1L, tags = "train"),
        m0r1 = p_lgl(default = TRUE, tags = "train"),
        itemps = p_uty(default = NULL, tags = "train"),
        pred.n = p_lgl(default = TRUE, init = FALSE, tags = "train"),
        krige = p_lgl(default = TRUE, init = FALSE, tags = "train"),
        zcov = p_lgl(default = FALSE, tags = "train"),
        Ds2x = p_lgl(default = FALSE, tags = "train"),
        improv = p_uty(default = FALSE, tags = "train"),
        sens.p = p_uty(default = NULL, tags = "train"),
        nu = p_dbl(
          lower = 0,
          default = 1.5,
          tags = "train",
          depends = quote(corr == "matern")
        ),
        trace = p_lgl(default = FALSE, tags = "train"),
        verb = p_int(lower = 0L, upper = 4L, default = 1L, init = 0L, tags = "train"),
        predict_BTE = p_uty(default = c(0L, 1L, 1L), tags = "predict"),
        predict_R = p_int(lower = 1L, default = 1L, tags = "predict"),
        MAP = p_lgl(default = TRUE, tags = "predict"),
        predict_pred.n = p_lgl(default = TRUE, init = FALSE, tags = "predict"),
        predict_zcov = p_lgl(default = FALSE, tags = "predict"),
        predict_trace = p_lgl(default = FALSE, tags = "predict"),
        predict_verb = p_int(lower = 0L, upper = 4L, default = 0L, tags = "predict")
      )

      super$initialize(
        id = "regr.tgp",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "tgp"),
        label = "tgp Bayesian GP",
        man = "celecx::mlr_learners_regr.tgp"
      )
    }
  ),
  private = list(
    .train = function(task) {
      invoke(
        tgp::bgp,
        X = task_feature_matrix(task),
        Z = task$truth(),
        .args = self$param_set$get_values(tags = "train")
      )
    },

    .predict = function(task) {
      predict_args <- rename_prefixed_params(
        self$param_set$get_values(tags = "predict"),
        c(predict_BTE = "BTE", predict_R = "R", "predict_pred.n" = "pred.n",
          predict_zcov = "zcov", predict_trace = "trace", predict_verb = "verb")
      )

      prediction <- invoke(
        get("predict.tgp", envir = asNamespace("tgp")),
        self$model,
        XX = task_feature_matrix(task),
        .args = predict_args
      )

      variance <- tgp_prediction_variance(prediction, task$nrow)
      list(
        response = prediction$ZZ.mean,
        se = prediction_se_from_variance(variance)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.tgp"]] <- LearnerRegrTGP
