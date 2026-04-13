#' @title tgp Regression Learner
#'
#' @name mlr_learners_regr.tgp
#'
#' @description
#' Bayesian Gaussian process regression.
#' Calls [tgp::bgp()] from package \CRANpkg{tgp}.
#'
#' Predictions return the posterior predictive mean and the square root of the
#' predictive kriging variance as standard error. Several parameters that
#' appear in both [tgp::bgp()] and `predict.tgp` are prefixed with `predict_`
#' to distinguish the predict-time variants (e.g. `predict_BTE`,
#' `predict_R`).
#'
#' The learner always trains without collecting training-location predictive
#' summaries and always predicts with pointwise kriging variances only. The
#' corresponding upstream options are therefore not exposed because their
#' effects are discarded by the mlr3 wrapper.
#'
#' @section Initial Parameter Values:
#' * `verb`: Set to `0` (upstream default is `1`) to suppress progress output.
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
        MAP = p_lgl(default = TRUE, tags = "predict")
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
      with_temp_workdir(invoke(
        tgp::bgp,
        X = task_feature_matrix(task),
        Z = task$truth(),
        pred.n = FALSE,
        krige = FALSE,
        zcov = FALSE,
        Ds2x = FALSE,
        improv = FALSE,
        sens.p = NULL,
        .args = self$param_set$get_values(tags = "train")
      ))
    },

    .predict = function(task) {
      pv <- self$param_set$get_values(tags = "predict")
      predict_args <- compact(list(
        BTE = pv$predict_BTE,
        R = pv$predict_R,
        MAP = pv$MAP,
        pred.n = FALSE,
        krige = TRUE,
        zcov = FALSE,
        trace = FALSE,
        verb = 0L
      ))

      prediction <- with_temp_workdir(invoke(
        get("predict.tgp", envir = asNamespace("tgp")),
        self$model,
        XX = task_feature_matrix(task),
        .args = predict_args
      ))

      variance <- prediction$ZZ.ks2
      if (is.null(variance)) {
        stopf("tgp prediction did not return predictive kriging variances.")
      }

      variance <- as.numeric(variance)
      if (length(variance) != task$nrow) {
        stopf(
          "tgp prediction returned %i kriging variances for %i observations.",
          length(variance), task$nrow
        )
      }

      list(
        response = prediction$ZZ.mean,
        se = prediction_se_from_variance(variance)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.tgp"]] <- LearnerRegrTGP
