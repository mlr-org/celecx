#' @title Learning Curve Extrapolation Learner
#'
#' @name LearnerLCE
#'
#' @include aaa.R
#'
#' @description
#' Abstract base class for learners that extrapolate the surrogate-model quality
#' trajectory of an active-learning run. Subclasses fit a model on a [TaskLCE]
#' and produce point and (optionally) distributional predictions of the
#' surrogate's performance at unseen `batch_nr` values.
#'
#' The supported predict types are:
#'
#' * `"response"`: the point forecast (predictive median on the natural scale).
#' * `"se"`: additionally the total predictive standard error `se` and the
#'   epistemic standard error of the mean `se_epistemic`, both on the task's
#'   [lce_link] scale (see [PredictionLCE]).
#' * `"quantiles"`: a matrix of predictive quantiles at the probabilities given
#'   by the `quantile_probs` parameter.
#' * `"samples"`: a matrix of predictive draws (joint sample paths).
#' * `"target_reached"`: a matrix of reach probabilities at the targets given by
#'   the `reach_target` parameter; needs the task's optimization direction (from
#'   the task `measure`, or from a directed codomain for best-so-far tasks).
#'
#' Subclasses that model the curve as Gaussian on the link scale assemble their
#' predictions with `lce_distr_predict()`; subclasses that produce explicit
#' predictive draws use `lce_samples_predict()`.
#'
#' @section Parameters:
#' Depending on the supported predict types, the following parameters are added
#' automatically:
#' * `quantile_probs` :: `numeric()`\cr
#'   Probabilities for the `"quantiles"` predict type. Defaults to
#'   `c(0.05, 0.25, 0.5, 0.75, 0.95)`.
#' * `reach_target` :: `numeric()`\cr
#'   Target value(s) for the `"target_reached"` predict type. No default.
#'
#' @export
LearnerLCE <- R6Class("LearnerLCE",
  inherit = Learner,
  public = list(
    #' @description
    #' Creates a new LCE learner.
    #'
    #' @param id (`character(1)`)\cr
    #'   Learner id.
    #' @param param_set ([paradox::ParamSet]).
    #' @param predict_types (`character()`)\cr
    #'   One or more of `"response"`, `"se"`, `"quantiles"`, `"samples"`,
    #'   `"target_reached"`.
    #' @param feature_types (`character()`)\cr
    #'   Supported feature types. Defaults to `"integer"` (the type of `batch_nr`).
    #' @param properties (`character()`).
    #' @param packages (`character()`).
    #' @param label (`character(1)`).
    #' @param man (`character(1)`).
    initialize = function(id, param_set = ps(), predict_types = "response",
        feature_types = "integer", properties = character(0),
        packages = character(0), label = NA_character_, man = NA_character_) {
      extra <- lce_predict_type_params(predict_types)
      if (length(extra$ids())) param_set <- c(param_set, extra)
      super$initialize(id = id, task_type = "lce", param_set = param_set,
        predict_types = predict_types, feature_types = feature_types,
        properties = properties, packages = packages, label = label, man = man)
    }
  )
)

# ParamSet fragment with the parameters needed for the distributional predict
# types a learner declares. `samples` learners source their draw count from
# their own parameters (e.g. n_bootstrap, n_restarts), so no parameter is added
# for it here.
lce_predict_type_params <- function(predict_types) {
  params <- list()
  if ("quantiles" %chin% predict_types) {
    params$quantile_probs <- p_uty(default = lce_default_probs, tags = "predict",
      custom_check = function(x) {
        check_numeric(x, lower = 0, upper = 1, any.missing = FALSE,
          min.len = 1L, sorted = TRUE)
      })
  }
  if ("target_reached" %chin% predict_types) {
    params$reach_target <- p_uty(tags = "predict",
      custom_check = function(x) {
        check_numeric(x, any.missing = FALSE, min.len = 1L)
      })
  }
  invoke(ps, .args = params)
}

# Optimization direction (minimize?) for a TaskLCE, for predict types / learner
# types whose meaning depends on it. The task's measure takes precedence;
# without one, the direction of the codomain's single minimize/maximize-tagged
# target is used (the case for best-so-far tasks from task_lce_best_so_far()).
# Errors if neither determines a direction.
lce_task_minimize <- function(task, what = "this operation") {
  measure <- task$measure
  if (!is.null(measure)) {
    minimize <- measure$minimize
    if (is.na(minimize)) {
      stopf("measure '%s' has an undefined optimization direction", measure$id)
    }
    return(minimize)
  }
  minimize <- lce_codomain_minimize(task$codomain)
  if (is.na(minimize)) {
    stopf(paste0("%s needs the task to carry a measure (or a codomain with a single ",
      "minimize/maximize target) so the optimization direction is known"), what)
  }
  minimize
}
