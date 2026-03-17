#' @title Learning Curve Extrapolation Learner Base Class
#'
#' @description
#' Base class for learning curve extrapolation learners that operate on
#' [TaskLCE] objects.
#' Extends [mlr3::LearnerRegr].
#'
#' @details
#' This is a convenience base class. Standard regression learners
#' (e.g., `lrn("regr.ranger")`) also work directly with [TaskLCE]; they simply
#' see `n_evals` as a regular feature.
#'
#' Subclasses that are LCE-aware (e.g., [LearnerLCEExtrapolator]) inherit from
#' this class and can access the task's order column and direction.
#'
#' The learner keeps `task_type = "regr"` so that standard regression
#' measures (e.g., `msr("regr.mse")`) remain compatible.
#'
#' @export
LearnerLCE = R6Class("LearnerLCE",
  inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new LearnerLCE.
    #'
    #' @param id (`character(1)`)\cr
    #'   Learner identifier.
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Parameter set.
    #' @param predict_types (`character()`)\cr
    #'   Supported predict types.
    #' @param feature_types (`character()`)\cr
    #'   Supported feature types.
    #' @param properties (`character()`)\cr
    #'   Learner properties.
    #' @param packages (`character()`)\cr
    #'   Required packages.
    #' @param label (`character(1)`)\cr
    #'   Human-readable label.
    #' @param man (`character(1)`)\cr
    #'   Manual page reference.
    initialize = function(id, param_set = ps(),
        predict_types = "response",
        feature_types = character(),
        properties = character(),
        packages = character(),
        label = NA_character_,
        man = NA_character_) {
      super$initialize(
        id = id,
        param_set = param_set,
        predict_types = predict_types,
        feature_types = feature_types,
        properties = properties,
        packages = packages,
        label = label,
        man = man
      )
    }
  ),

  private = list(
    # Helper to get the order column name from a task.
    .order_col = function(task) {
      order_cols = task$col_roles$order
      if (length(order_cols) == 0L) {
        stopf("Task '%s' has no order column", task$id)
      }
      order_cols
    }
  )
)
