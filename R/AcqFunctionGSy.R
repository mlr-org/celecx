#' @title GSy Acquisition Function
#'
#' @description
#' Greedy-sampling-in-output-space acquisition function.
#'
#' The score is the minimum absolute distance between a candidate prediction and
#' an already observed target value.
#'
#' @export
#' @family Acquisition Function
AcqFunctionGSy <- R6Class("AcqFunctionGSy",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new GSy acquisition function.
    #'
    #' @param surrogate (`NULL` | [mlr3mbo::SurrogateLearner])\cr
    #'   Surrogate used for mean predictions.
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      super$initialize(
        id = "acq_gsy",
        surrogate = surrogate,
        requires_predict_type_se = FALSE,
        surrogate_class = "SurrogateLearner",
        direction = "maximize",
        label = "GSy Acquisition Function",
        man = "celecx::AcqFunctionGSy"
      )
    }
  ),

  private = list(
    .fun = function(xdt) {
      p <- self$surrogate$predict(xdt)
      y_obs <- self$archive$data[[self$surrogate$cols_y]]

      score <- apply(abs(outer(p$mean, y_obs, "-")), 1L, min)
      data.table(acq_gsy = score)
    }
  )
)

mlr_acqfunctions$add("gsy", AcqFunctionGSy)
