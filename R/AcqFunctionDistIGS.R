#' @title Distance-Aware iGS Acquisition Function
#'
#' @include AcqFunctionDist.R
#'
#' @description
#' Improved greedy-sampling acquisition function using an [ALDistance].
#'
#' The score is the minimum, over already evaluated reference points, of the
#' product of input-space distance and output-space distance.
#'
#' @export
#' @family Acquisition Function
AcqFunctionDistIGS <- R6Class("AcqFunctionDistIGS",
  inherit = AcqFunctionDist,

  public = list(

    #' @description
    #' Creates a new distance-aware iGS acquisition function.
    #'
    #' @param surrogate (`NULL` | [mlr3mbo::SurrogateLearner])\cr
    #'   Surrogate used for mean predictions.
    #' @param al_distance ([ALDistance] | `NULL`)\cr
    #'   Distance used for input-space scores.
    initialize = function(surrogate = NULL, al_distance = clx_ald("standardize")) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      super$initialize(
        id = "acq_dist_igs",
        surrogate = surrogate,
        al_distance = al_distance,
        requires_predict_type_se = FALSE,
        surrogate_class = "SurrogateLearner",
        direction = "maximize",
        label = "Distance-Aware iGS Acquisition Function",
        man = "celecx::AcqFunctionDistIGS"
      )
    }
  ),

  private = list(
    .fun_dist = function(xdt, distances) {
      p <- self$surrogate$predict(xdt)
      y_obs <- self$archive$data[[self$surrogate$cols_y]]
      distances_y <- abs(outer(p$mean, y_obs, "-"))

      data.table(acq_dist_igs = apply(distances * distances_y, 1L, min))
    }
  )
)

mlr_acqfunctions$add("dist_igs", AcqFunctionDistIGS)
