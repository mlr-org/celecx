#' @title Distance-Aware GSx Acquisition Function
#'
#' @include AcqFunctionDist.R
#'
#' @description
#' Greedy-sampling-in-input-space acquisition function using an [ALDistance].
#'
#' The score is the distance to the nearest already evaluated reference point.
#'
#' @export
#' @family Acquisition Function
AcqFunctionDistGSx <- R6Class("AcqFunctionDistGSx",
  inherit = AcqFunctionDist,

  public = list(

    #' @description
    #' Creates a new distance-aware GSx acquisition function.
    #'
    #' @param surrogate (`NULL` | [mlr3mbo::Surrogate])\cr
    #'   Optional surrogate. Only used to derive the acquisition domain and
    #'   archive reference points.
    #' @param al_distance ([ALDistance] | `NULL`)\cr
    #'   Distance used for input-space scores.
    initialize = function(surrogate = NULL, al_distance = clx_ald("standardize")) {
      assert_r6(surrogate, "Surrogate", null.ok = TRUE)
      super$initialize(
        id = "acq_dist_gsx",
        surrogate = surrogate,
        al_distance = al_distance,
        requires_predict_type_se = FALSE,
        surrogate_class = "Surrogate",
        direction = "maximize",
        label = "Distance-Aware GSx Acquisition Function",
        man = "celecx::AcqFunctionDistGSx"
      )
    }
  ),

  private = list(
    .fun_dist = function(xdt, distances) {
      data.table(acq_dist_gsx = apply(distances, 1L, min))
    }
  )
)

acq_functions[["dist_gsx"]] <- AcqFunctionDistGSx
