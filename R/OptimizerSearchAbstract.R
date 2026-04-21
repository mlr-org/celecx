#' @title Abstract Base Class for Search-Compatible Optimizers
#'
#' @description
#' Extends [bbotk::OptimizerBatch] with support for [SearchInstance] in
#' addition to [bbotk::OptimInstance].
#' Optimizers that should work with both [bbotk::OptimInstance] and
#' [SearchInstance] should inherit from this class instead of
#' [bbotk::OptimizerBatch].
#'
#' @details
#' The default `.assign_result()` in [bbotk::OptimizerBatch]
#' assumes an [bbotk::OptimInstance] and errors on a [SearchInstance].
#' This class overrides `.assign_result()` to handle both: it delegates to
#' [bbotk::assign_result_default()] for [bbotk::OptimInstance] and is a no-op
#' for [SearchInstance] (which has no result slot).
#'
#' @export
OptimizerSearchAbstract <- R6Class("OptimizerSearchAbstract",
  inherit = OptimizerBatch,

  private = list(
    .assign_result = function(inst) {
      if (inherits(inst, "OptimInstance")) {
        assign_result_default(inst)
      } else {
        invisible(NULL)
      }
    }
  )
)
