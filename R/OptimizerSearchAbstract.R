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
#' For [SearchInstance], `$optimize()` runs a custom batch-style loop that
#' mirrors [bbotk::optimize_batch_default()] but accepts the broader
#' [bbotk::EvalInstance] base class and catches `terminated_error` (the
#' condition class raised by [search_terminated_error()]).
#'
#' @export
OptimizerSearchAbstract <- R6Class("OptimizerSearchAbstract",
  inherit = OptimizerBatch,

  public = list(
    #' @description
    #' Runs the optimizer on either an [bbotk::OptimInstance] or a [SearchInstance].
    #'
    #' @param inst ([bbotk::EvalInstance])\cr
    #'   Either an [bbotk::OptimInstance] (delegated to [bbotk::OptimizerBatch])
    #'   or a [SearchInstance] (handled by a custom loop).
    #' @return [data.table::data.table] for [bbotk::OptimInstance], `NULL` invisibly otherwise.
    optimize = function(inst) {
      if (inherits(inst, "OptimInstance")) {
        return(super$optimize(inst))
      }
      assert_search_instance_properties(self, inst)

      inst$archive$start_time <- Sys.time()
      get_private(inst)$.initialize_context(self)
      call_back("on_optimization_begin", inst$objective$callbacks, inst$objective$context)

      tryCatch({
        get_private(self)$.optimize(inst)
      }, terminated_error = function(cond) {})

      get_private(self)$.assign_result(inst)
      call_back("on_optimization_end", inst$objective$callbacks, inst$objective$context)
      invisible(NULL)
    }
  ),

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


# Mirror of bbotk:::assert_instance_properties but accepting any EvalInstance.
# Required because CRAN bbotk's assert_instance_properties enforces
# inherits(inst, "OptimInstance"), which excludes SearchInstance.
assert_search_instance_properties <- function(optimizer, inst) {
  assert_class(inst, "EvalInstance")
  require_namespaces(optimizer$packages)

  if ("multi-crit" %nin% optimizer$properties && inst$objective$ydim > 1) {
    stopf("'%s' does not support multi-crit objectives", optimizer$format())
  }
  if ("single-crit" %nin% optimizer$properties && inst$objective$ydim == 1) {
    stopf("'%s' does not support single-crit objectives", optimizer$format())
  }

  if ("dependencies" %nin% optimizer$properties && inst$search_space$has_deps) {
    stopf("'%s' does not support param sets with dependencies!", optimizer$format())
  }

  not_supported_pclasses <- setdiff(unique(inst$search_space$class), get_private(optimizer)$.param_classes)
  if (length(not_supported_pclasses)) {
    stopf(
      "'%s' does not support param types: '%s'",
      class(optimizer)[1L],
      paste0(not_supported_pclasses, collapse = ",")
    )
  }
}
