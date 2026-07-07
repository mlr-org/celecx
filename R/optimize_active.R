#' @title Run Active Learning
#'
#' @description
#' Convenience function that constructs an active learning [OptimizerAL]
#' via [optimizer_al()] and runs it on a [SearchInstance].
#'
#' @param objective ([bbotk::Objective])\cr
#'   Objective to evaluate. Typically has a single codomain target tagged `"learn"`.
#' @param search_space (`NULL` | [paradox::ParamSet])\cr
#'   Optional restricted search space. If `NULL`, the search space is derived from
#'   `objective$domain` (same logic as bbotk's `OptimInstanceBatch`).
#' @param n_evals (`NULL` | `integer(1)`)\cr
#'   Convenience evaluation budget used only if `terminator` is `NULL`.
#' @param terminator (`NULL` | [bbotk::Terminator])\cr
#'   Terminator for the outer active learning loop. If `NULL`, a
#'   `trm("evals", n_evals = n_evals)` is constructed.
#' @param callbacks (`NULL` | `list()` of [bbotk::CallbackBatch])\cr
#'   Callbacks attached to the instance, e.g. a
#'   [CallbackSurrogatePerformance] for per-batch surrogate tracking.
#' @param optimizer (`NULL` | [bbotk::OptimizerBatch])\cr
#'   Explicit optimizer to use. If `NULL`, constructs one via
#'   [optimizer_al()]. Supply an optimizer from
#'   [optimizer_pool_al()] to use paper-style active learning methods.
#' @param ...
#'   Passed to [optimizer_al()] when `optimizer` is `NULL`.
#'
#' @return `list()` with:
#' - `instance`: [SearchInstance]
#' - `optimizer`: configured optimizer
#'
#' @export
optimize_active <- function(objective,
    search_space = NULL,
    n_evals = NULL,
    terminator = NULL,
    callbacks = NULL,
    optimizer = NULL,
    ...) {

  assert_r6(objective, "Objective")
  if (!is.null(search_space)) {
    assert_param_set(search_space)
  }

  if (is.null(terminator)) {
    assert_int(n_evals, lower = 1L)
    terminator <- trm("evals", n_evals = n_evals)
  } else {
    assert_r6(terminator, "Terminator")
  }

  callbacks <- assert_callbacks(as_callbacks(callbacks))

  if (is.null(optimizer)) {
    optimizer <- optimizer_al(...)
  }

  search_instance <- SearchInstance$new(
    objective = objective,
    search_space = search_space,
    terminator = terminator,
    callbacks = callbacks
  )

  optimizer$optimize(search_instance)

  list(
    instance = search_instance,
    optimizer = optimizer
  )
}
