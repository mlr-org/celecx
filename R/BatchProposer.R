#' @title Batch Proposer
#'
#' @description
#' Extension of [mlr3mbo::AcqOptimizer] that applies a batch selection strategy
#' to diversify the returned candidate set.
#'
#' @details
#' This class implements a wrapper around the parent [mlr3mbo::AcqOptimizer]:
#' it temporarily requests a larger number of candidates, then down-selects to
#' the desired batch size using `batch_strategy`.
#'
#' Batch strategies operate on a pool that is already ranked by the acquisition
#' optimizer and follow the convention: **lower is better** (the sign is
#' adjusted automatically based on the acquisition codomain).
#'
#' @section Parameters:
#' Inherits all parameters from [mlr3mbo::AcqOptimizer], in particular:
#' - `n_candidates`: number of points to propose (batch size)
#' - `warmstart`, `warmstart_size`, `skip_already_evaluated`, `catch_errors`,
#'   `logging_level`
#'
#' @export
BatchProposer <- R6Class("BatchProposer",
  inherit = mlr3mbo::AcqOptimizer,

  public = list(

    #' @field pool_factor (`integer(1)`)\cr
    #' Multiplier for pool size. The internal pool size is
    #' `n_candidates * pool_factor`.
    pool_factor = NULL,

    #' @description
    #' Creates a new BatchProposer.
    #'
    #' @param optimizer ([bbotk::OptimizerBatch])\cr
    #'   Optimizer used for acquisition optimization.
    #' @param terminator ([bbotk::Terminator])\cr
    #'   Terminator used for acquisition optimization.
    #' @param acq_function (`NULL` | [mlr3mbo::AcqFunction])\cr
    #'   Acquisition function. Can be set later via `$acq_function`.
    #' @param batch_strategy (`function`)\cr
    #'   Batch selection strategy. Must have signature
    #'   `function(candidates, scores, batch_size, surrogate, archive, search_space)`
    #'   and return integer indices into `candidates`.
    #' @param pool_factor (`integer(1)`)\cr
    #'   Pool size multiplier. Pool size is `n_candidates * pool_factor`.
    #'   Default is 10.
    #' @param callbacks (`NULL` | `list()`)\cr
    #'   Callbacks. Passed to parent [mlr3mbo::AcqOptimizer].
    initialize = function(optimizer, terminator, acq_function = NULL,
        batch_strategy = batch_strategy_greedy(), pool_factor = 10L,
        callbacks = NULL) {

      assert_r6(optimizer, "OptimizerBatch")
      assert_r6(terminator, "Terminator")
      assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      assert_function(batch_strategy)
      assert_int(pool_factor, lower = 1L)

      self$pool_factor <- pool_factor
      private$.batch_strategy <- batch_strategy

      super$initialize(
        optimizer = optimizer,
        terminator = terminator,
        acq_function = acq_function,
        callbacks = callbacks
      )
    },

    #' @description
    #' Optimize the acquisition function and apply a batch selection strategy.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      if (is.null(self$acq_function)) {
        stopf("acq_function must be set before calling optimize()")
      }

      if (self$acq_function$codomain$target_length != 1L) {
        stopf("BatchProposer currently supports only single-objective acquisition functions")
      }

      batch_size <- self$param_set$values$n_candidates %??% 1L
      pool_factor <- self$pool_factor %??% 1L

      if (batch_size == 1L || pool_factor == 1L) {
        return(super$optimize())
      }

      pool_size <- batch_size * pool_factor

      # If possible, avoid requesting more candidates than the evaluation budget.
      if (inherits(self$terminator, "TerminatorEvals")) {
        pv <- self$terminator$param_set$values
        max_steps <- pv$n_evals + pv$k * self$acq_function$domain$length
        if (max_steps < batch_size) {
          stopf(
            "Acquisition terminator budget (%i) is smaller than batch_size (%i)",
            max_steps, batch_size
          )
        }
        pool_size <- min(pool_size, max_steps)
      }

      old_n_candidates <- self$param_set$values$n_candidates
      on.exit(self$param_set$set_values(n_candidates = old_n_candidates), add = TRUE)

      self$param_set$set_values(n_candidates = pool_size)
      pool_xdt <- super$optimize()

      if (nrow(pool_xdt) == batch_size) {
        return(pool_xdt)
      }

      x_ids <- self$acq_function$domain$ids()
      y_id <- self$acq_function$codomain$target_ids[[1L]]
      direction <- self$acq_function$codomain$direction[[y_id]]
      scores <- pool_xdt[[y_id]] * direction  # lower is better for strategy

      selected_indices <- private$.batch_strategy(
        candidates = pool_xdt[, x_ids, with = FALSE],
        scores = scores,
        batch_size = batch_size,
        surrogate = self$acq_function$surrogate,
        archive = self$acq_function$archive,
        search_space = self$acq_function$domain
      )

      assert_integerish(selected_indices, tol = 0, lower = 1L)
      if (length(selected_indices) != batch_size) {
        stopf("batch_strategy returned %i indices but batch_size is %i", length(selected_indices), batch_size)
      }
      if (any(selected_indices > nrow(pool_xdt))) {
        stopf("batch_strategy returned index > nrow(pool_xdt)")
      }

      pool_xdt[selected_indices]
    },

    #' @description
    #' Print method.
    #'
    #' @param ... (ignored).
    print = function(...) {
      catn(format(self), paste0(": ", self$print_id))
      catn(str_indent("* Pool factor:", self$pool_factor))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      if (!is.null(self$acq_function)) {
        catn(str_indent("* Acquisition function:", self$acq_function$id))
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field batch_strategy (`function`)\cr
    #' Batch selection strategy.
    batch_strategy = function(rhs) {
      if (missing(rhs)) {
        private$.batch_strategy
      } else {
        assert_function(rhs)
        private$.batch_strategy <- rhs
      }
    }
  ),

  private = list(
    .batch_strategy = NULL
  )
)
