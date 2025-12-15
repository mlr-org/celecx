#' @title Context for Search Instance
#'
#' @description
#' Context object that allows callbacks to access and modify data during
#' search operations on a [SearchInstance].
#'
#' @details
#' Similar to bbotk's `ContextBatch`, but for [SearchInstance] instead of
#' [OptimInstanceBatch].
#'
#' @export
ContextSearch <- R6Class("ContextSearch",
  inherit = Context,
  public = list(

    #' @field instance ([SearchInstance]).
    instance = NULL,

    #' @field optimizer ([Optimizer] | `NULL`).
    optimizer = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param inst ([SearchInstance]).
    #' @param optimizer ([Optimizer] | `NULL`).
    initialize = function(inst, optimizer) {
      super$initialize(id = "search")
      self$instance <- assert_r6(inst, "SearchInstance")
      self$optimizer <- optimizer
    }
  ),

  active = list(

    #' @field xdt ([data.table::data.table])\cr
    #' The points of the latest batch in `instance$eval_batch()`.
    xdt = function(rhs) {
      if (missing(rhs)) {
        get_private(self$instance)$.xdt
      } else {
        get_private(self$instance)$.xdt <- rhs
      }
    }
  )
)


#' @title Search Instance
#'
#' @description
#' Container for a search problem that extends bbotk's [EvalInstance] to support
#' both optimization and active learning. Holds the objective, search space,
#' archive, and terminator, and provides the evaluation loop mechanics.
#'
#' Unlike [OptimInstance], this class:
#' - Supports codomains with "learn" tags (in addition to minimize/maximize)
#' - Does not compute `objective_multiplicator`
#' - Does not assign a "result" (best point)
#'
#' @details
#' The search instance serves as the "problem specification" for custom search
#' loops. It owns the archive and handles evaluation mechanics.
#'
#' If you want to run a full MBO-style loop, prefer using bbotk's
#' [bbotk::OptimInstanceBatchSingleCrit] / [bbotk::OptimInstanceBatchMultiCrit]
#' together with [mlr3mbo::OptimizerMbo]. For codomains containing `"learn"`
#' targets, use [ResultAssignerNull] to disable assigning a "best" result.
#'
#' Since bbotk's Codomain now natively supports "learn" tags, codomains are
#' passed directly to ArchiveBatch without conversion. Note that calling
#' `archive$best()` or `archive$nds_selection()` will error if the codomain
#' contains only "learn" targets, which is the correct behavior.
#'
#' @section Callbacks:
#' Callbacks can be registered to hook into the evaluation loop. The following
#' stages are supported:
#' - `on_optimizer_before_eval`: Called before evaluating a batch
#' - `on_optimizer_after_eval`: Called after evaluating a batch
#'
#' @section Termination:
#' Before each batch evaluation, the terminator is checked. If terminated, a
#' `search_terminated_error` condition is raised. This can be caught with
#' `tryCatch(..., search_terminated_error = function(e) ...)`.
#'
#' @export
SearchInstance <- R6Class("SearchInstance",
  inherit = EvalInstance,
  public = list(

    #' @description
    #' Creates a new SearchInstance.
    #'
    #' @param objective ([Objective])\cr
    #'   The objective to evaluate. Can be any bbotk Objective subclass including
    #'   our ObjectiveDataset and ObjectiveLearner.
    #' @param search_space ([paradox::ParamSet])\cr
    #'   Optional restricted search space. If NULL:
    #'   - If the domain contains no TuneTokens, uses the whole domain
    #'   - If the domain contains TuneTokens, derives search space from them
    #'   Cannot be supplied if the domain already contains TuneTokens.
    #' @param terminator ([Terminator])\cr
    #'   When to stop the search. Uses bbotk terminators.
    #' @param archive ([ArchiveBatch])\cr
    #'   Optional pre-existing archive. If NULL, creates a new one.
    #' @param check_values (`logical(1)`)\cr
    #'   Whether to validate points against search_space before evaluation.
    #' @param callbacks (`list()` of [Callback])\cr
    #'   Optional callbacks to hook into the evaluation loop.
    initialize = function(objective, search_space = NULL, terminator,
        archive = NULL, check_values = TRUE, callbacks = NULL) {
      # Validate objective
      assert_r6(objective, "Objective")

      # Store callbacks on the objective (same pattern as OptimInstance)
      objective$callbacks <- assert_callbacks(as_callbacks(callbacks))

      # Derive search_space using same logic as OptimInstanceBatch
      search_space <- choose_search_space(objective, search_space)

      # Validate terminator
      assert_r6(terminator, "Terminator")

      # Store check_values flag
      private$.check_values <- assert_flag(check_values)

      # Create archive - Codomain now natively supports "learn" tags
      if (is.null(archive)) {
        archive <- ArchiveBatch$new(
          search_space = search_space,
          codomain = objective$codomain,
          check_values = check_values
        )
      } else {
        assert_r6(archive, "ArchiveBatch")
      }

      # Call parent constructor
      super$initialize(
        objective = objective,
        search_space = search_space,
        terminator = terminator,
        archive = archive
      )
    },

    #' @description
    #' Evaluates a batch of points and adds results to the archive.
    #'
    #' @param xdt (`data.table`)\cr
    #'   Points to evaluate, one row per configuration.
    #'
    #' @return Invisibly returns the ydt (codomain values).
    eval_batch = function(xdt) {
      # Store xdt for callbacks to access
      private$.xdt <- xdt

      # Initialize context if needed (for callbacks)
      if (is.null(self$objective$context)) {
        private$.initialize_context(NULL)
      }

      # Call pre-evaluation callbacks
      call_back("on_optimizer_before_eval", self$objective$callbacks,
        self$objective$context)

      # 1. Check if terminated, throw terminated_error if so
      if (self$is_terminated) {
        stop(search_terminated_error(self))
      }

      # 2. Validate xdt
      assert_data_table(xdt)
      assert_names(colnames(xdt), must.include = self$search_space$ids())

      # 3. Validate against search_space if check_values
      if (private$.check_values) {
        self$search_space$assert_dt(xdt[, self$search_space$ids(), with = FALSE])
      }

      # 4. Call objective$eval_dt(xdt) to get ydt
      ydt <- self$objective$eval_dt(xdt[, self$search_space$ids(), with = FALSE])

      # 5. Add to archive via archive$add_evals()
      # xss_trafoed is NULL since we don't transform
      self$archive$add_evals(xdt, xss_trafoed = NULL, ydt)

      # Call post-evaluation callbacks
      call_back("on_optimizer_after_eval", self$objective$callbacks,
        self$objective$context)

      # 6. Return ydt invisibly
      invisible(ydt[, self$archive$cols_y, with = FALSE])
    },

    #' @description
    #' Resets the instance for a fresh search.
    clear = function() {
      super$clear()
      invisible(self)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat(sprintf("<%s>\n", class(self)[[1L]]))
      cat(sprintf("* Objective: %s\n", self$objective$id))
      cat(sprintf("* Search space: %d parameters\n", self$search_space$length))
      cat(sprintf("* Terminator: %s\n", class(self$terminator)[[1L]]))
      cat(sprintf("* Evaluations: %d\n", self$n_evals))
      cat(sprintf("* Batches: %d\n", self$archive$n_batch))
      cat(sprintf("* Terminated: %s\n", self$is_terminated))

      # Show codomain goal
      goal <- codomain_goal(self$objective$codomain)
      cat(sprintf("* Goal: %s\n", goal))

      invisible(self)
    }
  ),

  private = list(
    .check_values = TRUE,
    .xdt = NULL,

    # Initialize context for callbacks (called by OptimizerBatch or eval_batch)
    .initialize_context = function(optimizer) {
      context <- ContextSearch$new(inst = self, optimizer = optimizer)
      self$objective$context <- context
    }

    # Note: deep_clone for objective, search_space, terminator, archive
    # is handled by parent EvalInstance class
  )
)


#' @title Search Terminated Error
#'
#' @description
#' Creates a condition indicating that a SearchInstance has terminated.
#' This is similar to bbotk's `terminated_error` but for SearchInstance.
#'
#' @param search_instance ([SearchInstance])\cr
#'   The SearchInstance that terminated.
#'
#' @return A condition object with class `search_terminated_error`.
#'
#' @export
search_terminated_error <- function(search_instance) {
  msg <- sprintf(
    "Search (obj:%s, term:%s) terminated",
    search_instance$objective$id,
    class(search_instance$terminator)[[1L]]
  )

  set_class(
    list(message = msg, call = NULL),
    c("search_terminated_error", "error", "condition")
  )
}


#' @title Create Search Instance
#'
#' @description
#' Convenience constructor for [SearchInstance].
#'
#' @param objective ([Objective])\cr
#'   The objective to evaluate.
#' @param terminator ([Terminator])\cr
#'   Termination criterion.
#' @param search_space ([paradox::ParamSet])\cr
#'   Optional restricted search space.
#' @param ... Additional arguments passed to SearchInstance$new().
#'
#' @return A [SearchInstance] object.
#'
#' @examples
#' \dontrun{
#' # Create instance with evaluation budget
#' terminator <- bbotk::TerminatorEvals$new()
#' terminator$param_set$set_values(n_evals = 100L, k = 0L)
#' instance <- search_instance(objective, terminator = terminator)
#' }
#'
#' @export
search_instance <- function(objective, terminator, search_space = NULL, ...) {
  assert_r6(terminator, "Terminator")

  SearchInstance$new(
    objective = objective,
    terminator = terminator,
    search_space = search_space,
    ...
  )
}


#' @title Choose Search Space
#'
#' @description
#' Derives the search space from the objective domain and optional user-supplied
#' search space. Follows the same logic as bbotk's OptimInstanceBatch.
#'
#' @param objective ([Objective])\cr
#'   The objective with a domain ParamSet.
#' @param search_space ([paradox::ParamSet] | `NULL`)\cr
#'   Optional user-supplied search space.
#'
#' @return A [paradox::ParamSet] to use as the search space.
#'
#' @keywords internal
choose_search_space <- function(objective, search_space) {
  domain_search_space <- objective$domain$search_space()
  if (is.null(search_space) && domain_search_space$length == 0) {
    # Use whole domain as search space
    objective$domain
  } else if (is.null(search_space) && domain_search_space$length > 0) {
    # Create search space from tune tokens in domain
    domain_search_space
  } else if (!is.null(search_space) && domain_search_space$length == 0) {
    # Use supplied search space
    assert_param_set(search_space)
  } else {
    stop("If the domain contains TuneTokens, you cannot supply a search_space.")
  }
}
