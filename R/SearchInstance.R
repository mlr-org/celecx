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
#' @include utils_objective.R utils_codomain.R utils_param_set.R
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
#' @section Callbacks:
#' Callbacks can be registered to hook into the evaluation loop. The following
#' stages are supported:
#' - `on_optimizer_before_eval`: Called before evaluating a batch
#' - `on_optimizer_after_eval`: Called after evaluating a batch
#'
#' @section Termination:
#' Before each batch evaluation, the terminator is checked. If terminated, a
#' `search_terminated_error` (inheriting from `terminated_error`) condition is raised.
#'
#' @export
SearchInstance <- R6Class("SearchInstance",
  inherit = EvalInstance,
  public = list(

    #' @field progressor ([Progressor] | `NULL`).
    #' The progressor for the search instance.
    #' Used to display a progress bar during the search.
    progressor = NULL,

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
      assert_pool_objective_search_space(objective, search_space)

      # Validate terminator
      assert_r6(terminator, "Terminator")

      # Create archive - Codomain now natively supports "learn" tags
      private$.check_values <- assert_flag(check_values)
      if (is.null(archive)) {
        archive <- ArchiveBatch$new(
          search_space = search_space,
          codomain = objective$codomain,
          check_values = FALSE
        )
      } else {
        assert_r6(archive, "ArchiveBatch")
        archive$check_values <- FALSE
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

      # we run this callback before termination check, so we are in agreement with OptimInstanceBatch
      call_back("on_optimizer_before_eval", self$objective$callbacks, self$objective$context)
      if (!is.null(self$progressor)) self$progressor$update(self$terminator, self$archive)

      if (self$is_terminated) {
        stop(search_terminated_error(self))
      }

      assert_data_table(xdt)
      assert_names(colnames(xdt), must.include = self$search_space$ids())
      if (private$.check_values) {
        assert_data_table_param_set(
          xdt,
          self$search_space,
          require_uniqueness = FALSE,
          min_rows = 0L,
          allow_untyped = TRUE,
          .param_set_name = "search_space",
          .dt_name = "xdt"
        )
      }

      if (!nrow(xdt)) {
        xss_trafoed <- NULL
        ydt <- param_set_empty_dt(self$objective$codomain)
      } else {
        search_space_xdt <- xdt[, self$search_space$ids(), with = FALSE]
        if (self$search_space$has_deps || length(self$search_space$values)) {
          # in-place set search space $values (if present) and NA-fill cols where dependencies are not satisfied
          apply_param_set_design_dt(search_space_xdt, self$search_space)
        }

        if (self$search_space$has_trafo) {
          xss_trafoed <- map(transpose_list(search_space_xdt), trafo_xs,
            search_space = self$search_space)
          ydt <- self$objective$eval_many(xss_trafoed)
        } else if (objective_uses_dt_eval(self$objective)) {
          # Only objectives that explicitly advertise native data.table
          # semantics stay on the dt path; wrappers around Objective$eval_dt()
          # must go through eval_many() to preserve inactive-parameter handling.
          objective_xdt <- search_space_xdt
          fill_param_set_missing_dt(objective_xdt, self$objective$domain)
          xss_trafoed <- NULL
          ydt <- self$objective$eval_dt(objective_xdt)
        } else {
          xss_trafoed <- map(transpose_list(search_space_xdt), discard, is_scalar_na)
          ydt <- self$objective$eval_many(xss_trafoed)
        }
      }

      self$archive$add_evals(xdt, xss_trafoed = xss_trafoed, ydt)

      call_back("on_optimizer_after_eval", self$objective$callbacks,
        self$objective$context)

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
      goal <- codomain_goals(self$objective$codomain)
      cat(sprintf("* Goal: %s\n", paste(goal, collapse = ", ")))

      invisible(self)
    }
  ),

  private = list(
    .xdt = NULL,
    .check_values = TRUE,

    # Initialize context for callbacks (called by OptimizerBatch or eval_batch)
    .initialize_context = function(optimizer) {
      context <- ContextSearch$new(inst = self, optimizer = optimizer)
      self$objective$context <- context
    }
  )
)


#' @title Search Terminated Error
#'
#' @description
#' Creates a condition indicating that a SearchInstance has terminated.
#' Inherits from bbotk's `terminated_error`.
#'
#' @param search_instance ([SearchInstance])\cr
#'   The SearchInstance that terminated.
#'
#' @return A condition object with class `search_terminated_error`
#' (inherits from `terminated_error`, `error`, `condition`).
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
    c("search_terminated_error", "terminated_error", "error", "condition")
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
