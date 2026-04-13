#' @title Active Learning Proposal Context
#'
#' @include AcqFunctionDist.R
#'
#' @description
#' Ephemeral context passed from [OptimizerAL] to [ALProposer] objects for a
#' single outer proposal round.
#'
#' The context keeps shallow references to the optimizer's canonical surrogate
#' and acquisition-function registries. Access should go through `$get_surrogate()`
#' and `$get_acq()`, because these methods implement lazy model updates, lazy
#' acquisition-function fitting, and proposer-local pending-point filtering.
#'
#' @export
ALContext <- R6Class("ALContext",
  public = list(

    #' @field instance ([bbotk::EvalInstance])\cr
    #' Search or optimization instance.
    instance = NULL,

    #' @description
    #' Creates a new active-learning context.
    #'
    #' @param instance ([bbotk::EvalInstance])\cr
    #'   Search or optimization instance.
    #' @param pool (`NULL` | `data.table`)\cr
    #'   Full finite candidate pool. `NULL` for continuous search spaces.
    #' @param surrogates (named `list()` of [mlr3mbo::Surrogate])\cr
    #'   Canonical run-local surrogate objects.
    #' @param acq_functions (named `list()` of [mlr3mbo::AcqFunction])\cr
    #'   Unwired acquisition-function prototypes.
    #' @param run_state (`list()`)\cr
    #'   Run-local mutable state shared across proposal rounds.
    #' @param allow_repeat_evaluations (`logical(1)`)\cr
    #'   Whether to allow repeat evaluations of the same point.
    initialize = function(instance, pool, surrogates, acq_functions, run_state, allow_repeat_evaluations = FALSE) {
      self$instance <- assert_r6(instance, "EvalInstance")
      private$.allow_repeat_evaluations <- assert_flag(allow_repeat_evaluations)
      feature_ids <- self$instance$search_space$ids()
      assert_data_table(pool, null.ok = TRUE)
      if (!is.null(pool)) {
        assert_names(names(pool), permutation.of = feature_ids)
        private$.pool <- pool[, feature_ids, with = FALSE]
      }

      xdt_prototype <- if (is.null(pool)) {
        param_set_empty_dt(self$instance$search_space)
      } else {
        pool[0L, feature_ids, with = FALSE]
      }
      private$.evaluated_xdt <- if (self$instance$archive$n_evals) {
        unique(self$instance$archive$data[, feature_ids, with = FALSE])
      } else {
        xdt_prototype
      }
      private$.pending_xdt <- xdt_prototype
      if (!is.null(pool)) {
        private$.evaluated_indices <- pool[private$.evaluated_xdt, on = feature_ids, which = TRUE]
        remaining_indices <- setdiff(seq_len(nrow(pool)), private$.evaluated_indices)
        private$.unevaluated_indices <- remaining_indices
        private$.proposable_indices <- if (allow_repeat_evaluations) seq_len(nrow(pool)) else remaining_indices
        private$.pending_indices <- integer(0)
      } else {
        private$.evaluated_indices <- NULL
        private$.unevaluated_indices <- NULL
        private$.proposable_indices <- NULL
        private$.pending_indices <- NULL
      }

      assert_list(surrogates, names = "unique")
      walk(surrogates, assert_r6, "Surrogate")
      private$.surrogates <- surrogates

      assert_list(acq_functions, names = "unique")
      walk(acq_functions, assert_r6, "AcqFunction")
      private$.acq_functions <- acq_functions

      assert_list(run_state, names = "named")
      assert_subset(c("working_acqs", "working_acqs_search_space"), names(run_state))
      private$.run_state <- run_state

      # IDs of all the surrogates that have been updated to the current context's round
      # (i.e., evaluated points)
      private$.updated_surrogates <- character(0)
      # IDs of all the acquisition functions that have been updated similarly
      private$.updated_acqs <- character(0)
    },

    #' @description
    #' Adds points selected for the current proposal batch.
    #'
    #' @param xdt (`data.table`)\cr
    #'   Points selected in the current proposal round.
    add_pending = function(xdt) {
      assert_data_table(xdt)
      feature_ids <- self$instance$search_space$ids()
      assert_names(names(xdt), must.include = feature_ids)
      private$.pending_xdt <- rbind(
        private$.pending_xdt,
        xdt[, feature_ids, with = FALSE],
        use.names = TRUE
      )
      if (!is.null(private$.pool)) {
        new_indices <- private$.pool[xdt, on = feature_ids, which = TRUE]
        private$.pending_indices <- c(private$.pending_indices, new_indices)
        private$.proposable_indices <- setdiff(private$.proposable_indices, new_indices)
      }
      invisible(NULL)
    },

    #' @description
    #' Lazily updates and returns a canonical surrogate.
    #'
    #' @param id (`character(1)`)\cr
    #'   Surrogate registry id.
    #'
    #' @return [mlr3mbo::Surrogate].
    get_surrogate = function(id) {
      assert_string(id, min.chars = 1L)
      if (!id %in% names(private$.surrogates)) {
        stopf("Unknown surrogate id '%s'", id)
      }

      surrogate <- private$.surrogates[[id]]
      surrogate$archive <- self$instance$archive
      if (!id %in% private$.updated_surrogates) {
        surrogate$update()
        private$.updated_surrogates[[length(private$.updated_surrogates) + 1L]] <- id
      }

      surrogate
    },

    #' @description
    #' Lazily wires, fits, updates, and returns an acquisition function.
    #'
    #' @param acq_id (`character(1)`)\cr
    #'   Acquisition-function registry id.
    #' @param surrogate_id (`character(1)`)\cr
    #'   Surrogate registry id.
    #' @param fit_scope (`character(1)`)\cr
    #'   Either `"global"` to fit on the run's finite pool or `"candidate"` to
    #'   fit on `pool`, or `"search_space"` to fit on the search-space bounds.
    #'   `"global"` falls back to `"search_space"` when the run has no finite
    #'   pool.
    #' @param pool (`NULL` | `data.table`)\cr
    #'   Candidate pool for candidate-scope fitting.
    #' @param clone (`logical(1)`)\cr
    #'   Whether to return a fresh working clone.
    #'
    #' @return [mlr3mbo::AcqFunction].
    get_acq = function(acq_id, surrogate_id, fit_scope = "global",
        pool = NULL, clone = FALSE) {
      fit_scope <- private$.normalize_fit_scope(fit_scope)
      clone <- assert_flag(clone)
      surrogate <- self$get_surrogate(surrogate_id)

      if (clone || fit_scope == "candidate") {
        return(self$new_acq(acq_id, surrogate, fit_scope = fit_scope, pool = pool))
      }

      acq_env <- private$.acq_cache_env(fit_scope)
      key <- paste(acq_id, surrogate_id, sep = "::")
      update_key <- paste(fit_scope, key, sep = "::")
      if (exists(key, envir = acq_env, inherits = FALSE)) {
        acq_function <- acq_env[[key]]
      } else {
        acq_function <- private$.clone_acq_prototype(acq_id)
        acq_function$surrogate <- surrogate
        private$.fit_acq_pool(acq_function, fit_scope = fit_scope, pool = NULL)
        acq_env[[key]] <- acq_function
      }

      if (!update_key %in% private$.updated_acqs) {
        acq_function$update()
        private$.updated_acqs[[length(private$.updated_acqs) + 1L]] <- update_key
      }

      acq_function
    },

    #' @description
    #' Creates a fresh acquisition-function working object.
    #'
    #' @param acq_id (`character(1)`)\cr
    #'   Acquisition-function registry id.
    #' @param surrogate ([mlr3mbo::Surrogate])\cr
    #'   Surrogate to wire into the acquisition function.
    #' @param fit_scope (`character(1)`)\cr
    #'   Either `"global"`, `"candidate"`, or `"search_space"`. `"global"`
    #'   falls back to `"search_space"` when the run has no finite pool.
    #' @param pool (`NULL` | `data.table`)\cr
    #'   Candidate pool for candidate-scope fitting.
    #'
    #' @return [mlr3mbo::AcqFunction].
    new_acq = function(acq_id, surrogate, fit_scope = "global", pool = NULL) {
      fit_scope <- private$.normalize_fit_scope(fit_scope)
      surrogate <- assert_r6(surrogate, "Surrogate")

      acq_function <- private$.clone_acq_prototype(acq_id)
      acq_function$surrogate <- surrogate
      private$.fit_acq_pool(acq_function, fit_scope = fit_scope, pool = pool)
      acq_function$update()
      acq_function
    }
  ),

  active = list(
    #' @field pool (`NULL` | `data.table`)
    #' Full finite candidate pool.
    pool = function(rhs) {
      assert_ro_binding(rhs)
      private$.pool
    },

    #' @field unevaluated_xdt (`NULL` | `data.table`)
    #' Remaining finite candidate pool that were not evaluated yet
    #' (but may have been proposed already; use `proposable_xdt` to get
    #' candidates that can still be proposed).
    #' NULL for continuous contexts without `pool`.
    #'
    #' 'unevaluated' candidates are the union of 'proposable' and 'pending' candidates.
    unevaluated_xdt = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(private$.pool)) {
        NULL
      } else {
        private$.pool[private$.unevaluated_indices]
      }
    },
    #' @field unevaluated_indices (`NULL` | `integer`)
    #' Indices within `pool` of the remaining finite candidate pool that were not evaluated yet
    #' (but may have been proposed already; use `proposable_indices` to get
    #' indices of candidates that can still be proposed).
    #' NULL for continuous contexts without `pool`.
    #'
    #' 'unevaluated' indices are the union of 'proposable' and 'pending' indices.
    unevaluated_indices = function(rhs) {
      assert_ro_binding(rhs)
      private$.unevaluated_indices
    },

    #' @field evaluated_xdt (`data.table`)
    #' Points that have been evaluated.
    evaluated_xdt = function(rhs) {
      assert_ro_binding(rhs)
      private$.evaluated_xdt
    },
    #' @field evaluated_indices (`integer` | `NULL`)
    #' Indices within `pool` of points that have been evaluated.
    #' NULL for continuous contexts without `pool`.
    evaluated_indices = function(rhs) {
      assert_ro_binding(rhs)
      private$.evaluated_indices
    },

    #' @field pending_xdt (`data.table`)
    #' Points already selected during the current proposal round.
    #' These are a subset of 'unevaluated' points.
    pending_xdt = function(rhs) {
      assert_ro_binding(rhs)
      private$.pending_xdt
    },
    #' @field pending_indices (`integer` | `NULL`)
    #' Indices within `pool` of points already selected during the current proposal round.
    #' NULL for continuous contexts without `pool`.
    #' These are a subset of 'unevaluated' indices.
    pending_indices = function(rhs) {
      assert_ro_binding(rhs)
      private$.pending_indices
    },

    #' @field proposable_xdt (`NULL` | `data.table`)
    #' Candidates that can still be proposed.
    #' NULL for continuous contexts without `pool`.
    #' These are all 'unevaluated' points that are not 'pending' when `allow_repeat_evaluations` is FALSE;
    #' otherwise, these are all points that are not pending.
    proposable_xdt = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(private$.pool)) {
        NULL
      } else {
        private$.pool[private$.proposable_indices]
      }
    },
    #' @field proposable_indices (`integer` | `NULL`)
    #' Indices within `pool` of candidates that can still be proposed.
    #' NULL for continuous contexts without `pool`.
    #' These are all 'unevaluated' indices that are not 'pending' when `allow_repeat_evaluations` is FALSE;
    #' otherwise, these are all indices that are not pending.
    proposable_indices = function(rhs) {
      assert_ro_binding(rhs)
      private$.proposable_indices
    },

    #' @description
    #' Points that have been evaluated or are pending evaluation.
    evaluated_and_pending_xdt = function(rhs) {
      assert_ro_binding(rhs)
      rbind(private$.evaluated_xdt, private$.pending_xdt, use.names = TRUE)
    },

    #' @field evaluated_and_pending_indices (`integer` | `NULL`)
    #' Indices within `pool` of points that have been evaluated or are pending evaluation.
    #' NULL for continuous contexts without `pool`.
    evaluated_and_pending_indices = function(rhs) {
      assert_ro_binding(rhs)
      c(private$.evaluated_indices, private$.pending_indices)
    },

    #' @field allow_repeat_evaluations (`logical(1)`)
    #' Whether to allow repeat evaluations of the same point.
    allow_repeat_evaluations = function(rhs) {
      assert_ro_binding(rhs)
      private$.allow_repeat_evaluations
    }

  ),

  private = list(
    .allow_repeat_evaluations = FALSE,
    .pool = NULL,
    .surrogates = NULL,
    .acq_functions = NULL,
    .run_state = NULL,
    .updated_surrogates = NULL,
    .updated_acqs = NULL,
    .evaluated_indices = NULL,
    .unevaluated_indices = NULL,
    .proposable_indices = NULL,
    .pending_indices = NULL,
    .evaluated_xdt = NULL,
    .pending_xdt = NULL,

    .clone_acq_prototype = function(acq_id) {
      assert_string(acq_id, min.chars = 1L)
      prototype <- private$.acq_functions[[acq_id]]
      if (is.null(prototype)) {
        stopf("Unknown acquisition function id '%s'", acq_id)
      }

      prototype$clone(deep = TRUE)
    },

    .normalize_fit_scope = function(fit_scope) {
      fit_scope <- assert_choice(fit_scope, al_proposer_acq_fit_scopes)
      if (identical(fit_scope, "global") && is.null(private$.pool)) {
        return("search_space")
      }
      fit_scope
    },

    .acq_cache_env = function(fit_scope) {
      switch(fit_scope,
        global = private$.run_state$working_acqs,
        search_space = private$.run_state$working_acqs_search_space,
        stopf("No acquisition cache exists for fit_scope = '%s'", fit_scope)
      )
    },

    .fit_acq_pool = function(acq_function, fit_scope, pool) {
      if (!("fit_pool" %in% names(acq_function))) {
        return(invisible(NULL))
      }

      fit_pool <- switch(fit_scope,
        global = private$.pool,
        candidate = pool,
        search_space = NULL,
        stopf("Unknown acquisition fit scope '%s'", fit_scope)
      )
      if (identical(fit_scope, "candidate") && is.null(fit_pool)) {
        stopf(
          "Acquisition function '%s' needs a finite pool for fit_scope = '%s'",
          acq_function$id, fit_scope
        )
      }

      acq_function$fit_pool(fit_pool, self$instance$search_space)
      invisible(NULL)
    }
  )
)

al_proposer_acq_fit_scopes <- c("global", "candidate", "search_space")
