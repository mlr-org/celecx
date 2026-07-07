#' @title Learning Curve Extrapolation Task
#'
#' @name TaskLCE
#'
#' @include aaa.R
#' @include lce_link.R
#'
#' @description
#' Task class for predicting the surrogate-model quality trajectory of an active
#' learning run. Each row corresponds to one archive evaluation and carries the
#' batch in which it was evaluated, the untransformed archive feature values,
#' the archive target values, and the surrogate's performance for that batch.
#'
#' The task has a single feature, `batch_nr`, which is the only column passed to
#' a [LearnerLCE] at predict time. The archive feature and target columns are
#' carried in the dedicated column roles `archive_x` and `archive_y` so that
#' more sophisticated extrapolators may inspect them during training without
#' the columns being required for prediction on new data.
#'
#' Optionally, the task also carries the [paradox::ParamSet] search space and the
#' [bbotk::Codomain] of the originating optimization run. These cannot be
#' recovered from the archive columns alone (which only retain storage types),
#' but they are required by extrapolators that replay the active-learning loop,
#' since they define parameter bounds, transformations, dependencies, and the
#' codomain optimization directions.
#'
#' Two further pieces of run information can be carried for replay extrapolators:
#' the regression [mlr3::Measure] that produced the performance `target` column
#' (so a simulation can score on the same scale) and, for finite-pool active
#' learning, the candidate `pool` (so a simulation can propose from the same
#' candidate set). The originating surrogate is *not* stored here, as it is a
#' property of the optimizer rather than of the data.
#'
#' `TaskLCE` objects are typically constructed by tracking an [`OptimizerAL`]
#' optimization run with a [`CallbackSurrogatePerformance`] callback.
#'
#' @export
TaskLCE <- R6Class("TaskLCE",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new LCE task.
    #'
    #' @param id (`character(1)`)\cr
    #'   Task id.
    #' @param backend ([mlr3::DataBackend] | `data.frame()`)\cr
    #'   Data backend. Coerced via [mlr3::as_data_backend()].
    #' @param target (`character(1)`)\cr
    #'   Name of the numeric column holding the surrogate performance.
    #' @param batch_nr (`character(1)`)\cr
    #'   Name of the integer column holding the batch number. This column becomes
    #'   the task's single feature.
    #' @param archive_x (`character()`)\cr
    #'   Names of the archive feature columns (untransformed `x` values). At
    #'   least one column is required at construction.
    #' @param archive_y (`character()`)\cr
    #'   Names of the archive target columns (objective `y` values). At least
    #'   one column is required at construction.
    #' @param search_space ([paradox::ParamSet] | `NULL`)\cr
    #'   Optional search space of the originating optimization run. Its
    #'   parameter ids must match `archive_x`. Cloned and stored for replay
    #'   extrapolators.
    #' @param codomain ([bbotk::Codomain] | `NULL`)\cr
    #'   Optional codomain of the originating optimization run. Its target ids
    #'   must match `archive_y`. Cloned and stored for replay extrapolators.
    #' @param measure ([mlr3::Measure] | `NULL`)\cr
    #'   Optional regression measure that produced the performance `target`
    #'   column. Cloned and stored so replay extrapolators can score on the same
    #'   scale.
    #' @param pool ([data.table::data.table] | `NULL`)\cr
    #'   Finite candidate pool of the originating run, for pool-based active
    #'   learning. Its columns must be exactly `archive_x` (the search-space
    #'   parameter ids). `NULL` for continuous search spaces.
    #' @param link (`character(1)`)\cr
    #'   Name of the predictive [lce_link] on whose scale the learners model the
    #'   performance curve and the distributional measures interpret `se`. A
    #'   property of the target metric; `"identity"` by default. See [lce_link]
    #'   (and [lce_link_from_range] to derive one from a measure's range).
    #' @param label (`character(1)`)\cr
    #'   Optional label.
    #' @param extra_args (`list()`)\cr
    #'   Extra constructor arguments preserved for cloning.
    initialize = function(id, backend, target, batch_nr, archive_x, archive_y,
        search_space = NULL, codomain = NULL, measure = NULL, pool = NULL, link = "identity",
        label = NA_character_, extra_args = list()) {
      assert_string(target, min.chars = 1L)
      assert_string(batch_nr, min.chars = 1L)
      assert_character(archive_x, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assert_character(archive_y, any.missing = FALSE, min.len = 1L, unique = TRUE)
      assert_r6(search_space, "ParamSet", null.ok = TRUE)
      assert_r6(codomain, "Codomain", null.ok = TRUE)
      if (!is.null(search_space)) {
        assert_set_equal(search_space$ids(), archive_x,
          .var.name = "search_space parameter ids")
        search_space <- search_space$clone(deep = TRUE)
      }
      if (!is.null(codomain)) {
        assert_set_equal(codomain$target_ids, archive_y,
          .var.name = "codomain target ids")
        codomain <- codomain$clone(deep = TRUE)
      }
      assert_choice(link, names(lce_links))

      role_cols <- c(target, batch_nr, archive_x, archive_y)
      if (anyDuplicated(role_cols)) {
        stopf("target, batch_nr, archive_x, and archive_y columns must be disjoint")
      }

      if (!is.null(measure)) {
        assert_r6(measure, "Measure")
        if (!identical(measure$task_type, "regr")) {
          stopf("measure must be a regression measure, got task_type '%s'",
            measure$task_type)
        }
        measure <- measure$clone(deep = TRUE)
      }
      if (!is.null(pool)) {
        pool <- as.data.table(pool)
        assert_names(names(pool), permutation.of = archive_x,
          .var.name = "pool columns")
      }

      super$initialize(id = id, task_type = "lce", backend = backend,
        target = target, label = label, extra_args = extra_args)

      col_roles <- self$col_roles
      col_roles$feature <- batch_nr
      col_roles$archive_x <- archive_x
      col_roles$archive_y <- archive_y
      self$col_roles <- col_roles

      batch_nr_type <- self$col_info[list(batch_nr), on = "id", "type", with = FALSE][[1L]]
      if (batch_nr_type != "integer") {
        stopf("batch_nr column '%s' must be integer (got '%s')",
          batch_nr, batch_nr_type)
      }

      self$extra_args <- insert_named(self$extra_args, list(
        batch_nr = batch_nr,
        archive_x = archive_x,
        archive_y = archive_y,
        search_space = search_space,
        codomain = codomain,
        measure = measure,
        pool = pool,
        link = link
      ))

      self$man <- "celecx::TaskLCE"
    },

    #' @description
    #' True surrogate performance for the given rows (defaults to all active rows).
    #'
    #' @param rows (`integer()`)
    #' @return `numeric()`.
    truth = function(rows = NULL) {
      super$truth(rows)[[1L]]
    },

    #' @description
    #' Returns the archive feature data (columns with role `archive_x`).
    #'
    #' @param rows (`integer()`)
    #' @return [data.table::data.table()].
    archive_x_data = function(rows = NULL) {
      self$data(rows = rows, cols = self$col_roles$archive_x)
    },

    #' @description
    #' Returns the archive target data (columns with role `archive_y`).
    #'
    #' @param rows (`integer()`)
    #' @return [data.table::data.table()].
    archive_y_data = function(rows = NULL) {
      self$data(rows = rows, cols = self$col_roles$archive_y)
    }
  ),

  active = list(
    #' @field batch_nr (`character(1)`)\cr
    #' Name of the batch_nr column.
    batch_nr = function(rhs) {
      assert_ro_binding(rhs)
      self$col_roles$feature
    },

    #' @field archive_x (`character()`)\cr
    #' Names of the archive feature columns.
    archive_x = function(rhs) {
      assert_ro_binding(rhs)
      self$col_roles$archive_x
    },

    #' @field archive_y (`character()`)\cr
    #' Names of the archive target columns.
    archive_y = function(rhs) {
      assert_ro_binding(rhs)
      self$col_roles$archive_y
    },

    #' @field batch_nrs (`integer()`)\cr
    #' Batch number for every active row, in the order of `task$row_ids`.
    batch_nrs = function(rhs) {
      assert_ro_binding(rhs)
      self$data(cols = self$col_roles$feature)[[1L]]
    },

    #' @field search_space ([paradox::ParamSet] | `NULL`)\cr
    #' Search space of the originating optimization run.
    search_space = function(rhs) {
      assert_ro_binding(rhs)
      self$extra_args$search_space
    },

    #' @field codomain ([bbotk::Codomain] | `NULL`)\cr
    #' Codomain of the originating optimization run.
    codomain = function(rhs) {
      assert_ro_binding(rhs)
      self$extra_args$codomain
    },

    #' @field measure ([mlr3::Measure] | `NULL`)\cr
    #' Regression measure that produced the performance `target` column.
    measure = function(rhs) {
      assert_ro_binding(rhs)
      self$extra_args$measure
    },

    #' @field pool ([data.table::data.table] | `NULL`)\cr
    #' Finite candidate pool of the originating run.
    pool = function(rhs) {
      assert_ro_binding(rhs)
      self$extra_args$pool
    },

    #' @field link (`character(1)`)\cr
    #' Name of the predictive link scale (see [lce_link]).
    link = function(rhs) {
      assert_ro_binding(rhs)
      self$extra_args$link
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      # search_space / codomain / measure are mutable R6 objects stored in
      # extra_args, and pool is a mutable data.table; clone/copy them so a
      # deep-cloned task does not alias the originals. Everything else is handled
      # by the base Task deep_clone.
      if (name == "extra_args") {
        if (!is.null(value$search_space)) value$search_space <- value$search_space$clone(deep = TRUE)
        if (!is.null(value$codomain)) value$codomain <- value$codomain$clone(deep = TRUE)
        if (!is.null(value$measure)) value$measure <- value$measure$clone(deep = TRUE)
        if (!is.null(value$pool)) value$pool <- copy(value$pool)
        value
      } else {
        super$deep_clone(name, value)
      }
    }
  )
)

#' @export
task_check_col_roles.TaskLCE <- function(task, new_roles, ...) {
  # The setter runs at intermediate states during TaskSupervised$initialize()
  # (e.g. when feature still contains every backend column) and during
  # predict_newdata (e.g. when archive_x / archive_y are dropped). Only check
  # what we can validate in isolation; the constructor enforces the strict
  # final shape (exactly one feature, non-empty archive roles).

  col_info <- task$col_info
  type_of <- function(cols) col_info[list(cols), on = "id", "type"][[1L]]

  target_cols <- new_roles[["target"]]
  if (length(target_cols) > 1L) {
    stopf("TaskLCE may have at most one column with role 'target'")
  }
  if (length(target_cols) == 1L && type_of(target_cols) %nin% c("integer", "numeric")) {
    stopf("Target column '%s' must be numeric or integer", target_cols)
  }

  archive_y_cols <- new_roles[["archive_y"]]
  if (length(archive_y_cols)) {
    bad <- archive_y_cols[type_of(archive_y_cols) %nin% c("integer", "numeric")]
    if (length(bad)) {
      stopf("archive_y column(s) %s must be numeric or integer",
        str_collapse(bad, quote = "'"))
    }
  }

  NextMethod()
}
