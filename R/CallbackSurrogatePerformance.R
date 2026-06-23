#' @title Surrogate Performance Callback
#'
#' @name celecx.surrogate_performance
#'
#' @include TaskLCE.R
#'
#' @description
#' Evaluates a named [OptimizerAL] surrogate on a held-out regression task after
#' every evaluated batch.
#'
#' The callback stores one row per batch in `$data`. Rows include the archive
#' batch number, cumulative number of evaluations, timestamp, surrogate id, and
#' one column per configured regression measure.
#'
#' @examples
#' \dontrun{
#' perf <- clbk("celecx.surrogate_performance",
#'   surrogate_id = "uncertainty",
#'   task = test_task,
#'   measures = list(r2 = msr("regr.rsq"), mae = msr("regr.mae"))
#' )
#' }
NULL

# Shared internals between CallbackSurrogatePerformance and
# replay_surrogate_performance(); each does real work beyond argument checks.

# Validate, clone, and name a list of regression measures: named entries keep
# their names, unnamed entries fall back to the measure id.
normalize_regr_measures <- function(measures) {
  assert_list(measures, min.len = 1L, .var.name = "measures")
  measure_names <- names2(measures, missing_val = "")
  measures <- lapply(measures, function(measure) {
    assert_r6(measure, "Measure")
    if (!identical(measure$task_type, "regr")) {
      stopf("Measure '%s' must be a regression measure", measure$id)
    }
    measure$clone(deep = TRUE)
  })
  use_ids <- !nzchar(measure_names)
  measure_names[use_ids] <- map_chr(measures, "id")[use_ids]
  assert_names(measure_names, type = "unique")
  set_names(measures, measure_names)
}

# Predict an (already updated) surrogate on a held-out regression task and score
# it with each measure, returning a named numeric vector keyed by names(measures).
# If the surrogate carries an output transformation that it does not invert
# itself (output_trafo_must_be_considered), the transformation is inverted here
# so scores are computed on the objective's original scale.
score_surrogate_on_task <- function(surrogate, task, measures) {
  cols_x <- surrogate$cols_x
  missing_cols <- setdiff(cols_x, task$feature_names)
  if (length(missing_cols)) {
    stopf("Task '%s' is missing surrogate feature columns: %s",
      task$id, str_collapse(missing_cols, quote = "'"))
  }

  pred <- surrogate$predict(task$data(cols = cols_x))
  if (isTRUE(surrogate$output_trafo_must_be_considered)) {
    trafo <- surrogate$output_trafo
    pred_dt <- as.data.table(pred)
    pred <- if ("se" %chin% names(pred_dt)) {
      # proper moment-corrected inversion of the posterior predictive
      as.list(trafo$inverse_transform_posterior(pred_dt))
    } else {
      # no posterior se: invert the point prediction only
      col_y <- surrogate$cols_y
      inv <- trafo$inverse_transform(setnames(data.table(pred_dt$mean), col_y))
      list(mean = inv[[col_y]])
    }
  }

  if (!"mean" %chin% names(pred)) {
    stopf("Surrogate predictions must include a 'mean' column")
  }
  if (length(pred$mean) != task$nrow) {
    stopf("Surrogate returned %i predictions for a task with %i rows",
      length(pred$mean), task$nrow)
  }

  prediction <- PredictionRegr$new(
    task = task,
    response = pred$mean,
    se = if ("se" %chin% names(pred)) pred$se else NULL
  )

  map_dbl(measures, function(measure) {
    score <- measure$score(
      prediction = prediction, task = task, learner = surrogate$learner)
    if (!test_number(score, na.ok = TRUE)) {
      stopf("Measure '%s' did not return a numeric scalar", measure$id)
    }
    as.numeric(score)
  })
}

# Build a TaskLCE from an archive and a per-batch performance table. `perf` has a
# `batch_nr` column plus one column per measure; `measure` selects the target.
# `measure_obj` is the corresponding [mlr3::Measure] and `pool` the optional
# candidate pool; both are carried into the task as replay provenance.
task_lce_from_perf <- function(archive, perf, measure, measure_obj = NULL,
    pool = NULL, link = "identity", id, label = NA_character_) {
  archive_x <- archive$cols_x
  archive_y <- archive$cols_y
  if (measure %in% c(archive_x, archive_y, "batch_nr")) {
    stopf("Measure name '%s' clashes with an archive column", measure)
  }
  archive_data <- archive$data[, c(archive_x, archive_y, "batch_nr"), with = FALSE]
  perf_subset <- perf[, c("batch_nr", measure), with = FALSE]
  joined <- merge(archive_data, perf_subset, by = "batch_nr",
    all.x = FALSE, sort = FALSE)
  TaskLCE$new(
    id = id, backend = joined, target = measure, batch_nr = "batch_nr",
    archive_x = archive_x, archive_y = archive_y,
    search_space = archive$search_space, codomain = archive$codomain,
    measure = measure_obj, pool = pool, link = link, label = label
  )
}

surrogate_performance_on_optimization_begin <- function(callback, context) {
  assert_string(callback$state$surrogate_id, min.chars = 1L, .var.name = "surrogate_id")
  assert_r6(callback$state$task, "TaskRegr", .var.name = "task")

  measures <- callback$state$measures
  if (is.null(measures)) {
    measures <- list(msr("regr.rsq"), msr("regr.mae"))
  }
  callback$state$measures <- normalize_regr_measures(measures)

  callback$state$data <- data.table(
    batch_nr = integer(0),
    n_evals = integer(0),
    timestamp = as.POSIXct(character(0)),
    surrogate_id = character(0)
  )
  for (measure_name in names(callback$state$measures)) {
    set(callback$state$data, j = measure_name, value = numeric(0))
  }

  assert_r6(context$optimizer, "OptimizerAL")
  if (!callback$state$surrogate_id %in% names(context$optimizer$surrogates)) {
    stopf("Unknown surrogate id '%s'", callback$state$surrogate_id)
  }

  # Keep a reference to the live archive so that $task() can later build a
  # TaskLCE from the archive plus the recorded surrogate performance.
  callback$state$archive <- context$instance$archive

  # For pool-based runs, also keep the candidate pool (search-space columns only)
  # so the resulting TaskLCE can drive a pool-restricted replay.
  objective <- context$instance$objective
  callback$state$pool <- if ("pool_restricted" %in% objective$properties) {
    objective$pool[, context$instance$search_space$ids(), with = FALSE]
  } else {
    NULL
  }

  invisible(NULL)
}

surrogate_performance_on_optimizer_after_eval <- function(callback, context) {
  surrogate_id <- callback$state$surrogate_id
  task <- callback$state$task
  surrogate <- context$optimizer$get_surrogate(surrogate_id)

  scores <- score_surrogate_on_task(surrogate, task, callback$state$measures)

  row <- data.table(
    batch_nr = as.integer(context$instance$archive$n_batch),
    n_evals = as.integer(context$instance$archive$n_evals),
    timestamp = Sys.time(),
    surrogate_id = surrogate_id
  )
  for (measure_name in names(scores)) {
    set(row, j = measure_name, value = scores[[measure_name]])
  }

  callback$state$data <- rbind(callback$state$data, row, use.names = TRUE)
  invisible(row)
}

#' @rdname celecx.surrogate_performance
#' @export
CallbackSurrogatePerformance <- R6Class("CallbackSurrogatePerformance",
  inherit = bbotk::CallbackBatch,

  public = list(
    #' @description
    #' Creates a new CallbackSurrogatePerformance.
    #'
    #' @param surrogate_id (`character(1)`)\cr
    #'   Surrogate registry id in `optimizer$surrogates`.
    #' @param task ([mlr3::TaskRegr])\cr
    #'   Held-out regression task.
    #' @param measures (`list()` of [mlr3::Measure])\cr
    #'   Regression measures. Named lists use their names as output column names;
    #'   unnamed measures use their measure ids.
    initialize = function(surrogate_id = NULL, task = NULL,
        measures = list(msr("regr.rsq"), msr("regr.mae"))) {
      super$initialize(
        id = "celecx.surrogate_performance",
        label = "Surrogate Performance",
        man = "celecx::celecx.surrogate_performance"
      )

      if (!is.null(surrogate_id)) {
        self$state$surrogate_id <- assert_string(surrogate_id, min.chars = 1L)
      }
      if (!is.null(task)) {
        self$state$task <- assert_r6(task, "TaskRegr")
      }
      self$state$measures <- normalize_regr_measures(measures)

      self$on_optimization_begin <- surrogate_performance_on_optimization_begin
      self$on_optimizer_after_eval <- surrogate_performance_on_optimizer_after_eval
    },

    #' @description
    #' Clears the logged performance history and the stored archive reference.
    clear = function() {
      if (is.null(self$state$data)) {
        self$state$data <- data.table(
          batch_nr = integer(0),
          n_evals = integer(0),
          timestamp = as.POSIXct(character(0)),
          surrogate_id = character(0)
        )
      } else {
        self$state$data <- self$state$data[0L]
      }
      self$state$archive <- NULL
      self$state$pool <- NULL
      invisible(self)
    },

    #' @description
    #' Build a [TaskLCE] from the recorded archive and surrogate performance.
    #'
    #' The resulting task has one row per archive evaluation. Its columns are
    #' the archive search-space columns (role `archive_x`), the archive
    #' codomain target columns (role `archive_y`), the `batch_nr` column
    #' (role `feature`), and the selected per-batch surrogate-performance
    #' column (role `target`). The archive's search space and codomain, the
    #' selected regression measure, and (for pool-based runs) the candidate
    #' pool are carried along for extrapolators that replay the active-learning
    #' loop.
    #'
    #' @param measure (`character(1)` | `NULL`)\cr
    #'   Name of the measure column to use as task target. Refers to the
    #'   `$state$measures` names. When `NULL` (default) and the callback was
    #'   configured with exactly one measure, that measure is used.
    #' @param id (`character(1)`)\cr
    #'   Task id. Defaults to `"surrogate_performance"`.
    #' @param link (`character(1)`)\cr
    #'   Name of the predictive [lce_link] for the resulting [TaskLCE].
    #'   `"identity"` by default.
    #' @param label (`character(1)`)\cr
    #'   Optional task label.
    #'
    #' @return [TaskLCE].
    task = function(measure = NULL, id = "surrogate_performance",
        link = "identity", label = NA_character_) {
      if (is.null(self$state$archive)) {
        stop("Callback has no archive reference; run the optimizer first")
      }
      perf <- self$state$data
      if (is.null(perf) || !nrow(perf)) {
        stop("Callback has no recorded performance entries")
      }

      measure_names <- names(self$state$measures)
      if (is.null(measure)) {
        if (length(measure_names) != 1L) {
          stopf(
            "Callback has %i measures; supply 'measure' to pick one of: %s",
            length(measure_names), str_collapse(measure_names, quote = "'")
          )
        }
        measure <- measure_names
      } else {
        assert_choice(measure, measure_names, .var.name = "measure")
      }

      task_lce_from_perf(self$state$archive, perf, measure,
        measure_obj = self$state$measures[[measure]],
        pool = self$state$pool, link = link, id = id, label = label)
    }
  ),

  active = list(
    #' @field data (`data.table`)\cr
    #' Logged performance history.
    data = function(rhs) {
      if (!missing(rhs)) {
        stop("data is read-only; use clear() to reset the callback")
      }
      if (is.null(self$state$data)) {
        return(data.table(
          batch_nr = integer(0),
          n_evals = integer(0),
          timestamp = as.POSIXct(character(0)),
          surrogate_id = character(0)
        ))
      }
      copy(self$state$data)
    },

    #' @field latest (`data.table` | `NULL`)\cr
    #' Most recently logged row.
    latest = function(rhs) {
      if (!missing(rhs)) {
        stop("latest is read-only")
      }
      if (is.null(self$state$data) || !nrow(self$state$data)) {
        return(NULL)
      }
      copy(self$state$data[.N])
    }
  )
)


# Lazy loader function for dictionary registration
load_callback_surrogate_performance <- function() {
  CallbackSurrogatePerformance$new()
}
