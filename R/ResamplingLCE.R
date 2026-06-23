#' @title Expanding-Window LCE Cross-Validation
#'
#' @name mlr_resamplings_lce.expanding_cv
#'
#' @include TaskLCE.R
#'
#' @description
#' Time-series style cross-validation for [TaskLCE]. Splits are made by whole
#' batches (column with role `feature`, i.e. the `batch_nr` column of the task).
#' Each fold trains on all batches from the very first up to a moving training
#' end and tests on the next `horizon` batches; the training window therefore
#' always starts at the beginning and expands by `step_size` batches between
#' folds.
#'
#' @section Parameters:
#' * `horizon` :: `integer(1)`\cr
#'   Number of consecutive batches used as test set in every fold. Initialized
#'   to `1`.
#' * `step_size` :: `integer(1)`\cr
#'   Number of batches between the train-end indices of consecutive folds.
#'   Initialized to `1`.
#' * `min_train_batches` :: `integer(1)`\cr
#'   Number of batches in the training set of the first fold. Required, with no
#'   default: results are sensitive to it and there is no good universal value.
#'   Must be at least the wrapped learner's minimum training requirement, which
#'   is larger for some learners (e.g. [LearnerLCESplineMonotone] needs five
#'   batches, [LearnerLCEConformal] needs `n_calibration_batches + 1`).
#' * `folds` :: `integer(1)` | `NULL`\cr
#'   Maximum number of folds. When unset, all feasible folds are generated.
#'
#' @export
ResamplingLCE <- R6Class("ResamplingLCE",
  inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new expanding-window LCE resampling.
    initialize = function() {
      param_set <- ps(
        horizon = p_int(lower = 1L, init = 1L, tags = "required"),
        step_size = p_int(lower = 1L, init = 1L, tags = "required"),
        min_train_batches = p_int(lower = 1L, tags = "required"),
        folds = p_int(lower = 1L)
      )

      super$initialize(
        id = "lce.expanding_cv",
        param_set = param_set,
        label = "Expanding-Window LCE Cross-Validation",
        man = "celecx::mlr_resamplings_lce.expanding_cv"
      )
    }
  ),

  active = list(
    #' @field iters (`integer(1)`)\cr
    #' Number of resampling iterations. Only meaningful after `$instantiate()`.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      if (!self$is_instantiated) return(NA_integer_)
      length(self$instance$train)
    }
  ),

  private = list(
    .sample = function(ids, task, ...) {
      if (!inherits(task, "TaskLCE")) {
        stopf("Resampling '%s' requires a TaskLCE", self$id)
      }
      unsupported <- intersect(task$properties, c("groups", "strata"))
      if (length(unsupported)) {
        stopf("Resampling '%s' does not support grouped or stratified TaskLCEs; remove role(s): %s",
          self$id, str_collapse(unsupported, quote = "'"))
      }

      pv <- self$param_set$get_values(check_required = FALSE)
      horizon <- pv$horizon
      step_size <- pv$step_size
      min_train_batches <- pv$min_train_batches
      if (is.null(min_train_batches)) {
        stopf("Resampling '%s': `min_train_batches` must be set (it has no default; choose at least the learner's minimum number of training batches)",
          self$id)
      }

      pk <- task$backend$primary_key
      batch_col <- task$col_roles$feature
      dt <- task$backend$data(rows = ids, cols = c(pk, batch_col))
      setnames(dt, c(pk, batch_col), c("row_id", "batch"))

      batches <- sort(unique(dt$batch))
      n_batches <- length(batches)
      if (n_batches < min_train_batches + horizon) {
        stopf(
          "Resampling '%s': need at least %i batches but task '%s' has %i",
          self$id, min_train_batches + horizon, task$id, n_batches
        )
      }

      max_train_end_idx <- n_batches - horizon
      train_end_idx <- seq.int(min_train_batches, max_train_end_idx, by = step_size)
      if (!is.null(pv$folds)) {
        if (pv$folds > length(train_end_idx)) {
          stopf(
            "Resampling '%s': requested %i folds but only %i feasible for task '%s'",
            self$id, pv$folds, length(train_end_idx), task$id
          )
        }
        train_end_idx <- train_end_idx[seq_len(pv$folds)]
      }

      train_sets <- lapply(train_end_idx, function(idx) {
        bb <- batches[seq_len(idx)]
        dt[batch %in% bb, row_id]
      })
      test_sets <- lapply(train_end_idx, function(idx) {
        bb <- batches[seq.int(idx + 1L, idx + horizon)]
        dt[batch %in% bb, row_id]
      })

      list(train = train_sets, test = test_sets)
    },

    .get_train = function(i) self$instance$train[[i]],
    .get_test = function(i) self$instance$test[[i]],
    .combine = function(instances) {
      list(
        train = unlist(map(instances, "train"), recursive = FALSE),
        test = unlist(map(instances, "test"), recursive = FALSE)
      )
    },

    deep_clone = function(name, value) {
      switch(name,
        instance = if (is.null(value)) NULL else {
          list(train = map(value$train, copy), test = map(value$test, copy))
        },
        param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)

#' @include aaa.R
resamplings[["lce.expanding_cv"]] <- ResamplingLCE
