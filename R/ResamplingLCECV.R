#' @title LCE Cross-Validation Resampling
#'
#' @name mlr_resamplings_lce_cv
#'
#' @description
#' Splits learning curve data using rolling/expanding window cross-validation.
#' Training uses the first part of each curve, testing uses the subsequent
#' `horizon` observations.
#'
#' @section Parameters:
#' * `horizon` (`integer(1)`)\cr
#'   Number of test observations per fold (default 1).
#' * `folds` (`integer(1)`)\cr
#'   Number of folds (default 5).
#' * `step_size` (`integer(1)`)\cr
#'   Step between consecutive training window endpoints (default 1).
#' * `window_size` (`integer(1)`)\cr
#'   Minimum training size for expanding window, or exact training size for
#'   fixed window (default 5).
#' * `fixed_window` (`logical(1)`)\cr
#'   If `TRUE`, use a fixed-size training window. If `FALSE` (default), use
#'   an expanding window from the beginning of the curve.
#'
#' @export
ResamplingLCECV = R6Class("ResamplingLCECV",
  inherit = Resampling,
  public = list(

    #' @description
    #' Creates a new instance.
    initialize = function() {
      param_set = ps(
        horizon = p_int(1L, tags = "required"),
        folds = p_int(1L, tags = "required"),
        step_size = p_int(1L, tags = "required"),
        window_size = p_int(2L, tags = "required"),
        fixed_window = p_lgl(tags = "required")
      )
      param_set$set_values(
        horizon = 1L,
        folds = 5L,
        step_size = 1L,
        window_size = 5L,
        fixed_window = FALSE
      )

      super$initialize(
        id = "lce_cv",
        label = "LCE Cross-Validation",
        param_set = param_set,
        man = "celecx::mlr_resamplings_lce_cv"
      )
    }
  ),

  active = list(
    #' @field iters (`integer(1)`)
    #'   Number of folds.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      as.integer(self$param_set$get_values()$folds)
    }
  ),

  private = list(
    .sample = function(ids, task, ...) {
      if ("ordered" %nin% task$properties) {
        stopf("Resampling '%s' requires an ordered task, but Task '%s' has no order.",
          self$id, task$id)
      }

      pv = self$param_set$get_values()
      window_size = pv$window_size
      horizon = pv$horizon
      fixed_window = pv$fixed_window
      step_size = pv$step_size
      folds = pv$folds

      col_roles = task$col_roles
      order_cols = col_roles$order
      key_cols = col_roles$key
      has_key = length(key_cols) > 0L

      dt = task$backend$data(
        rows = ids,
        cols = c(task$backend$primary_key, order_cols, key_cols)
      )
      setnames(dt, task$backend$primary_key, "row_id")

      if (!has_key) {
        setorderv(dt, order_cols)
        n = nrow(dt)
        private$.validate_window(n, window_size, horizon, folds, step_size)
        splits = private$.compute_splits(dt, n, window_size, horizon,
          folds, step_size, fixed_window)
        return(splits)
      }

      # Multi-curve: split per key group, merge by fold index
      setorderv(dt, c(key_cols, order_cols))
      group_splits = dt[, {
        n_group = .N
        private$.validate_window(n_group, window_size, horizon, folds, step_size)
        result = private$.compute_splits(.SD, n_group, window_size, horizon,
          folds, step_size, fixed_window)
        list(
          train_ids = result$train,
          test_ids = result$test,
          fold = seq_len(folds)
        )
      }, by = key_cols]

      # Merge across groups by fold
      merged = group_splits[, list(
        train = list(unlist(train_ids, use.names = FALSE)),
        test = list(unlist(test_ids, use.names = FALSE))
      ), by = "fold"]

      list(train = merged$train, test = merged$test)
    },

    .validate_window = function(n, window_size, horizon, folds, step_size) {
      if (window_size + horizon > n) {
        stopf(
          "Resampling '%s': window_size + horizon (%i) exceeds observations (%i).",
          self$id, window_size + horizon, n
        )
      }
      max_folds = floor((n - horizon - window_size) / step_size) + 1L
      if (folds > max_folds) {
        stopf(
          "Resampling '%s': folds (%i) exceeds maximum feasible folds (%i).",
          self$id, folds, max_folds
        )
      }
    },

    .compute_splits = function(dt, n, window_size, horizon, folds, step_size,
        fixed_window) {
      # Compute training window endpoints (earliest first)
      train_end = rev(seq(
        from = n - horizon, by = -step_size, length.out = folds
      ))

      if (fixed_window) {
        train_ids = map(train_end, function(i) {
          dt[(i - window_size + 1L):i, "row_id"][[1L]]
        })
      } else {
        train_ids = map(train_end, function(i) {
          dt[1L:i, "row_id"][[1L]]
        })
      }
      test_ids = map(train_end, function(i) {
        dt[(i + 1L):(i + horizon), "row_id"][[1L]]
      })
      list(train = train_ids, test = test_ids)
    },

    .get_train = function(i) {
      self$instance$train[[i]]
    },

    .get_test = function(i) {
      self$instance$test[[i]]
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },

    deep_clone = function(name, value) {
      switch(name,
        instance = copy(value),
        param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)

#' @include aaa.R
resamplings[["lce_cv"]] = ResamplingLCECV
