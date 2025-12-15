#' @title Metrics Tracker
#'
#' @description
#' Tracks per-batch metrics during a search, providing the data needed for
#' progress monitoring, learning curve analysis, and forecasting.
#'
#' @details
#' The tracker stores a history table with one row per batch. Built-in columns:
#' - `batch_nr`: Batch number (1-indexed)
#' - `n_evals`: Cumulative number of evaluations
#' - `timestamp`: When the batch was logged
#'
#' Additional columns are computed by user-defined metric functions. These
#' functions receive the current state (archive, surrogate, extra data) and
#' return a scalar value.
#'
#' @section Metric Functions:
#' Metric functions should have signature:
#' ```
#' function(archive, surrogate = NULL, ...)
#' ```
#' They should return a single numeric value. The `...` can include additional
#' context like test data, known optimum, etc.
#'
#' See `search_metrics.R` for built-in metrics:
#' - `metric_best_y()`: Best observed y value (optimization)
#' - `metric_regret()`: Gap to known optimum (optimization)
#' - `metric_model_rmse()`: Surrogate RMSE on test data (learning)
#' - `metric_mean_variance()`: Average prediction variance (learning)
#'
#' @section Usage with Forecasters:
#' The `$data` field returns the complete history as a `data.table`, which
#' can be directly fed to learning curve extrapolation models. The `n_evals`
#' column serves as the x-axis (sample size) for most forecasting methods.
#'
#' @export
MetricsTracker <- R6Class("MetricsTracker",
  public = list(

    #' @description
    #' Creates a new MetricsTracker.
    #'
    #' @param metrics (`list`)\cr
    #'   Named list of metric functions. Names become column names in the
    #'   history table. Each function should have signature
    #'   `function(archive, surrogate = NULL, ...)` and return a scalar.
    #' @param extra_data (`list`)\cr
    #'   Additional data passed to metric functions via `...`. For example,
    #'   `list(test_data = test_dt, optimum = 0.5)`.
    initialize = function(metrics = list(), extra_data = list()) {
      # Validate metrics is a named list of functions
      assert_list(metrics)
      if (length(metrics) > 0L) {
        assert_names(names(metrics), type = "unique")
        for (i in seq_along(metrics)) {
          if (!is.function(metrics[[i]])) {
            stopf("metrics[[%d]] ('%s') must be a function", i, names(metrics)[[i]])
          }
        }
      }

      # Check for reserved column names
      reserved <- c("batch_nr", "n_evals", "timestamp")
      conflicts <- intersect(names(metrics), reserved)
      if (length(conflicts) > 0L) {
        stopf("Metric names conflict with reserved columns: %s",
          str_collapse(conflicts, quote = "'"))
      }

      # Validate extra_data
      assert_list(extra_data)
      if (length(extra_data) > 0L && is.null(names(extra_data))) {
        stopf("extra_data must be a named list")
      }

      # Store metrics and extra_data
      private$.metrics <- metrics
      private$.extra_data <- extra_data

      # Initialize empty history table
      private$.init_data()
    },

    #' @description
    #' Log metrics for a completed batch.
    #'
    #' @param batch_nr (`integer(1)`)\cr
    #'   The batch number being logged.
    #' @param archive ([ArchiveBatch])\cr
    #'   Current archive with all evaluations.
    #' @param surrogate ([mlr3mbo::Surrogate])\cr
    #'   Current fitted surrogate (optional, may be NULL).
    #' @param ... \cr
    #'   Additional arguments passed to metric functions. These override
    #'   values in `extra_data` if there are conflicts.
    #'
    #' @return Invisibly returns the new row as a `data.table`.
    log_batch = function(batch_nr, archive, surrogate = NULL, ...) {
      assert_int(batch_nr, lower = 1L, tol = 0)
      assert_r6(archive, "Archive")

      # Create base row with built-in columns
      new_row <- data.table(
        batch_nr = as.integer(batch_nr),
        n_evals = archive$n_evals,
        timestamp = Sys.time()
      )

      # Merge extra_data with dynamic args (dynamic overrides)
      dynamic_args <- list(...)
      extra_args <- private$.extra_data
      if (length(dynamic_args) > 0L) {
        # Dynamic args override extra_data
        for (nm in names(dynamic_args)) {
          extra_args[[nm]] <- dynamic_args[[nm]]
        }
      }

      # Compute each metric
      for (metric_name in names(private$.metrics)) {
        metric_fun <- private$.metrics[[metric_name]]
        value <- private$.compute_metric(metric_fun, archive, surrogate, extra_args)
        set(new_row, j = metric_name, value = value)
      }

      # Append to history
      private$.data <- rbind(private$.data, new_row)

      invisible(new_row)
    },

    #' @description
    #' Add a new metric to track (for future batches).
    #'
    #' @param name (`character(1)`)\cr
    #'   Column name for the metric.
    #' @param fun (`function`)\cr
    #'   Metric function.
    #' @param backfill (`logical(1)`)\cr
    #'   If TRUE and there's existing history, attempt to compute the metric
    #'   for past batches. Requires archive snapshots which may not be available.
    add_metric = function(name, fun, backfill = FALSE) {
      assert_string(name)
      assert_function(fun)
      assert_flag(backfill)

      # Check for reserved column names
      reserved <- c("batch_nr", "n_evals", "timestamp")
      if (name %in% reserved) {
        stopf("Metric name '%s' conflicts with reserved column", name)
      }

      # Check for duplicate
      if (name %in% names(private$.metrics)) {
        stopf("Metric '%s' already exists", name)
      }

      # Add to metrics list
      private$.metrics[[name]] <- fun

      # Add column to data table (with NA for existing rows)
      if (self$n_batches > 0L) {
        if (backfill) {
          warningf("Backfill requested but archive snapshots are not available; new column filled with NA")
        }
        set(private$.data, j = name, value = NA_real_)
      }

      invisible(self)
    },

    #' @description
    #' Remove a metric from tracking.
    #'
    #' @param name (`character(1)`)\cr
    #'   Name of the metric to remove.
    #' @param keep_data (`logical(1)`)\cr
    #'   If TRUE, keep the column in the data table. Default FALSE removes it.
    remove_metric = function(name, keep_data = FALSE) {
      assert_string(name)
      assert_flag(keep_data)

      if (!name %in% names(private$.metrics)) {
        stopf("Metric '%s' not found", name)
      }

      # Remove from metrics list
      private$.metrics[[name]] <- NULL

      # Optionally remove from data table
      if (!keep_data && name %in% names(private$.data)) {
        set(private$.data, j = name, value = NULL)
      }

      invisible(self)
    },

    #' @description
    #' Update extra_data used by metric functions.
    #'
    #' @param ... Named arguments to add/update in extra_data.
    set_extra_data = function(...) {
      args <- list(...)
      if (length(args) > 0L && is.null(names(args))) {
        stopf("Arguments must be named")
      }
      for (nm in names(args)) {
        private$.extra_data[[nm]] <- args[[nm]]
      }
      invisible(self)
    },

    #' @description
    #' Clear all logged history.
    clear = function() {
      private$.init_data()
      invisible(self)
    },

    #' @description
    #' Get metrics for a specific batch.
    #'
    #' @param batch_nr (`integer(1)`)\cr
    #'   The batch number to retrieve.
    #'
    #' @return A single-row `data.table`, or NULL if batch not found.
    get_batch = function(batch_nr) {
      assert_int(batch_nr, lower = 1L, tol = 0)
      batch_nr_val <- as.integer(batch_nr)
      row <- private$.data[get("batch_nr") == batch_nr_val]
      if (nrow(row) == 0L) return(NULL)
      row
    },

    #' @description
    #' Extract a single metric's history as a vector.
    #'
    #' @param name (`character(1)`)\cr
    #'   Name of the metric.
    #'
    #' @return A numeric vector of metric values.
    get_metric_history = function(name) {
      assert_string(name)
      if (!name %in% names(private$.data)) {
        stopf("Metric '%s' not found in data", name)
      }
      private$.data[[name]]
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat(sprintf("<%s>\n", class(self)[[1L]]))
      cat(sprintf("* Batches logged: %d\n", self$n_batches))
      cat(sprintf("* Metrics tracked: %s\n",
        if (length(self$metric_names) == 0L) "(none)"
        else str_collapse(self$metric_names, n = 5)))

      if (length(private$.extra_data) > 0L) {
        cat(sprintf("* Extra data keys: %s\n",
          str_collapse(names(private$.extra_data), n = 5)))
      }

      if (self$n_batches > 0L) {
        cat("\n* Latest batch:\n")
        latest <- self$latest
        for (col in names(latest)) {
          val <- latest[[col]]
          if (is.numeric(val)) {
            cat(sprintf("  - %s: %.4g\n", col, val))
          } else {
            cat(sprintf("  - %s: %s\n", col, as.character(val)))
          }
        }
      }

      invisible(self)
    }
  ),

  active = list(

    #' @field data (`data.table`)\cr
    #'   The history table with all logged metrics.
    data = function(rhs) {
      if (!missing(rhs)) stop("data is read-only; use log_batch() to add rows")
      copy(private$.data)
    },

    #' @field metrics (`list`)\cr
    #'   The registered metric functions.
    metrics = function(rhs) {
      if (!missing(rhs)) stop("metrics is read-only; use add_metric()")
      private$.metrics
    },

    #' @field extra_data (`list`)\cr
    #'   Additional data passed to metric functions.
    extra_data = function(rhs) {
      if (!missing(rhs)) stop("extra_data is read-only; use set_extra_data()")
      private$.extra_data
    },

    #' @field n_batches (`integer(1)`)\cr
    #'   Number of batches logged.
    n_batches = function() {
      nrow(private$.data)
    },

    #' @field metric_names (`character()`)\cr
    #'   Names of all tracked metrics (excluding built-in columns).
    metric_names = function() {
      names(private$.metrics)
    },

    #' @field latest (`data.table`)\cr
    #'   The most recently logged row, or NULL if no history.
    latest = function() {
      if (self$n_batches == 0L) return(NULL)
      private$.data[.N]
    }
  ),

  private = list(
    .data = NULL,
    .metrics = NULL,
    .extra_data = NULL,

    #' Compute a single metric value.
    #'
    #' @param metric_fun (`function`) The metric function.
    #' @param archive ([ArchiveBatch]) Current archive.
    #' @param surrogate ([mlr3mbo::Surrogate]) Current surrogate (may be NULL).
    #' @param extra_args (`list`) Extra arguments for the function.
    #' @return Scalar metric value.
    .compute_metric = function(metric_fun, archive, surrogate, extra_args) {
      # Call metric_fun with archive, surrogate, and extra_args
      # Handle errors gracefully (return NA with warning)
      tryCatch({
        result <- do.call(metric_fun,
          c(list(archive = archive, surrogate = surrogate), extra_args))

        # Validate result is scalar
        if (!is.numeric(result) || length(result) != 1L) {
          warningf("Metric function returned non-scalar; coercing to NA")
          return(NA_real_)
        }
        as.numeric(result)
      }, error = function(e) {
        warningf("Error computing metric: %s", conditionMessage(e))
        NA_real_
      })
    },

    #' Initialize the empty history table.
    .init_data = function() {
      # Create empty data.table with columns:
      # batch_nr (integer), n_evals (integer), timestamp (POSIXct)
      # Plus one column per metric (all numeric, start empty)
      private$.data <- data.table(
        batch_nr = integer(0),
        n_evals = integer(0),
        timestamp = as.POSIXct(character(0))
      )

      # Add metric columns
      for (metric_name in names(private$.metrics)) {
        set(private$.data, j = metric_name, value = numeric(0))
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        .data = copy(value),
        .extra_data = lapply(value, function(x) {
          if (is.data.table(x)) copy(x)
          else if (is.environment(x) && !is.null(x[[".__enclos_env__"]])) x$clone(deep = TRUE)
          else x
        }),
        value
      )
    }
  )
)


#' @title Create Metrics Tracker
#'
#' @description
#' Convenience constructor for [MetricsTracker] with commonly used metrics.
#'
#' @param goal (`character(1)`)\cr
#'   Either `"optimize"`, `"learn"`, or `"both"`. Determines which default
#'   metrics to include.
#' @param metrics (`list`)\cr
#'   Additional custom metrics to include.
#' @param ... Extra data passed to metric functions (e.g., test_data, optimum).
#'
#' @return A [MetricsTracker] object.
#'
#' @examples
#' \dontrun{
#' # For optimization
#' tracker <- metrics_tracker("optimize", optimum = 0.0)
#'
#' # For active learning with test set
#' tracker <- metrics_tracker("learn", test_data = test_dt)
#'
#' # For both goals
#' tracker <- metrics_tracker("both", test_data = test_dt, optimum = 0.0)
#' }
#'
#' @export
metrics_tracker <- function(goal = c("optimize", "learn", "both"),
    metrics = list(), ...) {
  goal <- match.arg(goal)

  # Build default metrics based on goal
  default_metrics <- list()

  if (goal %in% c("optimize", "both")) {
    default_metrics$best_y <- metric_best_y
    # Regret requires optimum, added if provided in ...
    extra <- list(...)
    if ("optimum" %in% names(extra)) {
      default_metrics$regret <- metric_regret
    }
  }

  if (goal %in% c("learn", "both")) {
    # Variance metrics require surrogate
    default_metrics$mean_variance <- metric_mean_variance

    # RMSE requires test_data and target
    extra <- list(...)
    if ("test_data" %in% names(extra) && "target" %in% names(extra)) {
      default_metrics$model_rmse <- metric_model_rmse
    }
  }

  # Combine with custom metrics (custom overrides defaults)
  all_metrics <- c(default_metrics, metrics)

  MetricsTracker$new(metrics = all_metrics, extra_data = list(...))
}
