#' @title Forecast Tracker
#'
#' @description
#' Tracks forecast updates from a `CurveExtrapolator` during a run.
#'
#' @details
#' A `ForecastTracker` consumes a metrics history table (typically from
#' [MetricsTracker]) and periodically retrains its extrapolator, then stores the
#' resulting forecast summaries and prediction curves.
#'
#' @export
ForecastTracker <- R6Class("ForecastTracker",
  public = list(

    #' @field extrapolator (`CurveExtrapolator`)
    extrapolator = NULL,

    #' @field metric_name (`character(1)`)
    metric_name = NULL,

    #' @field x_col (`character(1)`)
    x_col = NULL,

    #' @field direction (`character(1)`)
    direction = NULL,

    #' @field target (`numeric(1)` | `NULL`)
    target = NULL,

    #' @field n_evals_budget (`integer(1)` | `NULL`)
    n_evals_budget = NULL,

    #' @description
    #' Creates a new ForecastTracker.
    #'
    #' @param extrapolator (`CurveExtrapolator`)
    #'   Extrapolator used for forecasting.
    #' @param metric_name (`character(1)`)
    #'   Metric column in incoming metrics data.
    #' @param x_col (`character(1)`)
    #'   Evaluation-count column in incoming metrics data.
    #' @param direction (`character(1)` | `NULL`)
    #'   Direction override. If `NULL`, uses `extrapolator$direction`.
    #' @param target (`numeric(1)` | `NULL`)
    #'   Optional target value.
    #' @param n_evals_budget (`integer(1)` | `NULL`)
    #'   Optional budget horizon.
    #' @param min_points (`integer(1)`)
    #'   Minimum number of rows required before fitting.
    #' @param update_every (`integer(1)`)
    #'   Update period in rows.
    #' @param default_horizon (`integer(1)`)
    #'   Additional horizon when `n_evals_budget` is `NULL`.
    #' @param training_args (`list`)
    #'   Extra arguments passed to `extrapolator$train()`.
    initialize = function(extrapolator,
        metric_name,
        x_col = "n_evals",
        direction = NULL,
        target = NULL,
        n_evals_budget = NULL,
        min_points = 5L,
        update_every = 1L,
        default_horizon = 20L,
        training_args = list()) {
      assert_r6(extrapolator, "CurveExtrapolator")
      assert_string(metric_name)
      assert_string(x_col)
      assert_choice(direction, c("minimize", "maximize"), null.ok = TRUE)
      assert_number(target, null.ok = TRUE)
      assert_int(n_evals_budget, lower = 1L, null.ok = TRUE)
      assert_int(min_points, lower = 2L)
      assert_int(update_every, lower = 1L)
      assert_int(default_horizon, lower = 1L)
      assert_list(training_args)
      if (length(training_args) > 0L && is.null(names(training_args))) {
        stopf("training_args must be a named list")
      }

      self$extrapolator <- extrapolator
      self$metric_name <- metric_name
      self$x_col <- x_col
      self$direction <- direction %??% extrapolator$direction
      self$target <- target
      self$n_evals_budget <- n_evals_budget

      private$.min_points <- min_points
      private$.update_every <- update_every
      private$.default_horizon <- default_horizon
      private$.training_args <- training_args

      private$.init_data()
    },

    #' @description
    #' Update forecast state from metrics history.
    #'
    #' @param metrics_data (`data.table`)
    #'   Metrics history table.
    #' @param ...
    #'   Extra arguments forwarded to `extrapolator$train()`, overriding
    #'   `training_args`.
    #'
    #' @return Invisibly returns the appended row or `NULL` if no update happened.
    update = function(metrics_data, ...) {
      assert_data_table(metrics_data, min.rows = 1L)
      assert_names(names(metrics_data), must.include = c(self$x_col, self$metric_name))

      if (nrow(metrics_data) < private$.min_points) {
        return(invisible(NULL))
      }
      if ((nrow(metrics_data) %% private$.update_every) != 0L) {
        return(invisible(NULL))
      }

      history <- copy(metrics_data[, c(self$x_col, self$metric_name), with = FALSE])
      setnames(history, c(self$x_col, self$metric_name), c(self$extrapolator$x_col, self$extrapolator$metric_col))

      training_args <- private$.training_args
      dot_args <- list(...)
      if (length(dot_args) > 0L) {
        for (name in names(dot_args)) {
          training_args[[name]] <- dot_args[[name]]
        }
      }

      train_call <- c(list(history = history), training_args)
      do.call(self$extrapolator$train, train_call)

      n_current <- as.integer(metrics_data[[self$x_col]][[nrow(metrics_data)]])
      n_budget <- self$n_evals_budget %??% (n_current + private$.default_horizon)
      n_budget <- max(n_budget, n_current)

      prediction_grid <- seq.int(n_current, n_budget)
      prediction <- self$extrapolator$predict(prediction_grid)

      target_stats <- if (is.null(self$target)) {
        list(p_reach_budget = NA_real_, n_evals_q50 = NA_integer_, n_evals_q90 = NA_integer_)
      } else {
        self$extrapolator$predict_target(target = self$target, n_evals_budget = n_budget)
      }

      batch_nr <- if ("batch_nr" %in% names(metrics_data)) {
        as.integer(metrics_data$batch_nr[[nrow(metrics_data)]])
      } else {
        NA_integer_
      }

      new_row <- data.table(
        update_nr = as.integer(nrow(private$.data) + 1L),
        batch_nr = batch_nr,
        n_evals = n_current,
        metric_value = as.numeric(metrics_data[[self$metric_name]][[nrow(metrics_data)]]),
        budget_n_evals = as.integer(n_budget),
        p_reach_budget = as.numeric(target_stats$p_reach_budget),
        n_evals_q50 = as.integer(target_stats$n_evals_q50),
        n_evals_q90 = as.integer(target_stats$n_evals_q90),
        timestamp = Sys.time(),
        prediction = list(prediction)
      )

      private$.data <- rbind(private$.data, new_row)
      invisible(new_row)
    },

    #' @description
    #' Set/replace training arguments for future updates.
    #'
    #' @param ... Named arguments.
    set_training_args = function(...) {
      args <- list(...)
      if (length(args) > 0L && is.null(names(args))) {
        stopf("Arguments must be named")
      }
      for (name in names(args)) {
        private$.training_args[[name]] <- args[[name]]
      }
      invisible(self)
    },

    #' @description
    #' Clear forecast history.
    #'
    #' @param clear_extrapolator (`logical(1)`)
    #'   Whether to also clear the extrapolator state.
    clear = function(clear_extrapolator = FALSE) {
      assert_flag(clear_extrapolator)
      private$.init_data()
      if (clear_extrapolator) {
        self$extrapolator$clear()
      }
      invisible(self)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat(sprintf("<%s>\n", class(self)[[1L]]))
      cat(sprintf("* Extrapolator: %s\n", class(self$extrapolator)[[1L]]))
      cat(sprintf("* Metric: %s\n", self$metric_name))
      cat(sprintf("* X column: %s\n", self$x_col))
      cat(sprintf("* Updates: %i\n", self$n_updates))
      if (!is.null(self$target)) {
        cat(sprintf("* Target: %.6g\n", self$target))
      }
      invisible(self)
    }
  ),

  active = list(

    #' @field data (`data.table`)
    #'   Forecast history table.
    data = function(rhs) {
      assert_ro_binding(rhs)
      copy(private$.data)
    },

    #' @field latest (`data.table` | `NULL`)
    #'   Most recent forecast update.
    latest = function(rhs) {
      assert_ro_binding(rhs)
      if (nrow(private$.data) == 0L) return(NULL)
      private$.data[.N]
    },

    #' @field n_updates (`integer(1)`)
    #'   Number of forecast updates.
    n_updates = function(rhs) {
      assert_ro_binding(rhs)
      nrow(private$.data)
    },

    #' @field min_points (`integer(1)`)
    #'   Minimum rows needed before updates start.
    min_points = function(rhs) {
      if (!missing(rhs)) {
        private$.min_points <- assert_int(rhs, lower = 2L)
      }
      private$.min_points
    },

    #' @field update_every (`integer(1)`)
    #'   Update period.
    update_every = function(rhs) {
      if (!missing(rhs)) {
        private$.update_every <- assert_int(rhs, lower = 1L)
      }
      private$.update_every
    }
  ),

  private = list(
    .data = NULL,
    .min_points = NULL,
    .update_every = NULL,
    .default_horizon = NULL,
    .training_args = NULL,

    .init_data = function() {
      private$.data <- data.table(
        update_nr = integer(0),
        batch_nr = integer(0),
        n_evals = integer(0),
        metric_value = numeric(0),
        budget_n_evals = integer(0),
        p_reach_budget = numeric(0),
        n_evals_q50 = integer(0),
        n_evals_q90 = integer(0),
        timestamp = as.POSIXct(character(0)),
        prediction = list()
      )
    },

    deep_clone = function(name, value) {
      switch(name,
        .data = copy(value),
        extrapolator = value$clone(deep = TRUE),
        .training_args = lapply(value, function(x) x),
        value
      )
    }
  )
)
