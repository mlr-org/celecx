#' @title Search Metrics
#'
#' @description
#' Built-in metric functions for tracking search progress. These functions
#' are used by [MetricsTracker] to compute per-batch summaries.
#'
#' @details
#' All metric functions follow the signature:
#' ```
#' function(archive, surrogate = NULL, ...)
#' ```
#'
#' They return a single numeric value. The `...` can receive additional
#' context like test data, known optimum, evaluation grid, etc.
#'
#' @section Optimization Metrics:
#' - `metric_best_y`: Best observed value (for minimization)
#' - `metric_worst_y`: Worst observed value
#' - `metric_regret`: Gap between current best and known optimum
#' - `metric_simple_regret`: Same as regret (alias)
#'
#' @section Learning Metrics:
#' - `metric_model_rmse`: Surrogate RMSE on test data
#' - `metric_model_mae`: Surrogate MAE on test data
#' - `metric_model_r2`: Surrogate R-squared on test data
#' - `metric_mean_variance`: Average prediction variance on a grid
#' - `metric_max_variance`: Maximum prediction variance on a grid
#' - `metric_integrated_variance`: Integral of variance over domain
#'
#' @section Progress Metrics:
#' - `metric_n_evals`: Total evaluations (built-in to tracker)
#' - `metric_improvement_rate`: Improvement per evaluation
#'
#' @name search_metrics
NULL


# =============================================================================
# Optimization Metrics
# =============================================================================

#' @title Best Y Metric
#'
#' @description
#' Returns the best (minimum) observed y value in the archive.
#'
#' @param archive ([ArchiveBatch]) The archive.
#' @param surrogate ([SurrogateModel] | [mlr3mbo::Surrogate]) Ignored.
#' @param target (`character(1)`) Target column name. If NULL, uses first
#'   codomain target.
#' @param ... Ignored.
#'
#' @return `numeric(1)` The minimum y value.
#'
#' @export
metric_best_y <- function(archive, surrogate = NULL, target = NULL, ...) {
  if (is.null(target)) {
    target <- archive$cols_y[[1L]]
  }
  min(archive$data[[target]], na.rm = TRUE)
}


#' @title Worst Y Metric
#'
#' @description
#' Returns the worst (maximum) observed y value in the archive.
#'
#' @param archive ([ArchiveBatch]) The archive.
#' @param surrogate ([SurrogateModel] | [mlr3mbo::Surrogate]) Ignored.
#' @param target (`character(1)`) Target column name.
#' @param ... Ignored.
#'
#' @return `numeric(1)` The maximum y value.
#'
#' @export
metric_worst_y <- function(archive, surrogate = NULL, target = NULL, ...) {
  if (is.null(target)) {
    target <- archive$cols_y[[1L]]
  }
  max(archive$data[[target]], na.rm = TRUE)
}


#' @title Regret Metric
#'
#' @description
#' Returns the gap between the current best and the known optimum.
#' Assumes minimization; for maximization, negate both values.
#'
#' @param archive ([ArchiveBatch]) The archive.
#' @param surrogate ([SurrogateModel] | [mlr3mbo::Surrogate]) Ignored.
#' @param optimum (`numeric(1)`) The known optimal value.
#' @param target (`character(1)`) Target column name.
#' @param ... Ignored.
#'
#' @return `numeric(1)` The regret (best_y - optimum).
#'
#' @export
metric_regret <- function(archive, surrogate = NULL, optimum, target = NULL, ...) {
  assert_number(optimum)
  metric_best_y(archive, target = target) - optimum
}


#' @title Simple Regret Metric
#'
#' @description
#' Alias for [metric_regret].
#'
#' @inheritParams metric_regret
#' @export
metric_simple_regret <- metric_regret


# =============================================================================
# Learning Metrics (require surrogate and test data)
# =============================================================================

#' @title Model RMSE Metric
#'
#' @description
#' Returns the root mean squared error of the surrogate's predictions on
#' test data.
#'
#' @param archive ([ArchiveBatch]) The archive (ignored for this metric).
#' @param surrogate ([SurrogateModel] | [mlr3mbo::Surrogate]) The fitted surrogate model.
#' @param test_data (`data.table`) Test data with features and target column.
#' @param target (`character(1)`) Target column name in test_data.
#' @param ... Ignored.
#'
#' @return `numeric(1)` The RMSE.
#'
#' @export
metric_model_rmse <- function(archive, surrogate = NULL, test_data, target, ...) {
  if (is.null(surrogate)) {
    stopf("metric_model_rmse requires a surrogate model")
  }
  # Check if fitted (SurrogateModel has $is_fitted, mlr3mbo::Surrogate doesn't)
  if (!is.null(surrogate$is_fitted) && !surrogate$is_fitted) {
    stopf("metric_model_rmse requires a fitted surrogate model")
  }
  assert_data_table(test_data)
  assert_string(target)
  if (!target %in% names(test_data)) {
    stopf("Target column '%s' not found in test_data", target)
  }

  pred <- surrogate$predict(test_data)
  y_true <- test_data[[target]]
  sqrt(mean((pred$mean - y_true)^2, na.rm = TRUE))
}


#' @title Model MAE Metric
#'
#' @description
#' Returns the mean absolute error of the surrogate's predictions on test data.
#'
#' @inheritParams metric_model_rmse
#'
#' @return `numeric(1)` The MAE.
#'
#' @export
metric_model_mae <- function(archive, surrogate = NULL, test_data, target, ...) {
  if (is.null(surrogate)) {
    stopf("metric_model_mae requires a surrogate model")
  }
  # Check if fitted (SurrogateModel has $is_fitted, mlr3mbo::Surrogate doesn't)
  if (!is.null(surrogate$is_fitted) && !surrogate$is_fitted) {
    stopf("metric_model_mae requires a fitted surrogate model")
  }
  assert_data_table(test_data)
  assert_string(target)
  if (!target %in% names(test_data)) {
    stopf("Target column '%s' not found in test_data", target)
  }

  pred <- surrogate$predict(test_data)
  y_true <- test_data[[target]]
  mean(abs(pred$mean - y_true), na.rm = TRUE)
}


#' @title Model R-squared Metric
#'
#' @description
#' Returns the R-squared (coefficient of determination) of the surrogate's
#' predictions on test data.
#'
#' @inheritParams metric_model_rmse
#'
#' @return `numeric(1)` The R-squared value.
#'
#' @export
metric_model_r2 <- function(archive, surrogate = NULL, test_data, target, ...) {
  if (is.null(surrogate)) {
    stopf("metric_model_r2 requires a surrogate model")
  }
  # Check if fitted (SurrogateModel has $is_fitted, mlr3mbo::Surrogate doesn't)
  if (!is.null(surrogate$is_fitted) && !surrogate$is_fitted) {
    stopf("metric_model_r2 requires a fitted surrogate model")
  }
  assert_data_table(test_data)
  assert_string(target)
  if (!target %in% names(test_data)) {
    stopf("Target column '%s' not found in test_data", target)
  }

  pred <- surrogate$predict(test_data)
  y_true <- test_data[[target]]
  y_mean <- mean(y_true, na.rm = TRUE)

  ss_res <- sum((y_true - pred$mean)^2, na.rm = TRUE)
  ss_tot <- sum((y_true - y_mean)^2, na.rm = TRUE)

  # Handle edge case where SS_tot is 0 (constant y)
  if (ss_tot == 0) {
    return(if (ss_res == 0) 1 else -Inf)
  }

  1 - ss_res / ss_tot
}


# =============================================================================
# Uncertainty Metrics (require surrogate and evaluation grid)
# =============================================================================

#' @title Mean Variance Metric
#'
#' @description
#' Returns the average prediction variance of the surrogate over an
#' evaluation grid. Measures overall model uncertainty.
#'
#' @param archive ([ArchiveBatch]) The archive (may be used for normalization).
#' @param surrogate ([SurrogateModel] | [mlr3mbo::Surrogate]) The fitted surrogate model.
#' @param grid (`data.table`) Points at which to evaluate variance. If NULL,
#'   generates a default grid from the search space.
#' @param search_space ([paradox::ParamSet]) Used to generate grid if not provided.
#' @param ... Ignored.
#'
#' @return `numeric(1)` The mean of se^2 over the grid.
#'
#' @export
metric_mean_variance <- function(archive, surrogate = NULL, grid = NULL,
    search_space = NULL, ...) {
  if (is.null(surrogate)) {
    stopf("metric_mean_variance requires a surrogate model")
  }
  # Check if fitted (SurrogateModel has $is_fitted, mlr3mbo::Surrogate doesn't)
  if (!is.null(surrogate$is_fitted) && !surrogate$is_fitted) {
    stopf("metric_mean_variance requires a fitted surrogate model")
  }

  # Generate grid if not provided
  if (is.null(grid)) {
    if (is.null(search_space)) {
      stopf("metric_mean_variance requires either 'grid' or 'search_space'")
    }
    grid <- generate_default_grid(search_space)
  }

  pred <- surrogate$predict(grid)
  mean(pred$se^2, na.rm = TRUE)
}


#' @title Maximum Variance Metric
#'
#' @description
#' Returns the maximum prediction variance of the surrogate over an
#' evaluation grid. Identifies the point of highest uncertainty.
#'
#' @inheritParams metric_mean_variance
#'
#' @return `numeric(1)` The maximum se^2 over the grid.
#'
#' @export
metric_max_variance <- function(archive, surrogate = NULL, grid = NULL,
    search_space = NULL, ...) {
  if (is.null(surrogate)) {
    stopf("metric_max_variance requires a surrogate model")
  }
  # Check if fitted (SurrogateModel has $is_fitted, mlr3mbo::Surrogate doesn't)
  if (!is.null(surrogate$is_fitted) && !surrogate$is_fitted) {
    stopf("metric_max_variance requires a fitted surrogate model")
  }

  # Generate grid if not provided
  if (is.null(grid)) {
    if (is.null(search_space)) {
      stopf("metric_max_variance requires either 'grid' or 'search_space'")
    }
    grid <- generate_default_grid(search_space)
  }

  pred <- surrogate$predict(grid)
  max(pred$se^2, na.rm = TRUE)
}


#' @title Integrated Variance Metric
#'
#' @description
#' Approximates the integral of prediction variance over the domain.
#' This is the continuous version of mean variance, accounting for
#' domain volume.
#'
#' @inheritParams metric_mean_variance
#'
#' @return `numeric(1)` The integrated variance.
#'
#' @details
#' For a uniform grid, this equals mean_variance * domain_volume.
#' More sophisticated integration (e.g., Monte Carlo) may be added.
#'
#' @export
metric_integrated_variance <- function(archive, surrogate = NULL, grid = NULL,
    search_space = NULL, ...) {
  if (is.null(surrogate)) {
    stopf("metric_integrated_variance requires a surrogate model")
  }
  # Check if fitted (SurrogateModel has $is_fitted, mlr3mbo::Surrogate doesn't)
  if (!is.null(surrogate$is_fitted) && !surrogate$is_fitted) {
    stopf("metric_integrated_variance requires a fitted surrogate model")
  }

  # Need search_space for domain volume calculation
  if (is.null(search_space)) {
    stopf("metric_integrated_variance requires 'search_space' for domain volume calculation")
  }

  # Generate grid if not provided
  if (is.null(grid)) {
    grid <- generate_default_grid(search_space)
  }

  mean_var <- metric_mean_variance(archive, surrogate, grid)
  volume <- compute_domain_volume(search_space)

  mean_var * volume
}


# =============================================================================
# Helper Functions
# =============================================================================

#' @title Generate Default Grid
#'
#' @description
#' Generates a default evaluation grid from a search space using Latin
#' Hypercube Sampling.
#'
#' @param search_space ([paradox::ParamSet]) The search space to sample from.
#' @param n (`integer(1)`) Number of grid points. Default 100.
#'
#' @return `data.table` with one row per grid point.
#'
#' @keywords internal
generate_default_grid <- function(search_space, n = 100L) {
  design <- generate_design_lhs(search_space, n = n)
  design$data
}


#' @title Compute Domain Volume
#'
#' @description
#' Computes the volume of the search space (product of ranges for numeric
#' parameters, product of level counts for categorical).
#'
#' @param search_space ([paradox::ParamSet]) The search space.
#'
#' @return `numeric(1)` The domain volume.
#'
#' @details
#' For mixed numeric/categorical spaces, this returns the product of:
#' - (upper - lower) for each numeric parameter
#' - Number of levels for each categorical parameter
#'
#' @keywords internal
compute_domain_volume <- function(search_space) {
  volume <- 1
  for (pid in search_space$ids()) {
    param_class <- search_space$class[[pid]]
    if (param_class %in% c("ParamDbl", "ParamInt")) {
      lower <- search_space$lower[[pid]]
      upper <- search_space$upper[[pid]]
      if (is.finite(lower) && is.finite(upper)) {
        volume <- volume * (upper - lower)
      } else {
        stopf("Cannot compute domain volume: parameter '%s' has infinite bounds", pid)
      }
    } else if (param_class == "ParamFct") {
      volume <- volume * search_space$nlevels[[pid]]
    } else if (param_class == "ParamLgl") {
      volume <- volume * 2  # TRUE/FALSE
    }
    # ParamUty: ignore, assume volume contribution of 1
  }
  volume
}


#' @title Make Metric Function
#'
#' @description
#' Creates a metric function with fixed parameters, useful for configuring
#' metrics that require specific settings.
#'
#' @param metric_fun (`function`) Base metric function.
#' @param ... Fixed arguments to pass to the metric function.
#'
#' @return A new metric function with the fixed arguments bound.
#'
#' @examples
#' \dontrun{
#' # Create RMSE metric with fixed test data
#' my_rmse <- make_metric(metric_model_rmse, test_data = my_test_dt, target = "y")
#'
#' # Use in tracker
#' tracker <- MetricsTracker$new(metrics = list(rmse = my_rmse))
#' }
#'
#' @export
make_metric <- function(metric_fun, ...) {
  fixed_args <- list(...)
  function(archive, surrogate = NULL, ...) {
    # Merge fixed_args with dynamic args (dynamic overrides fixed)
    dynamic_args <- list(...)
    # Remove fixed args that are also in dynamic args (dynamic overrides)
    effective_fixed <- fixed_args[setdiff(names(fixed_args), names(dynamic_args))]
    all_args <- c(list(archive = archive, surrogate = surrogate), effective_fixed, dynamic_args)
    do.call(metric_fun, all_args)
  }
}
