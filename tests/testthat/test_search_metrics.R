
# Tests for search_metrics.R

# =============================================================================
# Test Setup
# =============================================================================

# Helper to create a mock archive with data
create_test_archive <- function(x_vals, y_vals) {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))
  archive <- ArchiveBatch$new(search_space, codomain)

  xdt <- data.table(x = x_vals)
  ydt <- data.table(y = y_vals)
  archive$add_evals(xdt, NULL, ydt)
  archive
}

# Helper to create a mock archive with multiple targets
create_test_archive_multi <- function(x_vals, y1_vals, y2_vals) {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "maximize")
  )
  archive <- ArchiveBatch$new(search_space, codomain)

  xdt <- data.table(x = x_vals)
  ydt <- data.table(y1 = y1_vals, y2 = y2_vals)
  archive$add_evals(xdt, NULL, ydt)
  archive
}

# Helper to create a mock surrogate model
MockSurrogate <- R6Class("MockSurrogate",
  public = list(
    is_fitted = FALSE,
    predictions = NULL,

    initialize = function(predictions = NULL) {
      self$predictions <- predictions
      self$is_fitted <- !is.null(predictions)
    },

    predict = function(newdata) {
      if (!self$is_fitted) {
        stop("Model not fitted")
      }
      # Return fixed predictions or generate based on newdata
      if (!is.null(self$predictions)) {
        self$predictions
      } else {
        n <- nrow(newdata)
        data.table(mean = rep(1, n), se = rep(0.1, n))
      }
    }
  )
)

# =============================================================================
# metric_best_y tests
# =============================================================================

test_that("metric_best_y returns minimum value", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  expect_equal(metric_best_y(archive), 2)
})

test_that("metric_best_y works with negative values", {
  archive <- create_test_archive(c(1, 2, 3), c(-5, -2, -8))
  expect_equal(metric_best_y(archive), -8)
})

test_that("metric_best_y uses specified target", {
  archive <- create_test_archive_multi(c(1, 2), c(5, 2), c(10, 20))
  expect_equal(metric_best_y(archive, target = "y1"), 2)
  expect_equal(metric_best_y(archive, target = "y2"), 10)
})

test_that("metric_best_y defaults to first target", {
  archive <- create_test_archive_multi(c(1, 2), c(5, 2), c(10, 20))
  expect_equal(metric_best_y(archive), 2)  # y1 is first
})

test_that("metric_best_y handles single value", {
  archive <- create_test_archive(1, 5)
  expect_equal(metric_best_y(archive), 5)
})

# =============================================================================
# metric_worst_y tests
# =============================================================================

test_that("metric_worst_y returns maximum value", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  expect_equal(metric_worst_y(archive), 8)
})

test_that("metric_worst_y works with negative values", {
  archive <- create_test_archive(c(1, 2, 3), c(-5, -2, -8))
  expect_equal(metric_worst_y(archive), -2)
})

test_that("metric_worst_y uses specified target", {
  archive <- create_test_archive_multi(c(1, 2), c(5, 2), c(10, 20))
  expect_equal(metric_worst_y(archive, target = "y1"), 5)
  expect_equal(metric_worst_y(archive, target = "y2"), 20)
})

# =============================================================================
# metric_regret tests
# =============================================================================

test_that("metric_regret computes gap correctly", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  expect_equal(metric_regret(archive, optimum = 0), 2)  # best is 2, gap is 2-0
  expect_equal(metric_regret(archive, optimum = 1), 1)  # best is 2, gap is 2-1
})

test_that("metric_regret can be negative", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  expect_equal(metric_regret(archive, optimum = 3), -1)  # best is 2, gap is 2-3
})

test_that("metric_regret requires optimum argument", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  expect_error(metric_regret(archive), "optimum")
})

test_that("metric_regret validates optimum is numeric", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  expect_error(metric_regret(archive, optimum = "bad"))
})

test_that("metric_simple_regret is alias for metric_regret", {
  expect_identical(metric_simple_regret, metric_regret)
})

# =============================================================================
# metric_model_rmse tests
# =============================================================================

test_that("metric_model_rmse computes RMSE correctly", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4))
  # Mock predictions: mean = c(1.5, 2.5, 2.5, 3.5)
  pred <- data.table(mean = c(1.5, 2.5, 2.5, 3.5), se = rep(0.1, 4))
  surrogate <- MockSurrogate$new(predictions = pred)

  # RMSE = sqrt(mean((1-1.5)^2 + (2-2.5)^2 + (3-2.5)^2 + (4-3.5)^2))
  # = sqrt(mean(0.25 + 0.25 + 0.25 + 0.25)) = sqrt(0.25) = 0.5
  result <- metric_model_rmse(archive, surrogate, test_data, "y")
  expect_equal(result, 0.5)
})

test_that("metric_model_rmse requires surrogate", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2), y = c(1, 2))
  expect_error(metric_model_rmse(archive, surrogate = NULL, test_data, "y"),
    "requires a surrogate")
})

test_that("metric_model_rmse requires fitted surrogate", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2), y = c(1, 2))
  surrogate <- MockSurrogate$new()  # not fitted
  expect_error(metric_model_rmse(archive, surrogate, test_data, "y"),
    "requires a fitted")
})

test_that("metric_model_rmse validates test_data has target", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2))  # no y column
  pred <- data.table(mean = c(1, 2), se = c(0.1, 0.1))
  surrogate <- MockSurrogate$new(predictions = pred)
  expect_error(metric_model_rmse(archive, surrogate, test_data, "y"),
    "not found")
})

# =============================================================================
# metric_model_mae tests
# =============================================================================

test_that("metric_model_mae computes MAE correctly", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4))
  # Mock predictions: mean = c(2, 3, 4, 5)
  pred <- data.table(mean = c(2, 3, 4, 5), se = rep(0.1, 4))
  surrogate <- MockSurrogate$new(predictions = pred)

  # MAE = mean(|1-2| + |2-3| + |3-4| + |4-5|) = mean(1, 1, 1, 1) = 1
  result <- metric_model_mae(archive, surrogate, test_data, "y")
  expect_equal(result, 1)
})

test_that("metric_model_mae requires surrogate", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2), y = c(1, 2))
  expect_error(metric_model_mae(archive, surrogate = NULL, test_data, "y"),
    "requires a surrogate")
})

# =============================================================================
# metric_model_r2 tests
# =============================================================================

test_that("metric_model_r2 computes R-squared correctly", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4))
  # Perfect predictions
  pred <- data.table(mean = c(1, 2, 3, 4), se = rep(0.1, 4))
  surrogate <- MockSurrogate$new(predictions = pred)

  result <- metric_model_r2(archive, surrogate, test_data, "y")
  expect_equal(result, 1)  # Perfect fit
})

test_that("metric_model_r2 returns 0 for mean predictions", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4))
  # Predict mean of y = 2.5 for all
  pred <- data.table(mean = rep(2.5, 4), se = rep(0.1, 4))
  surrogate <- MockSurrogate$new(predictions = pred)

  result <- metric_model_r2(archive, surrogate, test_data, "y")
  expect_equal(result, 0)  # Same as predicting mean
})

test_that("metric_model_r2 can be negative for poor predictions", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4))
  # Very bad predictions
  pred <- data.table(mean = c(10, 10, 10, 10), se = rep(0.1, 4))
  surrogate <- MockSurrogate$new(predictions = pred)

  result <- metric_model_r2(archive, surrogate, test_data, "y")
  expect_lt(result, 0)  # Worse than mean
})

test_that("metric_model_r2 requires surrogate", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  test_data <- data.table(x = c(1, 2), y = c(1, 2))
  expect_error(metric_model_r2(archive, surrogate = NULL, test_data, "y"),
    "requires a surrogate")
})

# =============================================================================
# metric_mean_variance tests
# =============================================================================

test_that("metric_mean_variance computes mean of se^2", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  grid <- data.table(x = c(1, 2, 3, 4))
  # Mock predictions with varying se
  pred <- data.table(mean = c(1, 2, 3, 4), se = c(0.1, 0.2, 0.3, 0.4))
  surrogate <- MockSurrogate$new(predictions = pred)

  # Mean variance = mean(0.01 + 0.04 + 0.09 + 0.16) = mean(0.3) = 0.075
  result <- metric_mean_variance(archive, surrogate, grid)
  expect_equal(result, 0.075)
})

test_that("metric_mean_variance requires surrogate", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  grid <- data.table(x = c(1, 2))
  expect_error(metric_mean_variance(archive, surrogate = NULL, grid),
    "requires a surrogate")
})

test_that("metric_mean_variance requires grid or search_space", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  pred <- data.table(mean = c(1, 2), se = c(0.1, 0.2))
  surrogate <- MockSurrogate$new(predictions = pred)
  expect_error(metric_mean_variance(archive, surrogate),
    "requires either")
})

# =============================================================================
# metric_max_variance tests
# =============================================================================

test_that("metric_max_variance returns maximum se^2", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  grid <- data.table(x = c(1, 2, 3, 4))
  pred <- data.table(mean = c(1, 2, 3, 4), se = c(0.1, 0.2, 0.3, 0.5))
  surrogate <- MockSurrogate$new(predictions = pred)

  # Max variance = 0.5^2 = 0.25
  result <- metric_max_variance(archive, surrogate, grid)
  expect_equal(result, 0.25)
})

test_that("metric_max_variance requires surrogate", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  grid <- data.table(x = c(1, 2))
  expect_error(metric_max_variance(archive, surrogate = NULL, grid),
    "requires a surrogate")
})

# =============================================================================
# metric_integrated_variance tests
# =============================================================================

test_that("metric_integrated_variance multiplies by domain volume", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  grid <- data.table(x = c(1, 2, 3, 4))
  pred <- data.table(mean = c(1, 2, 3, 4), se = c(0.2, 0.2, 0.2, 0.2))
  surrogate <- MockSurrogate$new(predictions = pred)

  # Mean variance = 0.04, domain volume = 10
  result <- metric_integrated_variance(archive, surrogate, grid, search_space)
  expect_equal(result, 0.04 * 10)
})

test_that("metric_integrated_variance requires search_space", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  grid <- data.table(x = c(1, 2))
  pred <- data.table(mean = c(1, 2), se = c(0.1, 0.2))
  surrogate <- MockSurrogate$new(predictions = pred)
  expect_error(metric_integrated_variance(archive, surrogate, grid),
    "requires 'search_space'")
})

# =============================================================================
# compute_domain_volume tests
# =============================================================================

test_that("compute_domain_volume works for single numeric parameter", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  expect_equal(compute_domain_volume(search_space), 10)
})

test_that("compute_domain_volume works for multiple numeric parameters", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    y = p_dbl(lower = -5, upper = 5)
  )
  expect_equal(compute_domain_volume(search_space), 10 * 10)
})

test_that("compute_domain_volume works for integer parameters", {
  search_space <- ps(x = p_int(lower = 0, upper = 10))
  expect_equal(compute_domain_volume(search_space), 10)
})

test_that("compute_domain_volume works for categorical parameters", {
  search_space <- ps(x = p_fct(levels = c("a", "b", "c")))
  expect_equal(compute_domain_volume(search_space), 3)
})

test_that("compute_domain_volume works for logical parameters", {
  search_space <- ps(x = p_lgl())
  expect_equal(compute_domain_volume(search_space), 2)
})

test_that("compute_domain_volume works for mixed parameter types", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 5),
    y = p_fct(levels = c("a", "b")),
    z = p_lgl()
  )
  expect_equal(compute_domain_volume(search_space), 5 * 2 * 2)
})

test_that("compute_domain_volume errors on infinite bounds", {
  search_space <- ps(x = p_dbl())  # default bounds are -Inf, Inf
  expect_error(compute_domain_volume(search_space), "infinite bounds")
})

# =============================================================================
# generate_default_grid tests
# =============================================================================

test_that("generate_default_grid returns data.table", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  grid <- generate_default_grid(search_space, n = 10)
  expect_data_table(grid)
  expect_equal(nrow(grid), 10)
})

test_that("generate_default_grid has correct columns", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 1),
    y = p_dbl(lower = 0, upper = 1)
  )
  grid <- generate_default_grid(search_space, n = 20)
  expect_names(names(grid), identical.to = c("x", "y"))
})

test_that("generate_default_grid values are within bounds", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  grid <- generate_default_grid(search_space, n = 50)
  expect_true(all(grid$x >= 0))
  expect_true(all(grid$x <= 1))
})

# =============================================================================
# make_metric tests
# =============================================================================

test_that("make_metric binds fixed arguments", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  my_regret <- make_metric(metric_regret, optimum = 0)
  expect_equal(my_regret(archive), 2)  # best is 2, optimum is 0
})

test_that("make_metric allows dynamic override", {
  archive <- create_test_archive(c(1, 2, 3), c(5, 2, 8))
  my_regret <- make_metric(metric_regret, optimum = 0)
  # Dynamic arg should override fixed
  expect_equal(my_regret(archive, optimum = 1), 1)  # best is 2, optimum is 1
})

test_that("make_metric works with target argument", {
  archive <- create_test_archive_multi(c(1, 2), c(5, 2), c(10, 20))
  my_best_y1 <- make_metric(metric_best_y, target = "y1")
  my_best_y2 <- make_metric(metric_best_y, target = "y2")
  expect_equal(my_best_y1(archive), 2)
  expect_equal(my_best_y2(archive), 10)
})
