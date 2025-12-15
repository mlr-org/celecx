
# Tests for MetricsTracker.R

# =============================================================================
# Helper functions
# =============================================================================

# Create a simple archive for testing
create_test_archive <- function(n_evals = 10, n_batch = 2) {
  search_space <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = "minimize"))

  # ArchiveBatch expects ParamSet for codomain, not Codomain object
  archive <- ArchiveBatch$new(
    search_space = search_space,
    codomain = codomain
  )

  # Add some data
  evals_per_batch <- n_evals %/% n_batch
  for (b in seq_len(n_batch)) {
    xdt <- data.table(
      x1 = runif(evals_per_batch),
      x2 = runif(evals_per_batch)
    )
    ydt <- data.table(y = runif(evals_per_batch, 0, 1))
    archive$add_evals(xdt, xss_trafoed = NULL, ydt)
  }

  archive
}

# Simple metric function for testing
simple_metric <- function(archive, surrogate = NULL, ...) {
  archive$n_evals
}

# Metric that uses extra data
metric_with_extra <- function(archive, surrogate = NULL, multiplier = 1, ...) {
  archive$n_evals * multiplier
}

# Metric that can fail
failing_metric <- function(archive, surrogate = NULL, ...) {
  stop("Intentional error")
}

# Metric that returns non-scalar
non_scalar_metric <- function(archive, surrogate = NULL, ...) {
  c(1, 2, 3)
}


# =============================================================================
# Initialization Tests
# =============================================================================

test_that("MetricsTracker initializes with empty metrics", {
  tracker <- MetricsTracker$new()

  expect_r6(tracker, "MetricsTracker")
  expect_equal(tracker$n_batches, 0)
  expect_equal(length(tracker$metric_names), 0)
  expect_data_table(tracker$data, nrows = 0)
})

test_that("MetricsTracker initializes with metrics", {
  tracker <- MetricsTracker$new(
    metrics = list(my_metric = simple_metric)
  )

  expect_equal(tracker$metric_names, "my_metric")
  expect_true("my_metric" %in% names(tracker$metrics))
  expect_true(is.function(tracker$metrics$my_metric))
})

test_that("MetricsTracker initializes with extra_data", {
  tracker <- MetricsTracker$new(
    metrics = list(scaled = metric_with_extra),
    extra_data = list(multiplier = 2)
  )

  expect_equal(tracker$extra_data$multiplier, 2)
})

test_that("MetricsTracker data table has correct columns", {
  tracker <- MetricsTracker$new(
    metrics = list(metric1 = simple_metric, metric2 = simple_metric)
  )

  data <- tracker$data
  expect_true("batch_nr" %in% names(data))
  expect_true("n_evals" %in% names(data))
  expect_true("timestamp" %in% names(data))
  expect_true("metric1" %in% names(data))
  expect_true("metric2" %in% names(data))
})

test_that("MetricsTracker rejects non-function metrics", {
  expect_error(
    MetricsTracker$new(metrics = list(bad = "not_a_function")),
    "must be a function"
  )
})

test_that("MetricsTracker rejects reserved column names", {
  expect_error(
    MetricsTracker$new(metrics = list(batch_nr = simple_metric)),
    "reserved"
  )
  expect_error(
    MetricsTracker$new(metrics = list(n_evals = simple_metric)),
    "reserved"
  )
  expect_error(
    MetricsTracker$new(metrics = list(timestamp = simple_metric)),
    "reserved"
  )
})

test_that("MetricsTracker rejects unnamed extra_data", {
  expect_error(
    MetricsTracker$new(extra_data = list(1, 2, 3)),
    "named list"
  )
})


# =============================================================================
# log_batch Tests
# =============================================================================

test_that("MetricsTracker log_batch adds row to data", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()

  tracker$log_batch(1L, archive)

  expect_equal(tracker$n_batches, 1)
  expect_equal(tracker$data$batch_nr, 1L)
  expect_equal(tracker$data$n_evals, archive$n_evals)
})

test_that("MetricsTracker log_batch computes metrics", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive(n_evals = 20)

  tracker$log_batch(1L, archive)

  expect_equal(tracker$data$count, 20)
})

test_that("MetricsTracker log_batch uses extra_data", {
  tracker <- MetricsTracker$new(
    metrics = list(scaled = metric_with_extra),
    extra_data = list(multiplier = 3)
  )
  archive <- create_test_archive(n_evals = 10)

  tracker$log_batch(1L, archive)

  expect_equal(tracker$data$scaled, 30)  # 10 * 3
})

test_that("MetricsTracker log_batch allows dynamic args to override extra_data", {
  tracker <- MetricsTracker$new(
    metrics = list(scaled = metric_with_extra),
    extra_data = list(multiplier = 3)
  )
  archive <- create_test_archive(n_evals = 10)

  tracker$log_batch(1L, archive, multiplier = 5)

  expect_equal(tracker$data$scaled, 50)  # 10 * 5
})

test_that("MetricsTracker log_batch handles multiple batches", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive(n_evals = 10, n_batch = 1)

  tracker$log_batch(1L, archive)

  # Add more data to archive
  xdt <- data.table(x1 = runif(5), x2 = runif(5))
  ydt <- data.table(y = runif(5))
  archive$add_evals(xdt, xss_trafoed = NULL, ydt)

  tracker$log_batch(2L, archive)

  expect_equal(tracker$n_batches, 2)
  expect_equal(tracker$data$batch_nr, c(1L, 2L))
  expect_equal(tracker$data$count, c(10, 15))
})

test_that("MetricsTracker log_batch records timestamp", {
  tracker <- MetricsTracker$new()
  archive <- create_test_archive()

  before <- Sys.time()
  tracker$log_batch(1L, archive)
  after <- Sys.time()

  expect_true(tracker$data$timestamp >= before)
  expect_true(tracker$data$timestamp <= after)
})

test_that("MetricsTracker log_batch returns new row invisibly", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()

  result <- withVisible(tracker$log_batch(1L, archive))

  expect_false(result$visible)
  expect_data_table(result$value, nrows = 1)
  expect_true("count" %in% names(result$value))
})

test_that("MetricsTracker log_batch handles metric errors gracefully", {
  tracker <- MetricsTracker$new(
    metrics = list(
      good = simple_metric,
      bad = failing_metric
    )
  )
  archive <- create_test_archive()

  expect_warning(tracker$log_batch(1L, archive), "Error computing metric")

  expect_equal(tracker$n_batches, 1)
  expect_equal(tracker$data$good, archive$n_evals)
  expect_true(is.na(tracker$data$bad))
})

test_that("MetricsTracker log_batch handles non-scalar metrics gracefully", {
  tracker <- MetricsTracker$new(
    metrics = list(
      good = simple_metric,
      bad = non_scalar_metric
    )
  )
  archive <- create_test_archive()

  expect_warning(tracker$log_batch(1L, archive), "non-scalar")

  expect_true(is.na(tracker$data$bad))
})


# =============================================================================
# add_metric Tests
# =============================================================================

test_that("MetricsTracker add_metric adds new metric", {
  tracker <- MetricsTracker$new()
  tracker$add_metric("new_metric", simple_metric)

  expect_true("new_metric" %in% tracker$metric_names)
  expect_true(is.function(tracker$metrics$new_metric))
})

test_that("MetricsTracker add_metric rejects reserved names", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$add_metric("batch_nr", simple_metric), "reserved")
})

test_that("MetricsTracker add_metric rejects duplicates", {
  tracker <- MetricsTracker$new(metrics = list(existing = simple_metric))

  expect_error(tracker$add_metric("existing", simple_metric), "already exists")
})

test_that("MetricsTracker add_metric fills NA for existing rows", {
  tracker <- MetricsTracker$new(metrics = list(metric1 = simple_metric))
  archive <- create_test_archive()

  tracker$log_batch(1L, archive)
  tracker$add_metric("metric2", simple_metric)

  expect_true("metric2" %in% names(tracker$data))
  expect_true(is.na(tracker$data$metric2))
})

test_that("MetricsTracker add_metric warns on backfill request", {
  tracker <- MetricsTracker$new(metrics = list(metric1 = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  expect_warning(
    tracker$add_metric("metric2", simple_metric, backfill = TRUE),
    "archive snapshots"
  )
})

test_that("MetricsTracker add_metric returns self invisibly", {
  tracker <- MetricsTracker$new()
  result <- withVisible(tracker$add_metric("new", simple_metric))

  expect_false(result$visible)
  expect_identical(result$value, tracker)
})


# =============================================================================
# remove_metric Tests
# =============================================================================

test_that("MetricsTracker remove_metric removes metric", {
  tracker <- MetricsTracker$new(metrics = list(metric1 = simple_metric))
  tracker$remove_metric("metric1")

  expect_false("metric1" %in% tracker$metric_names)
})

test_that("MetricsTracker remove_metric removes data column by default", {
  tracker <- MetricsTracker$new(metrics = list(metric1 = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  tracker$remove_metric("metric1")

  expect_false("metric1" %in% names(tracker$data))
})

test_that("MetricsTracker remove_metric keeps data column when requested", {
  tracker <- MetricsTracker$new(metrics = list(metric1 = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  tracker$remove_metric("metric1", keep_data = TRUE)

  expect_false("metric1" %in% tracker$metric_names)
  expect_true("metric1" %in% names(tracker$data))
})

test_that("MetricsTracker remove_metric errors on unknown metric", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$remove_metric("nonexistent"), "not found")
})


# =============================================================================
# set_extra_data Tests
# =============================================================================

test_that("MetricsTracker set_extra_data adds new data", {
  tracker <- MetricsTracker$new()
  tracker$set_extra_data(key1 = "value1", key2 = 42)

  expect_equal(tracker$extra_data$key1, "value1")
  expect_equal(tracker$extra_data$key2, 42)
})

test_that("MetricsTracker set_extra_data updates existing data", {
  tracker <- MetricsTracker$new(extra_data = list(key1 = "old"))
  tracker$set_extra_data(key1 = "new")

  expect_equal(tracker$extra_data$key1, "new")
})

test_that("MetricsTracker set_extra_data returns self invisibly", {
  tracker <- MetricsTracker$new()
  result <- withVisible(tracker$set_extra_data(key = "value"))

  expect_false(result$visible)
  expect_identical(result$value, tracker)
})

test_that("MetricsTracker set_extra_data rejects unnamed args", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$set_extra_data(123), "must be named")
})


# =============================================================================
# clear Tests
# =============================================================================

test_that("MetricsTracker clear removes all history", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)
  tracker$log_batch(2L, archive)

  expect_equal(tracker$n_batches, 2)

  tracker$clear()

  expect_equal(tracker$n_batches, 0)
  expect_data_table(tracker$data, nrows = 0)
})

test_that("MetricsTracker clear keeps metrics", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  tracker$clear()

  expect_equal(tracker$metric_names, "count")
  expect_true("count" %in% names(tracker$data))
})

test_that("MetricsTracker clear returns self invisibly", {
  tracker <- MetricsTracker$new()
  result <- withVisible(tracker$clear())

  expect_false(result$visible)
  expect_identical(result$value, tracker)
})


# =============================================================================
# get_batch Tests
# =============================================================================

test_that("MetricsTracker get_batch returns correct row", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)
  tracker$log_batch(2L, archive)

  row <- tracker$get_batch(1L)

  expect_data_table(row, nrows = 1)
  expect_equal(row$batch_nr, 1L)
})

test_that("MetricsTracker get_batch returns NULL for missing batch", {
  tracker <- MetricsTracker$new()

  expect_null(tracker$get_batch(99L))
})


# =============================================================================
# get_metric_history Tests
# =============================================================================

test_that("MetricsTracker get_metric_history returns vector", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive(n_evals = 10, n_batch = 1)
  tracker$log_batch(1L, archive)

  xdt <- data.table(x1 = runif(5), x2 = runif(5))
  ydt <- data.table(y = runif(5))
  archive$add_evals(xdt, xss_trafoed = NULL, ydt)
  tracker$log_batch(2L, archive)

  history <- tracker$get_metric_history("count")

  expect_numeric(history, len = 2)
  expect_equal(history, c(10, 15))
})

test_that("MetricsTracker get_metric_history errors on unknown metric", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$get_metric_history("unknown"), "not found")
})


# =============================================================================
# Active Bindings Tests
# =============================================================================

test_that("MetricsTracker data is read-only", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$data <- data.table(), "read-only")
})

test_that("MetricsTracker metrics is read-only", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$metrics <- list(), "read-only")
})

test_that("MetricsTracker extra_data is read-only", {
  tracker <- MetricsTracker$new()

  expect_error(tracker$extra_data <- list(), "read-only")
})

test_that("MetricsTracker latest returns last row", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)
  tracker$log_batch(2L, archive)

  latest <- tracker$latest

  expect_data_table(latest, nrows = 1)
  expect_equal(latest$batch_nr, 2L)
})

test_that("MetricsTracker latest returns NULL when empty", {
  tracker <- MetricsTracker$new()

  expect_null(tracker$latest)
})


# =============================================================================
# Print Tests
# =============================================================================

test_that("MetricsTracker print shows basic info", {
  tracker <- MetricsTracker$new()
  output <- capture.output(tracker$print())

  expect_true(any(grepl("MetricsTracker", output)))
  expect_true(any(grepl("Batches logged: 0", output)))
})

test_that("MetricsTracker print shows metrics", {
  tracker <- MetricsTracker$new(metrics = list(
    metric1 = simple_metric,
    metric2 = simple_metric
  ))
  output <- capture.output(tracker$print())

  expect_true(any(grepl("Metrics tracked", output)))
  expect_true(any(grepl("metric1", output)))
})

test_that("MetricsTracker print shows extra_data keys", {
  tracker <- MetricsTracker$new(extra_data = list(key1 = 1, key2 = 2))
  output <- capture.output(tracker$print())

  expect_true(any(grepl("Extra data keys", output)))
})

test_that("MetricsTracker print shows latest batch when available", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  output <- capture.output(tracker$print())

  expect_true(any(grepl("Latest batch", output)))
})


# =============================================================================
# Clone Tests
# =============================================================================

test_that("MetricsTracker clone creates independent copy", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  clone <- tracker$clone(deep = TRUE)
  tracker$log_batch(2L, archive)

  expect_equal(tracker$n_batches, 2)
  expect_equal(clone$n_batches, 1)
})

test_that("MetricsTracker clone deep copies data", {
  tracker <- MetricsTracker$new(metrics = list(count = simple_metric))
  archive <- create_test_archive()
  tracker$log_batch(1L, archive)

  clone <- tracker$clone(deep = TRUE)

  # Clone should have same initial content
  expect_equal(tracker$n_batches, clone$n_batches)

  # Modify original by adding another batch
  tracker$log_batch(2L, archive)

  # Clone should be unaffected

  expect_equal(tracker$n_batches, 2L)
  expect_equal(clone$n_batches, 1L)
})


# =============================================================================
# metrics_tracker convenience constructor Tests
# =============================================================================

test_that("metrics_tracker creates optimization tracker", {
  tracker <- metrics_tracker("optimize")

  expect_r6(tracker, "MetricsTracker")
  expect_true("best_y" %in% tracker$metric_names)
})

test_that("metrics_tracker creates learning tracker", {
  tracker <- metrics_tracker("learn")

  expect_r6(tracker, "MetricsTracker")
  expect_true("mean_variance" %in% tracker$metric_names)
})

test_that("metrics_tracker creates combined tracker", {
  tracker <- metrics_tracker("both")

  expect_r6(tracker, "MetricsTracker")
  expect_true("best_y" %in% tracker$metric_names)
  expect_true("mean_variance" %in% tracker$metric_names)
})

test_that("metrics_tracker adds regret when optimum provided", {
  tracker <- metrics_tracker("optimize", optimum = 0.0)

  expect_true("regret" %in% tracker$metric_names)
  expect_equal(tracker$extra_data$optimum, 0.0)
})

test_that("metrics_tracker adds model_rmse when test_data and target provided", {
  test_data <- data.table(x1 = 1:5, x2 = 1:5, y = 1:5)
  tracker <- metrics_tracker("learn", test_data = test_data, target = "y")

  expect_true("model_rmse" %in% tracker$metric_names)
})

test_that("metrics_tracker passes extra_data", {
  tracker <- metrics_tracker("optimize", custom_param = 42)

  expect_equal(tracker$extra_data$custom_param, 42)
})

test_that("metrics_tracker includes custom metrics", {
  tracker <- metrics_tracker("optimize",
    metrics = list(custom = simple_metric))

  expect_true("custom" %in% tracker$metric_names)
  expect_true("best_y" %in% tracker$metric_names)
})


# =============================================================================
# Integration Tests
# =============================================================================

test_that("MetricsTracker works with real search_metrics", {
  tracker <- MetricsTracker$new(metrics = list(
    best_y = metric_best_y,
    worst_y = metric_worst_y
  ))

  archive <- create_test_archive(n_evals = 10, n_batch = 2)
  tracker$log_batch(1L, archive)

  expect_equal(tracker$n_batches, 1)
  expect_numeric(tracker$data$best_y, lower = 0, upper = 1)
  expect_numeric(tracker$data$worst_y, lower = 0, upper = 1)
  expect_true(tracker$data$best_y <= tracker$data$worst_y)
})

test_that("MetricsTracker works with regret metric", {
  tracker <- MetricsTracker$new(
    metrics = list(
      best_y = metric_best_y,
      regret = metric_regret
    ),
    extra_data = list(optimum = 0.0)
  )

  archive <- create_test_archive(n_evals = 10, n_batch = 2)
  tracker$log_batch(1L, archive)

  expect_equal(tracker$data$regret, tracker$data$best_y - 0.0)
})

test_that("MetricsTracker full workflow", {
  set.seed(42)

  # Create tracker with multiple metrics
  tracker <- MetricsTracker$new(
    metrics = list(
      best_y = metric_best_y,
      n_total = simple_metric
    )
  )

  # Create archive and simulate batches
  archive <- create_test_archive(n_evals = 5, n_batch = 1)

  # Log first batch
  tracker$log_batch(1L, archive)
  expect_equal(tracker$n_batches, 1)

  # Add more data and log second batch
  xdt <- data.table(x1 = runif(5), x2 = runif(5))
  ydt <- data.table(y = runif(5))
  archive$add_evals(xdt, xss_trafoed = NULL, ydt)
  tracker$log_batch(2L, archive)
  expect_equal(tracker$n_batches, 2)

  # Check data integrity
  data <- tracker$data
  expect_equal(nrow(data), 2)
  expect_equal(data$batch_nr, c(1L, 2L))
  expect_equal(data$n_total, c(5, 10))
  expect_true(all(data$best_y >= 0 & data$best_y <= 1))

  # Check history retrieval
  best_history <- tracker$get_metric_history("best_y")
  expect_length(best_history, 2)
})
