# =============================================================================
# Tests for ForecastTracker
# =============================================================================

create_metrics_history <- function(n = 12L) {
  data.table(
    batch_nr = seq_len(n),
    n_evals = seq_len(n),
    best_y = 1 / sqrt(seq_len(n))
  )
}


test_that("ForecastTracker updates according to min_points and update_every", {
  metrics <- create_metrics_history(12L)

  tracker <- ForecastTracker$new(
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 40L),
    metric_name = "best_y",
    target = 0.2,
    min_points = 4L,
    update_every = 2L,
    default_horizon = 5L
  )

  expect_null(tracker$update(metrics[1:3]))
  expect_equal(tracker$n_updates, 0L)

  tracker$update(metrics[1:4])
  expect_equal(tracker$n_updates, 1L)

  expect_null(tracker$update(metrics[1:5]))
  expect_equal(tracker$n_updates, 1L)

  tracker$update(metrics[1:6])
  expect_equal(tracker$n_updates, 2L)

  latest <- tracker$latest
  expect_data_table(latest, nrows = 1)
  expect_true("prediction" %in% names(latest))
  expect_true(is.list(latest$prediction))
})


test_that("ForecastTracker clear resets data", {
  metrics <- create_metrics_history(8L)

  tracker <- ForecastTracker$new(
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    min_points = 3L
  )

  tracker$update(metrics[1:4])
  expect_gt(tracker$n_updates, 0L)

  tracker$clear()
  expect_equal(tracker$n_updates, 0L)
  expect_data_table(tracker$data, nrows = 0)
})
