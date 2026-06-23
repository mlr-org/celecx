make_predicted_task <- function() {
  per_batch <- 4L
  batches <- rep(1:5, each = per_batch)
  truth <- rep(c(0.1, 0.3, 0.5, 0.6, 0.65), each = per_batch)
  response <- rep(c(0.0, 0.4, 0.4, 0.5, 0.7), each = per_batch)
  dt <- data.table(
    x1 = rnorm(length(batches)),
    x2 = rnorm(length(batches)),
    y = rnorm(length(batches)),
    batch_nr = as.integer(batches),
    perf = truth
  )
  task <- TaskLCE$new("ms", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  pred <- PredictionLCE$new(task = task, response = response)
  list(task = task, pred = pred)
}

test_that("MeasureLCEMAE aggregates per batch", {
  set.seed(11)
  setup <- make_predicted_task()
  # per-batch errors are |0.1-0|, |0.3-0.4|, |0.5-0.4|, |0.6-0.5|, |0.65-0.7|
  # = 0.1, 0.1, 0.1, 0.1, 0.05  -> mean 0.09
  expect_equal(msr("lce.mae")$score(setup$pred, task = setup$task), 0.09, tolerance = 1e-8)
})

test_that("MeasureLCERMSE matches sqrt(MSE)", {
  set.seed(12)
  setup <- make_predicted_task()
  mse <- msr("lce.mse")$score(setup$pred, task = setup$task)
  rmse <- msr("lce.rmse")$score(setup$pred, task = setup$task)
  expect_equal(rmse, sqrt(mse), tolerance = 1e-12)
})

test_that("MeasureLCE requires the task", {
  measure <- msr("lce.mae")
  expect_true("requires_task" %in% measure$properties)
})

test_that("Per-batch deduplication collapses redundancy", {
  set.seed(13)
  # Build a task where one batch has many more rows than others.
  dt <- rbind(
    data.table(x1 = rnorm(2), x2 = rnorm(2), y = rnorm(2),
      batch_nr = 1L, perf = 0.2),
    data.table(x1 = rnorm(10), x2 = rnorm(10), y = rnorm(10),
      batch_nr = 2L, perf = 0.4)
  )
  task <- TaskLCE$new("dup", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  response <- rep(c(0.3, 0.3), c(2L, 10L))
  pred <- PredictionLCE$new(task = task, response = response)
  # per-batch errors are |0.2-0.3|=0.1 and |0.4-0.3|=0.1 -> mean 0.1
  expect_equal(msr("lce.mae")$score(pred, task = task), 0.1, tolerance = 1e-12)
})
