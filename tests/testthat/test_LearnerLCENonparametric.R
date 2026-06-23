make_nonparam_task <- function(values, per_batch = 3L, measure = msr("regr.rsq")) {
  n_batches <- length(values)
  batches <- rep(seq_len(n_batches), each = per_batch)
  dt <- data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(batches),
    perf = rep(values, each = per_batch)
  )
  TaskLCE$new("np", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure)
}

test_that("lce.isotonic auto direction recovers a monotone-increasing fit", {
  set.seed(41)
  values <- c(0.0, 0.1, 0.1, 0.4, 0.5)
  task <- make_nonparam_task(values)
  l <- lrn("lce.isotonic")
  l$train(task)
  # isoreg of monotone-increasing values returns the same values.
  expect_equal(l$model$fitted, values, tolerance = 1e-12)
  expect_equal(l$model$direction, "increasing")
})

test_that("lce.isotonic auto direction inverts the constraint for losses", {
  set.seed(42)
  values <- c(0.5, 0.4, 0.3, 0.2, 0.1)
  task <- make_nonparam_task(values, measure = msr("regr.mae"))
  l <- lrn("lce.isotonic")
  l$train(task)
  expect_equal(l$model$fitted, values, tolerance = 1e-12)
  expect_equal(l$model$direction, "decreasing")
})

test_that("lce.isotonic explicit direction works without a measure", {
  set.seed(421)
  values <- c(0.5, 0.4, 0.3, 0.2, 0.1)
  task <- make_nonparam_task(values, measure = NULL)
  l <- lrn("lce.isotonic", direction = "decreasing")
  l$train(task)
  expect_equal(l$model$fitted, values, tolerance = 1e-12)
})

test_that("lce.isotonic auto direction needs a task measure", {
  set.seed(422)
  task <- make_nonparam_task(c(0.1, 0.2, 0.3), measure = NULL)
  expect_error(lrn("lce.isotonic")$train(task), "direction = 'auto'")
})

test_that("lce.isotonic extrapolates as the nearest endpoint", {
  set.seed(43)
  task <- make_nonparam_task(c(0.1, 0.3, 0.5, 0.7, 0.9))
  l <- lrn("lce.isotonic")
  l$train(task)
  pred <- l$predict_newdata(data.table(batch_nr = c(10L, 20L)))
  # Both extrapolated batches are above the training range -> last fit.
  expect_equal(pred$response, c(0.9, 0.9), tolerance = 1e-12)
})

test_that("lce.isotonic constant interpolation produces step output", {
  set.seed(44)
  task <- make_nonparam_task(c(0.0, 0.3, 0.6))
  l <- lrn("lce.isotonic", interpolation = "constant")
  l$train(task)
  pred <- l$predict_newdata(data.table(batch_nr = c(1L, 2L, 3L)))
  expect_equal(pred$response, c(0.0, 0.3, 0.6))
})

test_that("lce.spline_monotone fits and predicts SE", {
  set.seed(45)
  skip_if_not_installed("scam")
  values <- 0.9 - 0.7 * exp(-0.3 * (1:8))
  task <- make_nonparam_task(values)
  l <- lrn("lce.spline_monotone")
  l$predict_type <- "se"
  l$train(task)
  pred <- l$predict(task)
  # Monotone smoother should roughly recover the curve at the training batches.
  expect_true(all(is.finite(pred$response)))
  expect_true(all(is.finite(pred$se)))
  expect_true(all(pred$se >= 0))
  expect_lt(mean(abs(pred$response - pred$truth)), 0.02)
  expect_equal(l$model$direction, "increasing")
})

test_that("lce.spline_monotone needs at least five batches", {
  # scam's monotone basis needs k >= 4, and k is clamped to n_batches - 1, so
  # four distinct batches (the previous, broken minimum) must be rejected.
  set.seed(46)
  skip_if_not_installed("scam")
  l <- lrn("lce.spline_monotone")
  expect_error(l$train(make_nonparam_task(c(0.1, 0.2, 0.3, 0.4))), "at least five")
  expect_silent(l$train(make_nonparam_task(c(0.1, 0.2, 0.3, 0.4, 0.5))))
})
