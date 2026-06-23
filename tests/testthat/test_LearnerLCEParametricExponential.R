make_curve_task <- function(n_batches = 8L, per_batch = 3L,
    asymptote = 0.9, amplitude = -0.7, rate = 0.4) {
  batches <- rep(seq_len(n_batches), each = per_batch)
  perf <- asymptote + amplitude * exp(-rate * batches)
  dt <- data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(batches),
    perf = perf
  )
  TaskLCE$new("curve", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
}

test_that("LearnerLCEParametricExponential recovers known curve", {
  set.seed(7)
  task <- make_curve_task()
  learner <- lrn("lce.parametric_exponential")
  learner$train(task)
  coefs <- learner$model$coefficients
  expect_equal(coefs[["asymptote"]], 0.9, tolerance = 1e-4)
  expect_equal(coefs[["amplitude"]], -0.7, tolerance = 1e-4)
  expect_equal(coefs[["rate"]], 0.4, tolerance = 1e-4)
})

test_that("LearnerLCEParametricExponential predictions match the curve", {
  set.seed(8)
  task <- make_curve_task()
  learner <- lrn("lce.parametric_exponential")
  learner$train(task)
  pred <- learner$predict(task)
  expect_r6(pred, "PredictionLCE")
  expect_equal(pred$response, pred$truth, tolerance = 1e-4)
})

test_that("LearnerLCEParametricExponential errors on too-few batches", {
  set.seed(9)
  dt <- data.table(
    x1 = rnorm(3), x2 = rnorm(3), y = rnorm(3),
    batch_nr = rep(1L, 3L), perf = rnorm(3)
  )
  task <- TaskLCE$new("single", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  learner <- lrn("lce.parametric_exponential")
  expect_error(learner$train(task), "at least two distinct batches")
})

test_that("LearnerLCEParametricExponential supports predict_newdata", {
  set.seed(10)
  task <- make_curve_task()
  learner <- lrn("lce.parametric_exponential")
  learner$train(task)
  newdata <- data.table(batch_nr = c(10L, 12L, 15L))
  pred <- learner$predict_newdata(newdata)
  expected <- 0.9 + (-0.7) * exp(-0.4 * c(10, 12, 15))
  expect_equal(pred$response, expected, tolerance = 1e-3)
})
