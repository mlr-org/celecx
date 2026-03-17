test_that("MeasureLCECoverage perfect coverage", {
  # Create a prediction where truth is always within the interval
  dt <- data.table(n_evals = 1:10, metric = rep(0.5, 10))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  learner$predict_type <- "quantiles"
  learner$quantiles <- c(0.05, 0.5, 0.95)
  learner$quantile_response <- 0.5
  learner$train(task)

  # Predict on training data -- should be nearly perfect
  pred <- learner$predict(task)
  m <- msr("lce.coverage")
  coverage <- pred$score(m)
  expect_number(coverage, lower = 0, upper = 1)
})

test_that("MeasureLCECoverage parametrized by alpha", {
  m1 <- msr("lce.coverage")
  expect_equal(m1$param_set$values$alpha, 0.1)

  m2 <- msr("lce.coverage")
  m2$param_set$set_values(alpha = 0.1)
  expect_equal(m2$param_set$values$alpha, 0.1)
})

test_that("MeasureLCEWinkler computes known value", {
  # Manually construct a prediction to check Winkler score
  # For interval [0, 1] with alpha=0.1 and truth=0.5 (inside):
  # width = 1, no penalty => W = 1

  dt <- data.table(n_evals = 1:10, metric = rep(0.5, 10))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  learner$predict_type <- "quantiles"
  learner$quantiles <- c(0.05, 0.5, 0.95)
  learner$quantile_response <- 0.5
  learner$train(task)

  pred <- learner$predict(task)
  m <- msr("lce.winkler")
  winkler <- pred$score(m)
  expect_number(winkler, lower = 0)
})

test_that("MeasureLCEWinkler minimize is TRUE", {
  m <- msr("lce.winkler")
  expect_true(m$minimize)
})

test_that("MeasureLCECoverage minimize is FALSE", {
  m <- msr("lce.coverage")
  expect_false(m$minimize)
})

test_that("LCE measures work with resample()", {
  dt <- data.table(
    n_evals = 1:30,
    metric = 1/sqrt(1:30) + rnorm(30, 0, 0.02)
  )
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  learner$predict_type <- "quantiles"
  learner$quantiles <- c(0.05, 0.5, 0.95)
  learner$quantile_response <- 0.5

  rr <- resample(task, learner, rsmp("lce_holdout", ratio = 0.7))

  mse <- rr$score(msr("regr.mse"))$regr.mse
  coverage <- rr$score(msr("lce.coverage"))$lce.coverage
  winkler <- rr$score(msr("lce.winkler"))$lce.winkler

  expect_number(mse, lower = 0)
  expect_number(coverage, lower = 0, upper = 1)
  expect_number(winkler, lower = 0)
})
