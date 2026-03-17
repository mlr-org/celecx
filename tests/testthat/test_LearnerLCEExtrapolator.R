test_that("LearnerLCEExtrapolator wraps CurveExtrapolatorParametric", {
  dt <- data.table(n_evals = 1:20, metric = 1/sqrt(1:20) + rnorm(20, 0, 0.01))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  expect_equal(learner$id, "lce.extrapolator.parametric")
  expect_true("featureless" %in% learner$properties)

  learner$train(task)
  expect_r6(learner$model, "CurveExtrapolator")

  pred <- learner$predict(task, row_ids = 15:20)
  expect_numeric(pred$response, len = 6, any.missing = FALSE)
})

test_that("LearnerLCEExtrapolator predict_type se", {
  dt <- data.table(n_evals = 1:20, metric = 1/sqrt(1:20) + rnorm(20, 0, 0.02))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  learner$predict_type <- "se"
  learner$train(task)

  pred <- learner$predict(task, row_ids = 18:20)
  expect_numeric(pred$response, len = 3, any.missing = FALSE)
  expect_numeric(pred$se, len = 3, lower = 0, any.missing = FALSE)
})

test_that("LearnerLCEExtrapolator predict_type quantiles", {
  dt <- data.table(n_evals = 1:20, metric = 1/sqrt(1:20) + rnorm(20, 0, 0.02))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  learner$predict_type <- "quantiles"
  learner$quantiles <- c(0.05, 0.5, 0.95)
  learner$quantile_response <- 0.5
  learner$train(task)

  pred <- learner$predict(task, row_ids = 18:20)
  expect_matrix(pred$data$quantiles, nrows = 3, ncols = 3)
  expect_numeric(pred$response, len = 3, any.missing = FALSE)

  probs <- attr(pred$data$quantiles, "probs")
  expect_equal(probs, c(0.05, 0.5, 0.95))
})

test_that("LearnerLCEExtrapolator works with resample()", {
  dt <- data.table(n_evals = 1:30, metric = 1/sqrt(1:30) + rnorm(30, 0, 0.01))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")
  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())

  rr <- resample(task, learner, rsmp("lce_holdout", ratio = 0.7))
  scores <- rr$score(msr("regr.mse"))
  expect_numeric(scores$regr.mse, len = 1, lower = 0)
})

test_that("LearnerLCEExtrapolator works with resample() + CV", {
  dt <- data.table(n_evals = 1:30, metric = 1/sqrt(1:30) + rnorm(30, 0, 0.01))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")
  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())

  cv <- rsmp("lce_cv", folds = 3, horizon = 2, window_size = 10)
  rr <- resample(task, learner, cv)
  scores <- rr$score(msr("regr.mse"))
  expect_numeric(scores$regr.mse, len = 3, lower = 0)
})

test_that("LearnerLCEExtrapolator predict_newdata works", {
  dt <- data.table(n_evals = 1:20, metric = 1/sqrt(1:20))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  learner$train(task)

  newdata <- generate_newdata_lce(task, c(25L, 30L))
  pred <- learner$predict_newdata(newdata)
  expect_numeric(pred$response, len = 2, any.missing = FALSE)
})

test_that("LearnerLCEExtrapolator clone works", {
  learner <- LearnerLCEExtrapolator$new(CurveExtrapolatorParametric$new())
  clone <- learner$clone(deep = TRUE)

  expect_false(identical(learner$extrapolator, clone$extrapolator))
  expect_equal(learner$id, clone$id)
})

test_that("Standard regression learner works with TaskLCE", {
  dt <- data.table(n_evals = 1:20, metric = 1/sqrt(1:20))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  learner <- lrn("regr.featureless")
  rr <- resample(task, learner, rsmp("lce_holdout", ratio = 0.7))
  scores <- rr$score(msr("regr.mse"))
  expect_numeric(scores$regr.mse, len = 1, lower = 0)
})
