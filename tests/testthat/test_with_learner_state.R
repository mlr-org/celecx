test_that("with_learner_state harvests a state and leaves the learner untrained", {
  task <- tsk("mtcars")
  learner <- lrn("regr.rpart")

  state <- with_learner_state(learner, function(l) l$train(task)$state)

  expect_s3_class(state, "learner_state")
  expect_false(is.null(state$model))
  expect_false(is.null(state$train_task))
  expect_null(learner$state)
  expect_null(learner$model)
})

test_that("with_learner_state injects a state for prediction and resets afterwards", {
  task <- tsk("mtcars")
  learner <- lrn("regr.rpart")
  state <- with_learner_state(learner, function(l) l$train(task)$state)

  pred <- with_learner_state(learner, function(l) l$predict_newdata_fast(task$data()),
    state = state)

  expect_length(pred$response, task$nrow)
  expect_true(all(!is.na(pred$response)))
  expect_null(learner$state)

  # the injected state is not modified by the prediction
  expect_s3_class(state, "learner_state")
  expect_false(is.null(state$model))
})

test_that("with_learner_state overrides and restores predict_type", {
  task <- tsk("mtcars")
  learner <- lrn("regr.featureless")
  learner$predict_type <- "se"

  seen <- with_learner_state(learner, function(l) l$predict_type, predict_type = "response")

  expect_equal(seen, "response")
  expect_equal(learner$predict_type, "se")
})

test_that("with_learner_state restores a previously set quantile configuration", {
  learner <- lrn("regr.featureless")
  learner$quantiles <- c(0.25, 0.75)
  learner$quantile_response <- 0.5

  seen <- with_learner_state(learner, function(l) l$quantiles,
    quantiles = c(0.1, 0.9), quantile_response = 0.5)

  # inside fn: overridden quantiles with the response probability unioned in
  expect_equal(seen, c(0.1, 0.5, 0.9))
  # afterwards: the previous configuration is back
  expect_equal(learner$quantiles, c(0.25, 0.5, 0.75))
  expect_equal(learner$quantile_response, 0.5)
})

test_that("with_learner_state resets the learner when fn errors", {
  task <- tsk("mtcars")
  learner <- lrn("regr.featureless")
  learner$predict_type <- "se"
  state <- with_learner_state(learner, function(l) l$train(task)$state)

  expect_error(
    with_learner_state(learner, function(l) stop("boom"),
      state = state, predict_type = "response"),
    "boom"
  )

  expect_null(learner$state)
  expect_equal(learner$predict_type, "se")
})
