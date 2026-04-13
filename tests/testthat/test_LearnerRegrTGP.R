skip_if_not_installed("tgp")

test_that("LearnerRegrTGP basic properties", {
  learner <- lrn("regr.tgp")
  expect_r6(learner, "LearnerRegrTGP")
  expect_r6(learner, "LearnerRegr")
  expect_equal(learner$predict_types, c("response", "se"))
  expect_subset(c("integer", "numeric"), learner$feature_types)
  expect_subset("tgp", learner$packages)
})

test_that("LearnerRegrTGP train and predict", {
  task <- make_gp_task()
  learner <- lrn("regr.tgp", BTE = c(1000L, 2000L, 2L))
  learner$train(task)

  expect_false(is.null(learner$model))

  pred <- learner$predict(task)
  expect_r6(pred, "PredictionRegr")
  expect_equal(length(pred$response), task$nrow)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrTGP se prediction", {
  task <- make_gp_task()
  learner <- lrn("regr.tgp", predict_type = "se", BTE = c(1000L, 2000L, 2L))
  learner$train(task)

  pred <- learner$predict(task)
  expect_equal(length(pred$se), task$nrow)
  expect_true(all(pred$se >= 0))
  # SE should be > 0 (kriging variance is non-zero except at training points,
  # but even those can be non-zero with nugget)
})

test_that("LearnerRegrTGP predict on new data", {
  task <- make_gp_task()
  new_task <- TaskRegr$new("new", backend = data.table(x1 = c(0.25, 0.75), y = c(0, 0)), target = "y")

  set.seed(11)
  learner <- lrn("regr.tgp", predict_type = "se", BTE = c(1000L, 2000L, 2L))
  learner$train(task)
  pred <- learner$predict(new_task)

  expect_equal(length(pred$response), 2L)
  expect_equal(length(pred$se), 2L)
  expect_true(all(is.finite(pred$response)))
  expect_true(all(is.finite(pred$se)))
  # SE at new locations should be > 0
  expect_true(all(pred$se > 0))
})

test_that("LearnerRegrTGP initial parameter values", {
  learner <- lrn("regr.tgp")
  pv <- learner$param_set$values

  # verb is set via init, discarded upstream prediction-output toggles are not exposed
  expect_equal(pv$verb, 0L)
  expect_false(any(c(
    "pred.n", "krige", "zcov", "Ds2x", "improv", "sens.p",
    "predict_pred.n", "predict_zcov", "predict_trace", "predict_verb"
  ) %in% learner$param_set$ids()))
})

test_that("LearnerRegrTGP matern correlation", {
  task <- make_gp_task()
  learner <- lrn("regr.tgp", corr = "matern", nu = 1.5,
    BTE = c(1000L, 2000L, 2L))
  learner$train(task)

  pred <- learner$predict(task)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrTGP multidimensional input", {
  task <- make_gp_task(d = 2L)
  learner <- lrn("regr.tgp", predict_type = "se", BTE = c(1000L, 2000L, 2L))
  learner$train(task)

  pred <- learner$predict(task)
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)
})
