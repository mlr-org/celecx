test_that("LearnerRegrGPfit trains and predicts", {
  skip_if_not_installed("GPfit")

  learner <- lrn("regr.gpfit")
  task <- make_gp_task()

  expect_r6(learner, "LearnerRegrGPfit")
  expect_equal(learner$param_set$default$corr_power, 1.95)
  expect_false("corr_power" %in% names(learner$param_set$values))

  learner$param_set$set_values(maxit = 10L)
  learner$train(task)
  prediction <- learner$predict(task)

  expect_r6(prediction, "PredictionRegr")
  expect_equal(length(prediction$response), task$nrow)
  expect_true(all(is.finite(prediction$response)))
  expect_true(all(is.finite(prediction$se)))
  expect_true(all(prediction$se >= 0))
})

test_that("LearnerRegrHetGP uses full predictive variance", {
  skip_if_not_installed("hetGP")

  learner <- lrn("regr.hetgp")
  task <- make_gp_task()

  expect_r6(learner, "LearnerRegrHetGP")

  learner$param_set$set_values(maxit = 5L)
  learner$train(task)
  prediction <- learner$predict(task)

  raw_prediction <- predict(
    learner$model,
    x = as.matrix(task$data(cols = task$feature_names))
  )

  expect_r6(prediction, "PredictionRegr")
  expect_true(all(is.finite(prediction$response)))
  expect_true(all(prediction$se >= 0))
  expect_equal(
    prediction$se,
    sqrt(raw_prediction$sd2 + raw_prediction$nugs),
    tolerance = 1e-6
  )
})

test_that("LearnerRegrTGP preserves package defaults and initialized quiet settings", {
  skip_if_not_installed("tgp")

  learner <- lrn("regr.tgp")
  task <- make_gp_task(n = 10L)

  expect_r6(learner, "LearnerRegrTGP")
  expect_equal(learner$param_set$default$verb, 1L)
  expect_equal(learner$param_set$values$verb, 0L)
  expect_equal(learner$param_set$default$pred.n, TRUE)
  expect_false(learner$param_set$values$pred.n)
  expect_equal(learner$param_set$default[["predict_pred.n"]], TRUE)
  expect_false(learner$param_set$values[["predict_pred.n"]])

  learner$param_set$set_values(
    BTE = c(10L, 20L, 1L),
    predict_BTE = c(0L, 5L, 1L)
  )
  learner$train(task)
  set.seed(1)
  prediction <- learner$predict(task)

  set.seed(1)
  raw_prediction <- get("predict.tgp", envir = asNamespace("tgp"))(
    learner$model,
    XX = as.matrix(task$data(cols = task$feature_names)),
    BTE = c(0L, 5L, 1L),
    pred.n = FALSE,
    verb = 0
  )

  expect_r6(prediction, "PredictionRegr")
  expect_true(all(is.finite(prediction$response)))
  expect_true(all(prediction$se >= 0))
  expect_equal(
    prediction$se,
    prediction_se_from_variance(tgp_prediction_variance(raw_prediction, task$nrow)),
    tolerance = 1e-6
  )
})

test_that("LearnerRegrDeepGP uses fit_two_layer defaults when D is unset", {
  skip_if_not_installed("deepgp")

  learner <- lrn("regr.deepgp")
  task <- make_gp_task()
  task_d2 <- make_gp_task(n = 8L, d = 2L)

  expect_r6(learner, "LearnerRegrDeepGP")
  expect_equal(learner$param_set$default$verb, TRUE)
  expect_false(learner$param_set$values$verb)
  expect_false("D" %in% names(learner$param_set$values))

  learner$param_set$set_values(
    nmcmc = 20L,
    true_g = 1e-6
  )
  learner$train(task)
  prediction <- learner$predict(task)

  expect_r6(prediction, "PredictionRegr")
  expect_true(all(is.finite(prediction$response)))
  expect_true(all(prediction$se >= 0))

  learner_d2 <- lrn("regr.deepgp")
  learner_d2$param_set$set_values(
    nmcmc = 20L,
    true_g = 1e-6
  )
  learner_d2$train(task_d2)

  expect_equal(dim(learner_d2$model$theta_w)[2], 2L)
})
