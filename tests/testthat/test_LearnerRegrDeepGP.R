skip_if_not_installed("deepgp")

test_that("LearnerRegrDeepGP basic properties", {
  learner <- lrn("regr.deepgp")
  expect_r6(learner, "LearnerRegrDeepGP")
  expect_r6(learner, "LearnerRegr")
  expect_equal(learner$predict_types, c("response", "se"))
  expect_subset(c("integer", "numeric"), learner$feature_types)
  expect_subset("deepgp", learner$packages)
  expect_false("settings" %in% learner$param_set$ids())
  expect_subset(
    c("proposal_window_lower", "proposal_window_upper", "theta_y_alpha", "tau2_w"),
    learner$param_set$ids(tags = "settings")
  )
})

test_that("LearnerRegrDeepGP train and predict", {
  task <- make_gp_task()
  learner <- lrn("regr.deepgp", nmcmc = 50L)
  learner$train(task)

  expect_false(is.null(learner$model))

  pred <- learner$predict(task)
  expect_r6(pred, "PredictionRegr")
  expect_equal(length(pred$response), task$nrow)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrDeepGP se prediction", {
  task <- make_gp_task()
  learner <- lrn("regr.deepgp", predict_type = "se", nmcmc = 50L)
  learner$train(task)

  pred <- learner$predict(task)
  expect_equal(length(pred$se), task$nrow)
  expect_true(all(pred$se >= 0))
  # SE should be > 0 for a deep GP
  expect_true(all(pred$se > 0))
})

test_that("LearnerRegrDeepGP predict on new data", {
  task <- make_gp_task()
  new_task <- TaskRegr$new("new", backend = data.table(x1 = c(0.25, 0.75), y = c(0, 0)), target = "y")

  learner <- lrn("regr.deepgp", predict_type = "se", nmcmc = 50L)
  learner$train(task)
  pred <- learner$predict(new_task)

  expect_equal(length(pred$response), 2L)
  expect_equal(length(pred$se), 2L)
  expect_true(all(is.finite(pred$response)))
  expect_true(all(is.finite(pred$se)))
})

test_that("LearnerRegrDeepGP initial parameter values", {
  learner <- lrn("regr.deepgp")
  pv <- learner$param_set$values

  # verb and thread parameters are set via init
  expect_false(pv$verb)
  expect_equal(pv$train_cores, 1L)
  expect_equal(pv$predict_cores, 1L)
  expect_equal(learner$param_set$default$proposal_window_lower, 1)
  expect_equal(learner$param_set$default$proposal_window_upper, 2)
  expect_equal(learner$param_set$default$tau2_w, 1)
})

test_that("LearnerRegrDeepGP supports mlr3 set_threads", {
  learner <- lrn("regr.deepgp")

  expect_set_equal(
    learner$param_set$ids(tags = "threads"),
    c("train_cores", "predict_cores")
  )

  set_threads(learner, 3L)

  expect_equal(learner$param_set$values$train_cores, 3L)
  expect_equal(learner$param_set$values$predict_cores, 3L)
})

test_that("LearnerRegrDeepGP multidimensional input", {
  task <- make_gp_task(d = 2L)
  learner <- lrn("regr.deepgp", predict_type = "se", nmcmc = 50L)
  learner$train(task)

  pred <- learner$predict(task)
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)
})

test_that("LearnerRegrDeepGP exp2 covariance", {
  task <- make_gp_task()
  learner <- lrn("regr.deepgp", cov = "exp2", nmcmc = 50L)
  learner$train(task)

  pred <- learner$predict(task)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrDeepGP reconstructs settings", {
  task <- make_gp_task()
  learner <- lrn("regr.deepgp",
    nmcmc = 20L,
    true_g = 1e-6,
    proposal_window_upper = 3,
    theta_y_alpha = 1.7,
    theta_w_beta = 0.4,
    tau2_w = 2
  )

  learner$train(task)

  expect_equal(learner$model$settings$u, 3)
  expect_equal(learner$model$settings$theta_y$alpha, 1.7)
  expect_equal(learner$model$settings$theta_w$beta, 0.4)
  expect_equal(learner$model$settings$tau2_w, 2)
})
