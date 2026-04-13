skip_if_not_installed("hetGP")

test_that("LearnerRegrHetGP basic properties", {
  learner <- lrn("regr.hetgp")
  expect_r6(learner, "LearnerRegrHetGP")
  expect_r6(learner, "LearnerRegr")
  expect_equal(learner$predict_types, c("response", "se"))
  expect_subset(c("integer", "numeric"), learner$feature_types)
  expect_subset("hetGP", learner$packages)
  expect_false(any(c("settings", "noiseControl") %in% learner$param_set$ids()))
  expect_subset(
    c("link_thetas", "return_matrices", "pgtol"),
    learner$param_set$ids(tags = "settings")
  )
  expect_subset(
    c("g_bound_lower", "g_bound_upper", "g_max"),
    learner$param_set$ids(tags = "noise_control")
  )
  expect_equal(learner$param_set$lower[["trace"]], -1)
  expect_equal(learner$param_set$upper[["trace"]], 3)
  expect_equal(learner$param_set$default$trace, 0)
  expect_equal(learner$param_set$values$trace, -1)
})

test_that("LearnerRegrHetGP train and predict", {
  task <- make_gp_task()
  learner <- lrn("regr.hetgp")

  output <- capture.output(suppressMessages(learner$train(task)), type = "output")
  expect_length(output, 0L)
  expect_false(is.null(learner$model))

  pred <- learner$predict(task)
  expect_r6(pred, "PredictionRegr")
  expect_equal(length(pred$response), task$nrow)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrHetGP se prediction", {
  task <- make_gp_task()
  learner <- lrn("regr.hetgp", predict_type = "se")

  suppressMessages(learner$train(task))
  pred <- learner$predict(task)

  expect_equal(length(pred$se), task$nrow)
  expect_true(all(pred$se >= 0))
  # SE should include both sd2 and nugs, so should be > 0
  expect_true(all(pred$se > 0))
})

test_that("LearnerRegrHetGP predict on new data", {
  task <- make_gp_task()
  new_task <- TaskRegr$new("new", backend = data.table(x1 = c(0.25, 0.75), y = c(0, 0)), target = "y")

  learner <- lrn("regr.hetgp", predict_type = "se")
  suppressMessages(learner$train(task))
  pred <- learner$predict(new_task)

  expect_equal(length(pred$response), 2L)
  expect_equal(length(pred$se), 2L)
  expect_true(all(is.finite(pred$response)))
  expect_true(all(is.finite(pred$se)))
})

test_that("LearnerRegrHetGP covtype parameter", {
  task <- make_gp_task()
  learner <- lrn("regr.hetgp", covtype = "Matern5_2")

  suppressMessages(learner$train(task))
  pred <- learner$predict(task)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrHetGP multidimensional input", {
  task <- make_gp_task(d = 2L)
  learner <- lrn("regr.hetgp", predict_type = "se")

  suppressMessages(learner$train(task))
  pred <- learner$predict(task)
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)
})

test_that("LearnerRegrHetGP reconstructs settings and noiseControl", {
  task <- make_gp_task()
  learner <- lrn("regr.hetgp",
    maxit = 5L,
    link_thetas = "independent",
    pgtol = 1e-6,
    g_bound_upper = 2
  )

  suppressMessages(learner$train(task))

  expect_false(learner$model$used_args$settings$linkThetas)
  expect_false(learner$model$used_args$settings$return.matrices)
  expect_equal(learner$model$used_args$settings$pgtol, 1e-6)
  expect_equal(learner$model$used_args$noiseControl$g_bounds, c(1e-6, 2))
  expect_equal(learner$model$used_args$noiseControl$g_max, 100)
})
