skip_if_not_installed("GPfit")

test_that("LearnerRegrGPfit basic properties", {
  learner <- lrn("regr.gpfit")
  expect_r6(learner, "LearnerRegrGPfit")
  expect_r6(learner, "LearnerRegr")
  expect_equal(learner$predict_types, c("response", "se"))
  expect_subset(c("integer", "numeric"), learner$feature_types)
  expect_subset("GPfit", learner$packages)
  expect_set_equal(
    learner$param_set$ids(tags = "control"),
    c("control_search", "control_best", "control_cluster")
  )
  expect_true("scale" %in% learner$param_set$ids(tags = "required"))
  expect_equal(learner$param_set$values$scale, TRUE)
  expect_false("scale" %in% names(learner$param_set$default))
  expect_false("control" %in% learner$param_set$ids())
})

test_that("LearnerRegrGPfit train and predict", {
  task <- make_gp_task()
  learner <- lrn("regr.gpfit")
  learner$train(task)

  expect_false(is.null(learner$model))

  pred <- learner$predict(task)
  expect_r6(pred, "PredictionRegr")
  expect_equal(length(pred$response), task$nrow)

  # GPfit interpolates training data exactly (up to numerical noise)
  expect_true(all(abs(pred$response - task$truth()) < 1e-3))
})

test_that("LearnerRegrGPfit se prediction", {
  task <- make_gp_task()
  learner <- lrn("regr.gpfit", predict_type = "se")
  learner$train(task)

  pred <- learner$predict(task)
  expect_equal(length(pred$se), task$nrow)
  expect_true(all(pred$se >= 0))
})

test_that("LearnerRegrGPfit predict on new data", {
  task <- make_gp_task()
  new_task <- TaskRegr$new("new", backend = data.table(x1 = c(0.25, 0.75), y = c(0, 0)), target = "y")

  learner <- lrn("regr.gpfit", predict_type = "se")
  learner$train(task)
  pred <- learner$predict(new_task)

  expect_equal(length(pred$response), 2L)
  expect_equal(length(pred$se), 2L)
  expect_true(all(is.finite(pred$response)))
  expect_true(all(is.finite(pred$se)))
})

test_that("LearnerRegrGPfit matern correlation", {
  task <- make_gp_task()
  learner <- lrn("regr.gpfit", corr_type = "matern", corr_nu = 2.5)
  learner$train(task)

  pred <- learner$predict(task)
  expect_true(all(is.finite(pred$response)))
})

test_that("LearnerRegrGPfit multidimensional input", {
  task <- make_gp_task(d = 2L)
  learner <- lrn("regr.gpfit", predict_type = "se")
  expect_silent(learner$train(task))

  pred <- learner$predict(task)
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)
})

test_that("LearnerRegrGPfit control parameters can be set partially", {
  task <- make_gp_task()
  learner <- lrn("regr.gpfit", control_best = 10L, maxit = 5L)

  expect_silent(learner$train(task))
  expect_false(is.null(learner$model))
})

test_that("LearnerRegrGPfit validates control parameter ordering", {
  task <- make_gp_task()
  learner <- lrn("regr.gpfit", control_search = 2L, control_best = 3L)

  expect_error(
    learner$train(task),
    "control_search must be greater than or equal to control_best"
  )
})

test_that("LearnerRegrGPfit stores scaling and drops constant features", {
  task <- TaskRegr$new(
    id = "gpfit_constant_feature",
    backend = data.table(
      x1 = seq(10, 80, length.out = 8L),
      x2 = rep(5, 8L),
      y = sin(seq(0.1, 0.8, length.out = 8L))
    ),
    target = "y"
  )
  learner <- lrn("regr.gpfit", predict_type = "se")

  expect_silent(learner$train(task))
  expect_named(learner$model$feature_offset, c("x1", "x2"))
  expect_named(learner$model$feature_scaling, c("x1", "x2"))
  expect_equal(learner$model$feature_offset[["x2"]], 0)
  expect_equal(learner$model$feature_scaling[["x2"]], 0)
  expect_equal(ncol(learner$model$X), 1L)

  new_task <- TaskRegr$new(
    id = "gpfit_constant_feature_predict",
    backend = data.table(
      x1 = c(35, 35),
      x2 = c(-100, 100),
      y = c(0, 0)
    ),
    target = "y"
  )
  pred <- learner$predict(new_task)

  expect_equal(pred$response[1], pred$response[2], tolerance = 1e-10)
  expect_equal(pred$se[1], pred$se[2], tolerance = 1e-10)
})

test_that("LearnerRegrGPfit can disable scaling", {
  task <- TaskRegr$new(
    id = "gpfit_unscaled",
    backend = data.table(
      x1 = seq(10, 80, length.out = 8L),
      x2 = rep(5, 8L),
      y = sin(seq(0.1, 0.8, length.out = 8L))
    ),
    target = "y"
  )
  learner <- lrn("regr.gpfit", scale = FALSE, predict_type = "se")

  expect_warning(learner$train(task), "X should be in range \\(0, 1\\)")
  expect_equal(learner$model$feature_offset, c(x1 = 0, x2 = 0))
  expect_equal(learner$model$feature_scaling, c(x1 = 1, x2 = 1))
  expect_equal(ncol(learner$model$X), 2L)
})
