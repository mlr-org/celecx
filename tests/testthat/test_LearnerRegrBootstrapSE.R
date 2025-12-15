test_that("LearnerRegrBootstrapSE basic functionality", {
  # Create a simple learner
  base_learner <- lrn("regr.rpart")
  learner <- LearnerRegrBootstrapSE$new(base_learner)
  learner$param_set$set_values(n_bootstrap = 5L)

  # Check initialization
  expect_r6(learner, "LearnerRegrBootstrapSE")
  expect_r6(learner, "LearnerRegr")
  expect_equal(learner$predict_types, c("response", "se"))
  expect_equal(learner$param_set$values$n_bootstrap, 5L)

  # Check base_learner() method
  expect_r6(learner$base_learner(), "LearnerRegr")
  expect_equal(learner$base_learner()$id, "regr.rpart")
})

test_that("LearnerRegrBootstrapSE has default n_bootstrap", {
  # Create learner
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.featureless"))

  expect_r6(learner, "LearnerRegrBootstrapSE")
  expect_equal(learner$param_set$values$n_bootstrap, 30L)  # Default
  expect_equal(learner$base_learner()$id, "regr.featureless")
})

test_that("LearnerRegrBootstrapSE training and prediction", {
  # Create test task
  task <- tsk("mtcars")

  # Create and train learner
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.rpart"))
  learner$param_set$set_values(n_bootstrap = 5L)
  learner$train(task)

  # Check model is trained
  expect_false(is.null(learner$model))
  expect_equal(learner$model$n_bootstrap, 5L)

  # Predict
  pred <- learner$predict(task)

  # Check prediction has both response and SE
  expect_r6(pred, "PredictionRegr")
  expect_false(is.null(pred$response))
  expect_false(is.null(pred$se))
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)

  # SE should be non-negative
  expect_true(all(pred$se >= 0))

  # SE should be non-zero for at least some predictions (variability in bootstrap)
  expect_true(any(pred$se > 0))
})

test_that("LearnerRegrBootstrapSE preserves base learner properties", {
  base_learner <- lrn("regr.rpart")
  learner <- LearnerRegrBootstrapSE$new(base_learner)

  # Should inherit feature types from base
  expect_equal(learner$feature_types, base_learner$feature_types)

  # Should inherit properties from base
  expect_equal(learner$properties, base_learner$properties)

  # Should include base learner packages
  expect_true("rpart" %in% learner$packages)
})

test_that("LearnerRegrBootstrapSE parameter validation", {
  learner <- LearnerRegrBootstrapSE$new(lrn("regr.featureless"))

  # n_bootstrap must be at least 2
  expect_error(
    learner$param_set$set_values(n_bootstrap = 1L),
    "is not >= 1.5"  # Paradox error message
  )

  # learner must be a regression learner
  expect_error(
    LearnerRegrBootstrapSE$new(lrn("classif.featureless")),
    "regr"
  )
})

test_that("LearnerRegrBootstrapSE works on new data", {
  task_train <- tsk("mtcars")
  task_test <- task_train$clone()
  task_test$filter(1:10)  # Use subset for testing

  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.featureless"))
  learner$param_set$set_values(n_bootstrap = 3L)
  learner$train(task_train)

  # Predict on new data
  pred <- learner$predict(task_test)

  expect_equal(length(pred$response), 10L)
  expect_equal(length(pred$se), 10L)
  expect_true(all(!is.na(pred$response)))
  expect_true(all(!is.na(pred$se)))
})

test_that("LearnerRegrBootstrapSE wrapped field provides access to base learner", {
  base_learner <- lrn("regr.rpart")
  learner <- LearnerRegrBootstrapSE$new(base_learner)

  # $wrapped should return the base learner
  expect_r6(learner$wrapped, "LearnerRegr")
  expect_equal(learner$wrapped$id, "regr.rpart")

  # $wrapped should be read-only
  expect_error(learner$wrapped <- lrn("regr.featureless"), "read-only")
})

test_that("LearnerRegrBootstrapSE param_set allows setting base learner params", {
  base_learner <- lrn("regr.rpart")
  learner <- LearnerRegrBootstrapSE$new(base_learner)

  # Should be able to set base learner params via wrapper's param_set
  learner$param_set$set_values(maxdepth = 3L)

  # Setting via wrapper should affect wrapped learner
  expect_equal(learner$wrapped$param_set$values$maxdepth, 3L)

  # Should also be able to set wrapper's own params
  learner$param_set$set_values(n_bootstrap = 10L)
  expect_equal(learner$param_set$values$n_bootstrap, 10L)
})

test_that("LearnerRegrBootstrapSE param_set changes via wrapped learner are visible", {
  base_learner <- lrn("regr.rpart")
  learner <- LearnerRegrBootstrapSE$new(base_learner)

  # Set param directly on wrapped learner
  learner$wrapped$param_set$set_values(maxdepth = 5L)

  # Should be visible via wrapper's param_set
  expect_equal(learner$param_set$values$maxdepth, 5L)
})

test_that("LearnerRegrBootstrapSE cloning keeps param sets independent", {
  base_learner <- lrn("regr.rpart")
  original <- LearnerRegrBootstrapSE$new(base_learner)
  original$param_set$set_values(maxdepth = 3L, n_bootstrap = 5L)

  # Clone the learner
  clone <- original$clone(deep = TRUE)

  # Modify clone's params
  clone$param_set$set_values(maxdepth = 10L, n_bootstrap = 15L)

  # Original should be unchanged
  expect_equal(original$param_set$values$maxdepth, 3L)
  expect_equal(original$param_set$values$n_bootstrap, 5L)
  expect_equal(original$wrapped$param_set$values$maxdepth, 3L)

  # Clone should have new values
  expect_equal(clone$param_set$values$maxdepth, 10L)
  expect_equal(clone$param_set$values$n_bootstrap, 15L)
  expect_equal(clone$wrapped$param_set$values$maxdepth, 10L)
})

test_that("LearnerRegrBootstrapSE $wrapped is not trained after wrapper training", {
  task <- tsk("mtcars")
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.rpart"))
  learner$param_set$set_values(n_bootstrap = 3L)

  # Before training, wrapped should not be trained
  expect_null(learner$wrapped$state)

  # Train the wrapper
  learner$train(task)

  # After training wrapper, $wrapped should still not be trained
  expect_null(learner$wrapped$state)
})

test_that("LearnerRegrBootstrapSE $wrapped is direct reference", {
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.rpart"))

  # Set param on wrapped learner
  learner$wrapped$param_set$set_values(maxdepth = 7L)

  # Should be visible via wrapper's param_set
  expect_equal(learner$param_set$values$maxdepth, 7L)

  # Set param via wrapper's param_set
  learner$param_set$set_values(maxdepth = 9L)

  # Should be visible on wrapped learner
  expect_equal(learner$wrapped$param_set$values$maxdepth, 9L)
})

test_that("LearnerRegrBootstrapSE $base_learner() is trained and is deep clone", {
  task <- tsk("mtcars")
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.rpart"))
  learner$param_set$set_values(n_bootstrap = 3L, maxdepth = 5L)
  learner$train(task)

  # Get base_learner
  base <- learner$base_learner()

  # base_learner() should be trained
  expect_false(is.null(base$state))

  # Should be able to predict with base_learner
  pred <- base$predict(task)
  expect_r6(pred, "PredictionRegr")

  # Modifying base_learner's params should not affect wrapper
  base$param_set$set_values(maxdepth = 10L)
  expect_equal(learner$param_set$values$maxdepth, 5L)
  expect_equal(learner$wrapped$param_set$values$maxdepth, 5L)
})

test_that("LearnerRegrBootstrapSE model has correct state class", {
  task <- tsk("mtcars")
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.rpart"))
  learner$param_set$set_values(n_bootstrap = 3L)
  learner$train(task)

  # Model should have the state class
  expect_true(inherits(learner$model, "learner_regr_bootstrap_se_state"))
})

test_that("LearnerRegrBootstrapSE marshaling works", {
  task <- tsk("mtcars")
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.rpart"))
  learner$param_set$set_values(n_bootstrap = 3L)
  learner$train(task)

  # Get original predictions
  pred_original <- learner$predict(task)

  # Marshal the model
  model_marshaled <- marshal_model(learner$model)

  # Check if marshaling was applied (depends on base learner)
  # For rpart, marshaling may or may not be needed
  if (is_marshaled_model(model_marshaled)) {
    expect_true(inherits(model_marshaled, "learner_regr_bootstrap_se_state_marshaled"))
    expect_true(inherits(model_marshaled, "marshaled"))

    # Unmarshal
    model_unmarshaled <- unmarshal_model(model_marshaled)
    expect_true(inherits(model_unmarshaled, "learner_regr_bootstrap_se_state"))
    expect_false(inherits(model_unmarshaled, "marshaled"))

    # Predictions should be identical after marshal/unmarshal
    learner$model <- model_unmarshaled
    pred_after <- learner$predict(task)
    expect_equal(pred_after$response, pred_original$response)
    expect_equal(pred_after$se, pred_original$se)
  } else {
    # Model didn't need marshaling (rpart is serializable)
    expect_identical(model_marshaled, learner$model)
  }
})

test_that("LearnerRegrBootstrapSE serialization with saveRDS/readRDS works", {
  skip_if_not_installed("ranger")

  task <- tsk("mtcars")
  # Use ranger which has external pointers that need marshaling
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.ranger"))
  learner$param_set$set_values(n_bootstrap = 3L, num.trees = 10L)
  learner$train(task)

  # Get original predictions
  pred_original <- learner$predict(task)

  # Marshal the model before saving
  learner$model <- marshal_model(learner$model)

  # Save to temp file
  tmpfile <- tempfile(fileext = ".rds")
  on.exit(unlink(tmpfile), add = TRUE)
  saveRDS(learner, tmpfile)

  # Load from file
  learner_loaded <- readRDS(tmpfile)

  # Unmarshal after loading
  learner_loaded$model <- unmarshal_model(learner_loaded$model)

  # Should be able to predict
  pred_after <- learner_loaded$predict(task)

  # Predictions should be identical
  expect_equal(pred_after$response, pred_original$response)
  expect_equal(pred_after$se, pred_original$se)
})

test_that("LearnerRegrBootstrapSE marshaling preserves all bootstrap states", {
  skip_if_not_installed("ranger")

  task <- tsk("mtcars")
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.ranger"))
  learner$param_set$set_values(n_bootstrap = 5L, num.trees = 10L)
  learner$train(task)

  # Original model should have 5 bootstrap states
  expect_equal(length(learner$model$bootstrap_states), 5L)
  expect_equal(learner$model$n_bootstrap, 5L)

  # Marshal
  model_marshaled <- marshal_model(learner$model)

  # If marshaling was applied, check the structure
  if (is_marshaled_model(model_marshaled)) {
    # Unmarshaled model should still have 5 states
    model_unmarshaled <- unmarshal_model(model_marshaled)
    expect_equal(length(model_unmarshaled$bootstrap_states), 5L)
    expect_equal(model_unmarshaled$n_bootstrap, 5L)

    # Predictions should work
    learner$model <- model_unmarshaled
  } else {
    # Marshaling not needed, model unchanged
    expect_identical(model_marshaled, learner$model)
  }

  pred <- learner$predict(task)
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)
})
