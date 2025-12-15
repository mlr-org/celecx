# Helper: Create a simple mock quantile learner for testing
make_mock_quantile_learner <- function() {
  LearnerRegrMockQuantile <- R6Class("LearnerRegrMockQuantile",
    inherit = LearnerRegr,
    public = list(
      initialize = function() {
        super$initialize(
          id = "regr.mock_quantile",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response", "quantiles"),
          properties = c("missings"),
          packages = character(),
          param_set = ps(
            spread_multiplier = p_dbl(lower = 0.1, upper = 10, default = 2)
          ),
          man = "test::LearnerRegrMockQuantile"
        )
        self$predict_type <- "response"
      }
    ),
    private = list(
      .train = function(task) {
        # Store simple mean for predictions
        list(mean_y = mean(task$truth()))
      },
      .predict = function(task) {
        n <- task$nrow

        if (self$predict_type == "quantiles") {
          # Generate mock quantiles around the mean
          spread_multiplier <- self$param_set$values$spread_multiplier
          if (is.null(spread_multiplier)) spread_multiplier <- 2

          quantiles <- self$quantiles
          if (is.null(quantiles)) quantiles <- c(0.1, 0.5, 0.9)

          q_matrix <- matrix(NA_real_, nrow = n, ncol = length(quantiles))

          for (i in seq_along(quantiles)) {
            # Create mock quantile predictions with some spread
            # Lower quantiles -> subtract from mean, upper -> add
            offset <- qnorm(quantiles[i]) * spread_multiplier
            q_matrix[, i] <- self$model$mean_y + offset
          }

          # Set required attributes for mlr3
          setattr(q_matrix, "probs", quantiles)
          setattr(q_matrix, "response", 0.5)

          # Calculate response at quantile_response level
          if (!is.null(self$quantile_response)) {
            # Find closest quantile or interpolate
            offset_response <- qnorm(self$quantile_response) * spread_multiplier
            response <- rep(self$model$mean_y + offset_response, n)
          } else {
            # Default to mean if quantile_response not set
            response <- rep(self$model$mean_y, n)
          }

          PredictionRegr$new(task = task, response = response, quantiles = q_matrix)
        } else {
          response <- rep(self$model$mean_y, n)
          PredictionRegr$new(task = task, response = response)
        }
      }
    )
  )

  LearnerRegrMockQuantile$new()
}

test_that("LearnerRegrQuantileSE basic functionality", {
  # Create a quantile-capable learner
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Check initialization
  expect_r6(learner, "LearnerRegrQuantileSE")
  expect_r6(learner, "LearnerRegr")
  expect_equal(learner$predict_types, c("response", "se"))
  expect_equal(learner$param_set$values$quantile_lower, 0.1)  # Default
  expect_equal(learner$param_set$values$quantile_upper, 0.9)  # Default
  expect_equal(learner$param_set$values$quantile_response, 0.5)  # Default
  expect_equal(learner$param_set$values$se_factor, 0.5)  # Default

  # Check base_learner() method
  expect_r6(learner$base_learner(), "LearnerRegr")
  expect_equal(learner$base_learner()$id, "regr.mock_quantile")
})

test_that("LearnerRegrQuantileSE has default quantiles", {
  # Create learner
  base_learner <- make_mock_quantile_learner()
  learner <- lrn("regr.quantile_se", learner = base_learner)

  expect_r6(learner, "LearnerRegrQuantileSE")
  expect_equal(learner$param_set$values$quantile_lower, 0.1)
  expect_equal(learner$param_set$values$quantile_upper, 0.9)
  expect_equal(learner$param_set$values$quantile_response, 0.5)
  expect_equal(learner$base_learner()$id, "regr.mock_quantile")
})

test_that("LearnerRegrQuantileSE training and prediction", {
  # Create test task
  task <- tsk("mtcars")

  # Create and train learner
  base_learner <- make_mock_quantile_learner()
  learner <- lrn("regr.quantile_se", learner = base_learner)
  learner$train(task)

  # Check model is trained - now only 2 quantiles stored
  expect_false(is.null(learner$model))
  expect_equal(learner$model$quantiles, c(0.1, 0.9))
  expect_equal(learner$model$quantile_response, 0.5)

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

  # SE should be non-zero for at least some predictions (uncertainty in quantiles)
  expect_true(any(pred$se > 0))
})

test_that("LearnerRegrQuantileSE SE calculation correctness", {
  # Create test task
  task <- tsk("mtcars")

  # Create learner with specific quantiles
  base_learner <- make_mock_quantile_learner()
  learner <- lrn("regr.quantile_se", learner = base_learner)
  learner$train(task)

  # Get quantile prediction directly from base learner
  base_learner_trained <- learner$base_learner()
  base_learner_trained$predict_type <- "quantiles"
  base_learner_trained$quantiles <- c(0.1, 0.9)
  base_learner_trained$quantile_response <- 0.5
  pred_quantiles <- base_learner_trained$predict(task)
  q_matrix <- pred_quantiles$quantiles

  # Get SE prediction from wrapper
  pred_se <- learner$predict(task)

  # Calculate expected SE manually with new formula
  lower_q <- q_matrix[, 1]
  upper_q <- q_matrix[, 2]
  expected_se <- (upper_q - lower_q) * 0.5  # Default se_factor

  # Check that response uses quantile_response (0.5 = median)
  expect_equal(pred_se$response, pred_quantiles$response)

  # Check that SE matches our calculation
  expect_equal(pred_se$se, expected_se)
})

test_that("LearnerRegrQuantileSE SE calculation with different se_factor", {
  task <- tsk("mtcars")

  # Test with se_factor = 1.0
  base_learner <- make_mock_quantile_learner()
  learner <- lrn("regr.quantile_se", learner = base_learner)
  learner$param_set$set_values(se_factor = 1.0)
  learner$train(task)
  pred <- learner$predict(task)

  # Get direct quantile predictions
  base_trained <- learner$base_learner()
  base_trained$predict_type <- "quantiles"
  base_trained$quantiles <- c(0.1, 0.9)
  base_trained$quantile_response <- 0.5
  pred_q <- base_trained$predict(task)

  # SE should equal the full range
  expected_se <- pred_q$quantiles[, 2] - pred_q$quantiles[, 1]
  expect_equal(pred$se, expected_se)
})

test_that("LearnerRegrQuantileSE preserves base learner properties", {
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Should inherit feature types from base
  expect_equal(learner$feature_types, base_learner$feature_types)

  # Should inherit properties from base
  expect_equal(learner$properties, base_learner$properties)

  # Should include base learner packages (mlr3 is always added)
  expect_true("mlr3" %in% learner$packages)
})

test_that("LearnerRegrQuantileSE parameter validation", {
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # quantile_lower must be >= 0
  expect_error(
    learner$param_set$set_values(quantile_lower = -0.1),
    "is not >="  # Paradox error message
  )

  # quantile_upper must be <= 1
  expect_error(
    learner$param_set$set_values(quantile_upper = 1.1),
    "is not <="  # Paradox error message
  )

  # quantile_lower must be < quantile_upper (checked during training)
  learner2 <- LearnerRegrQuantileSE$new(make_mock_quantile_learner())
  learner2$param_set$set_values(quantile_lower = 0.9, quantile_upper = 0.1)
  expect_error(
    learner2$train(tsk("mtcars")),
    "quantile_lower must be less than quantile_upper"
  )

  # learner must support quantile predictions
  expect_error(
    LearnerRegrQuantileSE$new(lrn("regr.rpart")),
    "does not support quantile predictions"
  )

  # learner must be a regression learner
  expect_error(
    LearnerRegrQuantileSE$new(lrn("classif.featureless")),
    "regr"
  )
})

test_that("LearnerRegrQuantileSE works on new data", {
  task_train <- tsk("mtcars")
  task_test <- task_train$clone()
  task_test$filter(1:10)  # Use subset for testing

  base_learner <- make_mock_quantile_learner()
  learner <- lrn("regr.quantile_se", learner = base_learner)
  learner$train(task_train)

  # Predict on new data
  pred <- learner$predict(task_test)

  expect_equal(length(pred$response), 10L)
  expect_equal(length(pred$se), 10L)
  expect_true(all(!is.na(pred$response)))
  expect_true(all(!is.na(pred$se)))
})

test_that("LearnerRegrQuantileSE with different quantile ranges", {
  task <- tsk("mtcars")

  # Narrow range (higher confidence)
  base_learner_narrow <- make_mock_quantile_learner()
  learner_narrow <- lrn("regr.quantile_se", learner = base_learner_narrow)
  learner_narrow$param_set$set_values(quantile_lower = 0.4, quantile_upper = 0.6)
  learner_narrow$train(task)
  pred_narrow <- learner_narrow$predict(task)

  # Wide range (lower confidence)
  base_learner_wide <- make_mock_quantile_learner()
  learner_wide <- lrn("regr.quantile_se", learner = base_learner_wide)
  learner_wide$param_set$set_values(quantile_lower = 0.05, quantile_upper = 0.95)
  learner_wide$train(task)
  pred_wide <- learner_wide$predict(task)

  # Both learners should produce valid predictions with SE
  expect_true(all(pred_narrow$se >= 0))
  expect_true(all(pred_wide$se >= 0))
  expect_equal(length(pred_narrow$response), task$nrow)
  expect_equal(length(pred_wide$response), task$nrow)
})

test_that("LearnerRegrQuantileSE wrapped field provides access to base learner", {
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # $wrapped should return the base learner
  expect_r6(learner$wrapped, "LearnerRegr")
  expect_equal(learner$wrapped$id, "regr.mock_quantile")

  # $wrapped should be read-only
  expect_error(learner$wrapped <- make_mock_quantile_learner(), "read-only")
})

test_that("LearnerRegrQuantileSE param_set allows setting base learner params", {
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Should be able to set base learner params via wrapper's param_set
  learner$param_set$set_values(spread_multiplier = 5)

  # Setting via wrapper should affect wrapped learner
  expect_equal(learner$wrapped$param_set$values$spread_multiplier, 5)

  # Should also be able to set wrapper's own params
  learner$param_set$set_values(quantile_lower = 0.2)
  expect_equal(learner$param_set$values$quantile_lower, 0.2)
})

test_that("LearnerRegrQuantileSE param_set changes via wrapped learner are visible", {
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Set param directly on wrapped learner
  learner$wrapped$param_set$set_values(spread_multiplier = 3)

  # Should be visible via wrapper's param_set
  expect_equal(learner$param_set$values$spread_multiplier, 3)
})

test_that("LearnerRegrQuantileSE cloning keeps param sets independent", {
  base_learner <- make_mock_quantile_learner()
  original <- LearnerRegrQuantileSE$new(base_learner)
  original$param_set$set_values(spread_multiplier = 3)

  # Clone the learner
  clone <- original$clone(deep = TRUE)

  # Modify clone's params
  clone$param_set$set_values(spread_multiplier = 7, quantile_lower = 0.2, quantile_upper = 0.8)

  # Original should be unchanged
  expect_equal(original$param_set$values$spread_multiplier, 3)
  expect_equal(original$param_set$values$quantile_lower, 0.1)  # Default
  expect_equal(original$param_set$values$quantile_upper, 0.9)  # Default
  expect_equal(original$wrapped$param_set$values$spread_multiplier, 3)

  # Clone should have new values
  expect_equal(clone$param_set$values$spread_multiplier, 7)
  expect_equal(clone$param_set$values$quantile_lower, 0.2)
  expect_equal(clone$param_set$values$quantile_upper, 0.8)
  expect_equal(clone$wrapped$param_set$values$spread_multiplier, 7)
})

# ============================================================================
# Tests for specific behaviors requested by user
# ============================================================================

test_that("$wrapped learner is NOT trained after training wrapper", {
  task <- tsk("mtcars")
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Before training
  expect_null(learner$wrapped$state)
  expect_null(learner$wrapped$model)

  # Train wrapper
  learner$train(task)

  # After training: wrapper is trained but $wrapped is not
  expect_false(is.null(learner$model))
  expect_null(learner$wrapped$state)
  expect_null(learner$wrapped$model)
})

test_that("$wrapped is direct reference - param changes affect wrapper", {
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Changing $wrapped param_set should affect wrapper
  learner$wrapped$param_set$set_values(spread_multiplier = 7)
  expect_equal(learner$param_set$values$spread_multiplier, 7)

  # Vice versa
  learner$param_set$set_values(spread_multiplier = 9)
  expect_equal(learner$wrapped$param_set$values$spread_multiplier, 9)
})

test_that("$base_learner() IS trained and is a deep clone", {
  task <- tsk("mtcars")
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)
  learner$train(task)

  # Get trained base learner
  base_trained <- learner$base_learner()

  # Should be trained (has state)
  expect_false(is.null(base_trained$state))
  expect_false(is.null(base_trained$model))

  # Should be a clone - changing its params doesn't affect wrapper
  base_trained$param_set$set_values(spread_multiplier = 9)
  # Wrapper's param should be unaffected (NULL since not explicitly set)
  expect_true(is.null(learner$param_set$values$spread_multiplier))
})

test_that("Missing required parameters cause errors", {
  task <- tsk("mtcars")
  base_learner <- make_mock_quantile_learner()
  learner <- LearnerRegrQuantileSE$new(base_learner)

  # Remove a training param (quantile_lower) - should error during training
  learner$param_set$values$quantile_lower <- NULL
  expect_error(
    learner$train(task),
    "required"  # Paradox error for missing required param
  )

  # Reset and train successfully
  learner2 <- LearnerRegrQuantileSE$new(make_mock_quantile_learner())
  learner2$train(task)

  # Remove a predict param (se_factor) - should error during prediction
  learner2$param_set$values$se_factor <- NULL
  expect_error(
    learner2$predict(task),
    "required"  # Paradox error for missing required param
  )
})

test_that("SE calculation uses correct formula", {
  task <- tsk("mtcars")
  base_learner <- make_mock_quantile_learner()
  learner <- lrn("regr.quantile_se", learner = base_learner)

  # Set specific se_factor
  learner$param_set$set_values(se_factor = 0.75)
  learner$train(task)
  pred <- learner$predict(task)

  # Get quantiles directly
  base_trained <- learner$base_learner()
  base_trained$predict_type <- "quantiles"
  base_trained$quantiles <- c(0.1, 0.9)
  base_trained$quantile_response <- 0.5
  pred_q <- base_trained$predict(task)

  # Verify SE = (upper - lower) * se_factor
  expected_se <- (pred_q$quantiles[, 2] - pred_q$quantiles[, 1]) * 0.75
  expect_equal(pred$se, expected_se)
})

test_that("quantile_response controls response prediction", {
  task <- tsk("mtcars")

  # Learner with median response (default)
  learner_median <- LearnerRegrQuantileSE$new(make_mock_quantile_learner())
  learner_median$train(task)
  pred_median <- learner_median$predict(task)

  # Learner with 0.25 quantile as response
  learner_q25 <- LearnerRegrQuantileSE$new(make_mock_quantile_learner())
  learner_q25$param_set$set_values(quantile_response = 0.25)
  learner_q25$train(task)
  pred_q25 <- learner_q25$predict(task)

  # Responses should differ (unless by extreme coincidence)
  expect_false(isTRUE(all.equal(pred_median$response, pred_q25$response)))

  # Verify q25 response is lower than median (for this mock learner)
  expect_true(all(pred_q25$response <= pred_median$response))
})

test_that("LearnerRegrQuantileSE model has correct state class", {
  task <- tsk("mtcars")
  learner <- lrn("regr.quantile_se", learner = make_mock_quantile_learner())
  learner$train(task)

  # Model should have the state class
  expect_true(inherits(learner$model, "learner_regr_quantile_se_state"))
})

test_that("LearnerRegrQuantileSE marshaling works", {
  task <- tsk("mtcars")
  learner <- lrn("regr.quantile_se", learner = make_mock_quantile_learner())
  learner$train(task)

  # Get original predictions
  pred_original <- learner$predict(task)

  # Marshal the model
  model_marshaled <- marshal_model(learner$model)

  # Check if marshaling was applied (depends on base learner)
  # For mock learner, marshaling may or may not be needed
  if (is_marshaled_model(model_marshaled)) {
    expect_true(inherits(model_marshaled, "learner_regr_quantile_se_state_marshaled"))
    expect_true(inherits(model_marshaled, "marshaled"))

    # Unmarshal
    model_unmarshaled <- unmarshal_model(model_marshaled)
    expect_true(inherits(model_unmarshaled, "learner_regr_quantile_se_state"))
    expect_false(inherits(model_unmarshaled, "marshaled"))

    # Predictions should be identical after marshal/unmarshal
    learner$model <- model_unmarshaled
    pred_after <- learner$predict(task)
    expect_equal(pred_after$response, pred_original$response)
    expect_equal(pred_after$se, pred_original$se)
  } else {
    # Model didn't need marshaling
    expect_identical(model_marshaled, learner$model)
  }
})

test_that("LearnerRegrQuantileSE serialization with saveRDS/readRDS works", {
  task <- tsk("mtcars")
  # Use mock learner for testing serialization
  learner <- lrn("regr.quantile_se", learner = make_mock_quantile_learner())
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

test_that("LearnerRegrQuantileSE marshaling preserves state", {
  task <- tsk("mtcars")
  learner <- lrn("regr.quantile_se", learner = make_mock_quantile_learner())
  learner$param_set$set_values(
    quantile_lower = 0.2,
    quantile_upper = 0.8,
    quantile_response = 0.5,
    se_factor = 0.6
  )
  learner$train(task)

  # Original model should have correct quantiles
  expect_equal(learner$model$quantiles, c(0.2, 0.8))
  expect_equal(learner$model$quantile_response, 0.5)

  # Marshal the model
  model_marshaled <- marshal_model(learner$model)

  # Unmarshal
  model_unmarshaled <- unmarshal_model(model_marshaled)
  expect_equal(model_unmarshaled$quantiles, c(0.2, 0.8))
  expect_equal(model_unmarshaled$quantile_response, 0.5)

  # Predictions should work
  learner$model <- model_unmarshaled
  pred <- learner$predict(task)
  expect_equal(length(pred$response), task$nrow)
  expect_equal(length(pred$se), task$nrow)
})
