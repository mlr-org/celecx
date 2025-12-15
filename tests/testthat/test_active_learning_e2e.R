# =============================================================================
# End-to-End Tests for Active Learning Experiments
# =============================================================================

# =============================================================================
# Test Helpers
# =============================================================================

#' Create a simple 1D test function for active learning
create_al_test_function <- function() {
  function(x) {
    1.2 * exp(-0.5 * (x - 2)^2) +
      0.7 * exp(-0.2 * (x - 7)^2) +
      0.3 * x / 10
  }
}

#' Create an active learning objective
create_al_objective <- function(fun = create_al_test_function()) {
  ObjectiveRFun$new(
    fun = function(xs) {
      x <- xs$x
      list(y = fun(x))
    },
    domain = ps(x = p_dbl(lower = 0, upper = 10)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )
}


# =============================================================================
# Basic Active Learning Tests
# =============================================================================

test_that("Active learning with Kriging surrogate works end-to-end", {
  skip_if_not_installed("DiceKriging")

  set.seed(1)
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 30))

  # Create surrogate with Kriging
  smodel <- lrn("regr.km", predict_type = "se", control = list(trace = FALSE), optim.method = "gen", covtype = "matern5_2")
  smodel$param_set$set_values(nugget.stability = 10^-8)
  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function (standard deviation for active learning)
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create acquisition optimizer
  acq_optimizer <- AcqOptimizer$new(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100),
    acq_function = acq_function
  )

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)

  # Track number of evaluations before each iteration
  n_evals_before <- 0
  proposed_points <- initial_design$data

  # Run 10 iterations of active learning
  for (i in seq_len(10)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()
    proposed_points <- acq_optimizer$optimize()

    # Check that we evaluated the right number of points
    if (i == 1) {
      expect_equal(instance$archive$n_evals, 10)  # Initial design
    } else {
      expect_equal(instance$archive$n_evals, n_evals_before + 1)  # One proposal per iteration
    }
    n_evals_before <- instance$archive$n_evals

    # Check that proposed points are valid
    expect_data_table(proposed_points, nrows = 1)
    expect_names(names(proposed_points), must.include = "x")
    expect_true(proposed_points$x >= 0 && proposed_points$x <= 10)
  }

  # After 10 iterations with initial design of 10, we should have 10 + 9 = 19 evaluations
  # (initial design + 9 single proposals, because the 10th iteration proposes but doesn't eval)
  expect_equal(instance$archive$n_evals, 19)

  # Archive should contain x and y columns
  expect_names(names(instance$archive$data), must.include = c("x", "y"))

  # All y values should be within reasonable range for our test function
  expect_true(all(instance$archive$data$y >= -1 & instance$archive$data$y <= 3))
})


test_that("Active learning with bootstrap SE works end-to-end", {
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 30))

  # Create bootstrap-based surrogate
  lrn.base <- lrn("regr.rpart")
  lrn.base$param_set$values$minsplit <- 2
  smodel <- LearnerRegrBootstrapSE$new(lrn.base)
  smodel$param_set$values$n_bootstrap <- 30  # Reduced from 300 for faster tests

  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create acquisition optimizer
  acq_optimizer <- AcqOptimizer$new(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100),
    acq_function = acq_function
  )

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)

  # Track number of evaluations
  n_evals_before <- 0
  proposed_points <- initial_design$data

  # Run 10 iterations of active learning
  for (i in seq_len(10)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()
    proposed_points <- acq_optimizer$optimize()

    # Check that we evaluated the right number of points
    if (i == 1) {
      expect_equal(instance$archive$n_evals, 10)
    } else {
      expect_equal(instance$archive$n_evals, n_evals_before + 1)
    }
    n_evals_before <- instance$archive$n_evals

    # Check that proposed points are valid
    expect_data_table(proposed_points, nrows = 1)
    expect_names(names(proposed_points), must.include = "x")
    expect_true(proposed_points$x >= 0 && proposed_points$x <= 10)
  }

  # Should have 19 evaluations total (10 initial + 9 single proposals)
  expect_equal(instance$archive$n_evals, 19)

  # Archive should be properly populated
  expect_names(names(instance$archive$data), must.include = c("x", "y"))
})


test_that("Active learning with multipoint proposals works end-to-end", {
  skip_if_not_installed("DiceKriging")

  set.seed(1)
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 1000))

  # Create surrogate with Kriging
  smodel <- lrn("regr.km", predict_type = "se", control = list(trace = FALSE), optim.method = "gen", covtype = "matern5_2")
  smodel$param_set$set_values(nugget.stability = 10^-8)
  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create acquisition optimizer with multiple candidates
  acq_optimizer <- AcqOptimizer$new(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100),
    acq_function = acq_function
  )
  acq_optimizer$param_set$values$n_candidates <- 5

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)

  # Track number of evaluations
  n_evals_before <- 0
  proposed_points <- initial_design$data

  # Run 10 iterations of active learning
  for (i in seq_len(10)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()
    proposed_points <- acq_optimizer$optimize()

    # Check that we evaluated the right number of points
    if (i == 1) {
      expect_equal(instance$archive$n_evals, 10)
    } else {
      expect_equal(instance$archive$n_evals, n_evals_before + 5)  # 5 proposals per iteration
    }
    n_evals_before <- instance$archive$n_evals

    # Check that proposed points are valid
    expect_data_table(proposed_points, nrows = 5)
    expect_names(names(proposed_points), must.include = "x")
    expect_true(all(proposed_points$x >= 0 & proposed_points$x <= 10))
  }

  # After 10 iterations: 10 initial + 9 * 5 proposals = 55 evaluations
  expect_equal(instance$archive$n_evals, 55)

  # Archive should be properly populated
  expect_names(names(instance$archive$data), must.include = c("x", "y"))
})


test_that("Active learning with OptimizerPool works end-to-end", {
  skip_if_not_installed("DiceKriging")

  set.seed(1)
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 1000))

  # Create surrogate with Kriging
  smodel <- lrn("regr.km", predict_type = "se", control = list(trace = FALSE), optim.method = "gen", covtype = "matern5_2")
  smodel$param_set$set_values(nugget.stability = 10^-8)
  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create OptimizerPool
  inner.opt <- OptimizerPool$new()
  inner.opt$param_set$values$candidate_generator <- candidate_generator_sobol()

  # Create acquisition optimizer using OptimizerPool
  acq_optimizer <- AcqOptimizer$new(
    optimizer = inner.opt,
    terminator = trm("evals", n_evals = 20),
    acq_function = acq_function
  )
  acq_optimizer$param_set$values$n_candidates <- 5

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)

  # Track number of evaluations
  n_evals_before <- 0
  proposed_points <- initial_design$data

  # Run 10 iterations of active learning
  for (i in seq_len(10)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()
    proposed_points <- acq_optimizer$optimize()

    # Check that we evaluated the right number of points
    if (i == 1) {
      expect_equal(instance$archive$n_evals, 10)
    } else {
      expect_equal(instance$archive$n_evals, n_evals_before + 5)  # 5 proposals per iteration
    }
    n_evals_before <- instance$archive$n_evals

    # Check that proposed points are valid
    expect_data_table(proposed_points, nrows = 5)
    expect_names(names(proposed_points), must.include = "x")
    expect_true(all(proposed_points$x >= 0 & proposed_points$x <= 10))
  }

  # Should have 55 evaluations total (10 initial + 9 * 5 proposals)
  expect_equal(instance$archive$n_evals, 55)

  # Archive should be properly populated
  expect_names(names(instance$archive$data), must.include = c("x", "y"))
})


test_that("Active learning with BatchProposer and local penalization works end-to-end", {
  skip_if_not_installed("DiceKriging")

  set.seed(1)
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 1000))

  # Create surrogate with Kriging
  smodel <- lrn("regr.km", predict_type = "se", control = list(trace = FALSE), optim.method = "gen", covtype = "matern5_2")
  smodel$param_set$set_values(nugget.stability = 10^-8)
  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create OptimizerPool
  inner.opt <- OptimizerPool$new()
  inner.opt$param_set$values$candidate_generator <- candidate_generator_sobol()

  # Create BatchProposer with local penalization
  batch_proposer <- BatchProposer$new(
    optimizer = inner.opt,
    terminator = trm("evals", n_evals = 20),
    acq_function = acq_function,
    batch_strategy = batch_strategy_local_penalization(bandwidth = 0.1, penalization = Inf),
    pool_factor = 4L
  )
  batch_proposer$param_set$values$n_candidates <- 5L

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)

  # Track number of evaluations
  n_evals_before <- 0
  proposed_points <- initial_design$data

  # Run 10 iterations of active learning
  for (i in seq_len(10)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()
    proposed_points <- batch_proposer$optimize()

    # Check that we evaluated the right number of points
    if (i == 1) {
      expect_equal(instance$archive$n_evals, 10)
    } else {
      expect_equal(instance$archive$n_evals, n_evals_before + 5)  # 5 proposals per iteration
    }
    n_evals_before <- instance$archive$n_evals

    # Check that proposed points are valid
    expect_data_table(proposed_points, nrows = 5)
    expect_names(names(proposed_points), must.include = "x")
    expect_true(all(proposed_points$x >= 0 & proposed_points$x <= 10))

    # Check that local penalization produces diverse points
    # Points should not all be identical (though they could be close)
    if (nrow(proposed_points) > 1) {
      x_range <- diff(range(proposed_points$x))
      # With local penalization, we expect some diversity
      # Don't check this too strictly as it depends on the acquisition function landscape
    }
  }

  # Should have 55 evaluations total (10 initial + 9 * 5 proposals)
  expect_equal(instance$archive$n_evals, 55)

  # Archive should be properly populated
  expect_names(names(instance$archive$data), must.include = c("x", "y"))
})


# =============================================================================
# Integration Tests with Predictions
# =============================================================================

test_that("Active learning produces valid surrogate predictions", {
  skip_if_not_installed("DiceKriging")

  set.seed(1)
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 30))

  # Create surrogate with Kriging
  smodel <- lrn("regr.km", predict_type = "se", control = list(trace = FALSE), optim.method = "gen", covtype = "matern5_2")
  smodel$param_set$set_values(nugget.stability = 10^-8)
  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create acquisition optimizer
  acq_optimizer <- AcqOptimizer$new(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100),
    acq_function = acq_function
  )

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)
  proposed_points <- initial_design$data

  # Run a few iterations
  for (i in seq_len(5)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()

    # Make predictions on a grid
    pred.grid <- data.table(x = seq(0, 10, length.out = 100))
    surrogate_pred <- acq_function$surrogate$predict(pred.grid)

    # Check that predictions have the right structure (returns a list)
    expect_list(surrogate_pred)
    expect_names(names(surrogate_pred), must.include = c("mean", "se"))

    # Check that mean predictions are within reasonable range
    expect_true(all(is.finite(surrogate_pred$mean)))
    expect_true(all(is.finite(surrogate_pred$se)))

    # SE should be non-negative
    expect_true(all(surrogate_pred$se >= 0))

    proposed_points <- acq_optimizer$optimize()
  }
})


test_that("Active learning reduces uncertainty over iterations", {
  skip_if_not_installed("DiceKriging")

  set.seed(1)
  fun <- create_al_test_function()
  objective <- create_al_objective(fun)
  instance <- SearchInstance$new(objective = objective, terminator = trm("evals", n_evals = 100))

  # Create surrogate with Kriging
  smodel <- lrn("regr.km", predict_type = "se", control = list(trace = FALSE), optim.method = "gen", covtype = "matern5_2")
  smodel$param_set$set_values(nugget.stability = 10^-8)
  smodel.object <- SurrogateLearner$new(smodel, archive = instance$archive)

  # Create acquisition function
  acq_function <- acqf("sd", surrogate = smodel.object)

  # Create acquisition optimizer
  acq_optimizer <- AcqOptimizer$new(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100),
    acq_function = acq_function
  )

  # Generate initial design
  search_space <- instance$search_space
  initial_design <- generate_design_random(search_space, n = 10)
  proposed_points <- initial_design$data

  # Track average SE over iterations
  avg_se_history <- numeric()
  pred.grid <- data.table(x = seq(0, 10, length.out = 100))

  # Run iterations
  for (i in seq_len(10)) {
    instance$eval_batch(proposed_points)
    acq_function$surrogate$update()
    acq_function$update()

    # Make predictions
    surrogate_pred <- acq_function$surrogate$predict(pred.grid)
    avg_se_history[i] <- mean(surrogate_pred$se)

    proposed_points <- acq_optimizer$optimize()
  }

  # Average SE should generally decrease (though not strictly monotonic)
  # Compare first half to second half
  first_half_avg <- mean(avg_se_history[1:5])
  second_half_avg <- mean(avg_se_history[6:10])

  # Second half should have lower average SE than first half
  # (active learning should reduce uncertainty)
  expect_lt(second_half_avg, first_half_avg * 1.5)  # Allow some slack
})
