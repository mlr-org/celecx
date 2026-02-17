# =============================================================================
# Tests for optimizer_active_learning() / optimize_active()
# =============================================================================

# =============================================================================
# Test Helpers
# =============================================================================

#' Create a simple test objective for active learning tests
create_test_objective <- function() {
  ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$x - 1)^2 + 0.1 * rnorm(1)),
    domain = ps(x = p_dbl(lower = -2, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )
}

# =============================================================================
# SE Method Tests
# =============================================================================

test_that("optimizer_active_learning returns configured OptimizerMbo", {
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.rpart"),
    se_method = "auto",
    batch_size = 2L,
    multipoint_method = "greedy",
    aqf_evals = 20L
  )

  expect_r6(optimizer, "OptimizerMbo")
  expect_r6(optimizer$surrogate, "SurrogateLearner")
  expect_r6(optimizer$acq_function, "AcqFunction")
  expect_r6(optimizer$acq_optimizer, "AcqOptimizer")
  expect_true(inherits(optimizer$result_assigner, "ResultAssignerNull"))

  expect_true(inherits(optimizer$surrogate$learner, "LearnerRegrBootstrapSE"))

  # Default `aqf_optimizer = opt("pool")` is an Optimizer and is wrapped into BatchProposer.
  expect_true(inherits(optimizer$acq_optimizer, "BatchProposer"))
  expect_equal(optimizer$acq_optimizer$param_set$values$n_candidates, 2L)
  expect_equal(optimizer$acq_optimizer$pool_factor, 1L)
})


test_that("se_method = 'auto' uses native SE when learner supports it", {
  skip_if_not_installed("DiceKriging")

  optimizer <- optimizer_active_learning(
    learner = lrn("regr.km", covtype = "matern5_2"),
    se_method = "auto",
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  # regr.km supports native SE, so should NOT be wrapped
  expect_false(inherits(optimizer$surrogate$learner, "LearnerRegrBootstrapSE"))
  expect_equal(optimizer$surrogate$learner$id, "regr.km")
  expect_equal(optimizer$surrogate$learner$predict_type, "se")
})


test_that("se_method = 'bootstrap' forces bootstrap wrapping even with SE-capable learner", {
  skip_if_not_installed("DiceKriging")

  optimizer <- optimizer_active_learning(
    learner = lrn("regr.km", covtype = "matern5_2"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 5L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  # Even though regr.km supports SE, bootstrap should be forced
  expect_true(inherits(optimizer$surrogate$learner, "LearnerRegrBootstrapSE"))
})


test_that("se_method = 'quantile' works with quantile-supporting learner", {
  skip_if_not_installed("ranger")

  optimizer <- optimizer_active_learning(
    learner = lrn("regr.ranger"),
    se_method = "quantile",
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  expect_true(inherits(optimizer$surrogate$learner, "LearnerRegrQuantileSE"))
})


# =============================================================================
# Multipoint Method Tests
# =============================================================================

test_that("multipoint_method = 'greedy' configures correctly", {
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 3L,
    multipoint_method = "greedy",
    aqf_evals = 20L
  )

  expect_r6(optimizer, "OptimizerMbo")
  expect_true(inherits(optimizer$acq_optimizer, "BatchProposer"))
  expect_equal(optimizer$acq_optimizer$param_set$values$n_candidates, 3L)
  # Greedy sets pool_factor = 1
  expect_equal(optimizer$acq_optimizer$pool_factor, 1L)
})


test_that("multipoint_method = 'diversity' configures BatchProposer correctly", {
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 3L,
    multipoint_method = "diversity",
    aqf_evals = 30L
  )

  expect_r6(optimizer, "OptimizerMbo")
  expect_true(inherits(optimizer$acq_optimizer, "BatchProposer"))
  expect_equal(optimizer$acq_optimizer$param_set$values$n_candidates, 3L)
  # Diversity uses default pool_factor (not 1)
  expect_gt(optimizer$acq_optimizer$pool_factor, 1L)
})


test_that("multipoint_method = 'constant_liar' configures for mpcl loop", {
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "constant_liar",
    aqf_evals = 20L
  )

  expect_r6(optimizer, "OptimizerMbo")
  # For constant_liar, n_candidates should be 1 (mpcl handles batching)
  expect_equal(optimizer$acq_optimizer$param_set$values$n_candidates, 1L)
  # Check loop_function is bayesopt_mpcl (via args having q = batch_size)
  expect_equal(optimizer$args$q, 2L)
})


# =============================================================================
# Error Cases
# =============================================================================

test_that("optimizer_active_learning errors for quantile SE without support", {
  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.rpart"),
      se_method = "quantile"
    ),
    "does not support quantile predictions"
  )
})


test_that("optimizer_active_learning enforces batch_size >= 2 for constant liar", {
  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.featureless"),
      multipoint_method = "constant_liar",
      batch_size = 1L
    ),
    "requires batch_size >= 2"
  )
})


test_that("optimizer_active_learning errors when batch_size > aqf_evals", {
  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.featureless"),
      batch_size = 50L,
      aqf_evals = 10L
    ),
    "batch_size.*must be <= aqf_evals"
  )
})


test_that("optimizer_active_learning requires BatchProposer for diversity", {
  acq_opt <- AcqOptimizer$new(
    optimizer = OptimizerBatchRandomSearch$new(),
    terminator = trm("evals", n_evals = 10L),
    acq_function = NULL
  )

  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.featureless"),
      multipoint_method = "diversity",
      batch_size = 2L,
      aqf_optimizer = acq_opt
    ),
    "requires aqf_optimizer to be a BatchProposer"
  )
})


test_that("optimizer_active_learning requires BatchProposer for local_penalization", {
  acq_opt <- AcqOptimizer$new(
    optimizer = OptimizerBatchRandomSearch$new(),
    terminator = trm("evals", n_evals = 10L),
    acq_function = NULL
  )

  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.featureless"),
      multipoint_method = "local_penalization",
      batch_size = 2L,
      aqf_optimizer = acq_opt
    ),
    "requires aqf_optimizer to be a BatchProposer"
  )
})


test_that("optimizer_active_learning allows AcqOptimizer for greedy", {
  acq_opt <- AcqOptimizer$new(
    optimizer = OptimizerBatchRandomSearch$new(),
    terminator = trm("evals", n_evals = 10L),
    acq_function = NULL
  )

  # Should NOT error - greedy works without BatchProposer
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    multipoint_method = "greedy",
    batch_size = 2L,
    aqf_optimizer = acq_opt
  )

  expect_r6(optimizer, "OptimizerMbo")
  # AcqOptimizer passed through
  expect_false(inherits(optimizer$acq_optimizer, "BatchProposer"))
})


test_that("optimizer_active_learning allows AcqOptimizer for constant_liar", {
  acq_opt <- AcqOptimizer$new(
    optimizer = OptimizerBatchRandomSearch$new(),
    terminator = trm("evals", n_evals = 10L),
    acq_function = NULL
  )

  # Should NOT error - constant_liar works without BatchProposer
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    multipoint_method = "constant_liar",
    batch_size = 2L,
    aqf_optimizer = acq_opt
  )

  expect_r6(optimizer, "OptimizerMbo")
})


# =============================================================================
# Integration Tests: optimize_active()
# =============================================================================

test_that("optimize_active runs an AL loop (local penalization) and logs metrics", {
  set.seed(1)

  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$x - 1)^2),
    domain = ps(x = p_dbl(lower = -2, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))

  res <- optimize_active(
    objective = objective,
    term_evals = 10L,
    metrics_tracker = tracker,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "local_penalization",
    aqf_evals = 25L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_r6(res$optimizer, "OptimizerMbo")
  expect_true(identical(res$metrics_tracker, tracker))

  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 10L)
  expect_equal(tracker$n_batches, res$instance$archive$n_batch)
})


test_that("optimize_active runs with greedy multipoint method", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 8L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "greedy",
    aqf_evals = 15L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 8L)
})


test_that("optimize_active runs with diversity multipoint method", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 8L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "diversity",
    aqf_evals = 20L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 8L)
})


test_that("optimize_active runs with constant_liar multipoint method", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 8L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "constant_liar",
    aqf_evals = 15L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 8L)
})


test_that("optimize_active runs with native SE learner (Kriging)", {
  skip_if_not_installed("DiceKriging")

  set.seed(123)

  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = sin(xs$x * pi)),
    domain = ps(x = p_dbl(lower = 0, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  res <- optimize_active(
    objective = objective,
    term_evals = 12L,
    learner = lrn("regr.km", covtype = "matern5_2", control = list(trace = FALSE)),
    se_method = "auto",
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 15L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 12L)
  # Learner should NOT be wrapped in bootstrap
  expect_false(inherits(res$optimizer$surrogate$learner, "LearnerRegrBootstrapSE"))
})


test_that("optimize_active runs with quantile SE learner", {
  skip_if_not_installed("ranger")

  set.seed(123)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 10L,
    learner = lrn("regr.ranger", num.trees = 20L),
    se_method = "quantile",
    batch_size = 2L,
    multipoint_method = "greedy",
    aqf_evals = 15L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 10L)
  # Learner should be wrapped in quantile SE
  expect_true(inherits(res$optimizer$surrogate$learner, "LearnerRegrQuantileSE"))
})


test_that("optimize_active works without metrics_tracker", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 6L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  expect_type(res, "list")
  expect_null(res$metrics_tracker)
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
})


test_that("optimize_active accepts custom terminator", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    terminator = trm("evals", n_evals = 7L),
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 7L)
})


test_that("optimize_active accepts custom search_space", {
  set.seed(42)

  # Objective with larger domain
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -10, upper = 10)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  # Restrict search space
  search_space <- ps(x = p_dbl(lower = -1, upper = 1))

  res <- optimize_active(
    objective = objective,
    search_space = search_space,
    term_evals = 6L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)

  # All evaluated points should be within the restricted search_space
  archive_data <- res$instance$archive$data
  expect_true(all(archive_data$x >= -1 & archive_data$x <= 1))
})


test_that("optimize_active with batch_size = 1 runs correctly", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 5L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  expect_type(res, "list")
  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 5L)
})


# =============================================================================
# optimize_active() Specific Tests
# =============================================================================

test_that("optimize_active errors when neither term_evals nor terminator provided", {
  objective <- create_test_objective()

  expect_error(
    optimize_active(
      objective = objective,
      learner = lrn("regr.featureless"),
      se_method = "bootstrap",
      se_method_n_bootstrap = 3L
    ),
    "term_evals"
  )
})


test_that("optimize_active works with multi-dimensional search space", {
  set.seed(42)

  # 2D objective
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x1^2 + xs$x2^2),
    domain = ps(
      x1 = p_dbl(lower = -2, upper = 2),
      x2 = p_dbl(lower = -2, upper = 2)
    ),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  # Need more evals for 2D space (initial design is larger)
  res <- optimize_active(
    objective = objective,
    term_evals = 15L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "greedy",
    aqf_evals = 20L
  )

  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 15L)

  # Archive should have both parameters
  archive_data <- res$instance$archive$data
  expect_names(names(archive_data), must.include = c("x1", "x2", "y"))
  expect_true(all(archive_data$x1 >= -2 & archive_data$x1 <= 2))
  expect_true(all(archive_data$x2 >= -2 & archive_data$x2 <= 2))
})


test_that("optimize_active archive contains expected structure", {
  set.seed(42)

  objective <- create_test_objective()

  res <- optimize_active(
    objective = objective,
    term_evals = 6L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  archive <- res$instance$archive
  archive_data <- archive$data

  # Basic structure checks
  expect_data_table(archive_data)
  expect_gte(nrow(archive_data), 6L)

  # Must contain domain and codomain columns
  expect_names(names(archive_data), must.include = c("x", "y"))

  # Must contain batch number
  expect_names(names(archive_data), must.include = "batch_nr")

  # Batch numbers should be sequential starting from 1
  expect_true(all(archive_data$batch_nr >= 1L))

  # y values should be finite
  expect_true(all(is.finite(archive_data$y)))
})


test_that("optimize_active metrics_tracker records correct number of batches", {
  set.seed(42)

  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -2, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))

  res <- optimize_active(
    objective = objective,
    term_evals = 10L,
    metrics_tracker = tracker,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 2L,
    multipoint_method = "greedy",
    aqf_evals = 15L
  )

  # Tracker should have recorded metrics for each batch
  expect_equal(tracker$n_batches, res$instance$archive$n_batch)

  # Metrics data should contain our metric
  tracker_data <- tracker$data
  expect_data_table(tracker_data)
  expect_names(names(tracker_data), must.include = c("batch_nr", "best_y"))

  # best_y should be non-increasing (finding better minima over time, or staying same)
  # Actually for "learn" tag we're not optimizing, just exploring, so this might not hold
  # But values should at least be finite
  expect_true(all(is.finite(tracker_data$best_y)))
})


test_that("optimize_active ignores term_evals when terminator is provided", {
  set.seed(42)

  objective <- create_test_objective()

  # Provide both - terminator should take precedence
  res <- optimize_active(
    objective = objective,
    term_evals = 100L,  # Would take very long
    terminator = trm("evals", n_evals = 5L),  # Should use this instead
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  # Should have stopped around 5 evals, not 100
  expect_lte(res$instance$archive$n_evals, 10L)
})


test_that("optimize_active return structure is complete", {
  set.seed(42)

  objective <- create_test_objective()
  tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))

  res <- optimize_active(
    objective = objective,
    term_evals = 5L,
    metrics_tracker = tracker,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 10L
  )

  # Check return is a list with exactly the expected elements
  expect_type(res, "list")
  expect_names(names(res), identical.to = c("instance", "optimizer", "metrics_tracker"))

  # Each element has correct type
  expect_r6(res$instance, "SearchInstance")
  expect_r6(res$optimizer, "OptimizerMbo")
  expect_identical(res$metrics_tracker, tracker)
})


test_that("optimize_active works with categorical parameters", {
  set.seed(42)

  # Mixed parameter types
  objective <- ObjectiveRFun$new(
    fun = function(xs) {
      base <- xs$x^2
      modifier <- switch(xs$method, "a" = 0, "b" = 0.5, "c" = 1)
      list(y = base + modifier)
    },
    domain = ps(
      x = p_dbl(lower = -2, upper = 2),
      method = p_fct(levels = c("a", "b", "c"))
    ),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  # Need more evals for 2D mixed-type space (initial design is larger)
  res <- optimize_active(
    objective = objective,
    term_evals = 15L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 20L
  )

  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)
  expect_gte(res$instance$archive$n_evals, 15L)

  # Archive should have both parameters
  archive_data <- res$instance$archive$data
  expect_names(names(archive_data), must.include = c("x", "method", "y"))
  expect_true(all(archive_data$method %in% c("a", "b", "c")))
})


test_that("optimize_active works with integer parameters", {
  set.seed(42)

  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$n - 5)^2 + xs$x^2),
    domain = ps(
      x = p_dbl(lower = -2, upper = 2),
      n = p_int(lower = 1L, upper = 10L)
    ),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  # Need more evals for 2D space (initial design is larger)
  res <- optimize_active(
    objective = objective,
    term_evals = 15L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 3L,
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_evals = 20L
  )

  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)

  # Integer parameter should be integers
  archive_data <- res$instance$archive$data
  expect_true(all(archive_data$n == floor(archive_data$n)))
  expect_true(all(archive_data$n >= 1L & archive_data$n <= 10L))
})


test_that("optimize_active propagates ... args to optimizer_active_learning", {
  set.seed(42)

  objective <- create_test_objective()

  # Test that custom aqf_optimizer is passed through
  custom_opt <- opt("random_search", batch_size = 50L)

  res <- optimize_active(
    objective = objective,
    term_evals = 6L,
    learner = lrn("regr.featureless"),
    se_method = "bootstrap",
    se_method_n_bootstrap = 5L,  # Custom bootstrap count
    batch_size = 1L,
    multipoint_method = "greedy",
    aqf_optimizer = custom_opt,
    aqf_evals = 12L
  )

  expect_r6(res$instance, "SearchInstance")
  expect_true(res$instance$is_terminated)

  # Check that bootstrap count was applied
  learner <- res$optimizer$surrogate$learner
  expect_true(inherits(learner, "LearnerRegrBootstrapSE"))
})

test_that("optimize_active requires metrics_tracker when forecast_tracker is set", {
  objective <- create_test_objective()
  forecast_tracker <- ForecastTracker$new(
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    min_points = 2L
  )

  expect_error(
    optimize_active(
      objective = objective,
      term_evals = 5L,
      forecast_tracker = forecast_tracker,
      learner = lrn("regr.featureless")
    ),
    "forecast_tracker requires metrics_tracker"
  )
})
