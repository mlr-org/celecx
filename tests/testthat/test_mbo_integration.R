# =============================================================================
# Tests for mlr3mbo integration without SearchLoop
# =============================================================================

create_objective_quadratic_1d <- function() {
  ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$x - 2)^2),
    domain = ps(x = p_dbl(lower = -5, upper = 5)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
}

create_surrogate_bootstrap_se <- function(archive) {
  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.featureless"))
  learner$param_set$set_values(n_bootstrap = 3L)
  SurrogateLearner$new(learner = learner, archive = archive)
}

create_acq_optimizer_batch_proposer <- function(acq_function,
    batch_size = 1L,
    pool_factor = 10L,
    batch_strategy = batch_strategy_greedy()) {

  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 100L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 100L, k = 0L)

  proposer <- BatchProposer$new(
    optimizer = optimizer,
    terminator = terminator,
    acq_function = acq_function,
    batch_strategy = batch_strategy,
    pool_factor = pool_factor
  )
  proposer$param_set$set_values(n_candidates = as.integer(batch_size))
  proposer
}


test_that("CallbackMetricsTracker logs one row per evaluated batch (OptimizerMbo)", {
  objective <- create_objective_quadratic_1d()
  tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))

  callback <- CallbackMetricsTracker$new(metrics_tracker = tracker)

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 12L),
    callbacks = list(callback)
  )

  surrogate <- create_surrogate_bootstrap_se(instance$archive)
  acq_function <- AcqFunctionEI$new(surrogate = surrogate)
  proposer <- create_acq_optimizer_batch_proposer(
    acq_function = acq_function,
    batch_size = 1L,
    pool_factor = 1L,
    batch_strategy = batch_strategy_greedy()
  )

  optimizer <- OptimizerMbo$new(
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = proposer,
    args = list(init_design_size = 3L)
  )

  optimizer$optimize(instance)

  expect_true(instance$is_terminated)
  expect_gte(instance$archive$n_evals, 12L)

  # One log row per batch evaluation.
  expect_equal(tracker$n_batches, instance$archive$n_batch)
  expect_data_table(tracker$data, nrows = instance$archive$n_batch)
  expect_names(names(tracker$data), must.include = c("batch_nr", "n_evals", "best_y"))
})


test_that("ResultAssignerNull avoids archive$best() errors for learn codomains", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -2, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  make_instance <- function(result_assigner = NULL) {
    instance <- OptimInstanceBatchSingleCrit$new(
      objective = objective,
      terminator = trm("evals", n_evals = 8L)
    )
    surrogate <- create_surrogate_bootstrap_se(instance$archive)
    acq_function <- AcqFunctionSD$new(surrogate = surrogate)
    proposer <- create_acq_optimizer_batch_proposer(
      acq_function = acq_function,
      batch_size = 2L,
      pool_factor = 4L,
      batch_strategy = batch_strategy_local_penalization(bandwidth = 0.1, penalization = Inf)
    )
    optimizer <- OptimizerMbo$new(
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = proposer,
      args = list(init_design_size = 2L),
      result_assigner = result_assigner
    )
    list(instance = instance, optimizer = optimizer)
  }

  run_default <- make_instance()
  expect_error(
    run_default$optimizer$optimize(run_default$instance),
    "Cannot determine best points"
  )

  run_null <- make_instance(result_assigner = ResultAssignerNull$new())
  run_null$optimizer$optimize(run_null$instance)

  expect_true(run_null$instance$is_terminated)
  expect_gte(run_null$instance$archive$n_evals, 8L)
})


test_that("OptimizerMbo works with non-greedy BatchProposer strategy", {
  objective <- create_objective_quadratic_1d()

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 15L)
  )

  surrogate <- create_surrogate_bootstrap_se(instance$archive)
  acq_function <- AcqFunctionCB$new(surrogate = surrogate, lambda = 2)
  proposer <- create_acq_optimizer_batch_proposer(
    acq_function = acq_function,
    batch_size = 3L,
    pool_factor = 4L,
    batch_strategy = batch_strategy_diversity(diversity_weight = 0.5)
  )

  optimizer <- OptimizerMbo$new(
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = proposer,
    args = list(init_design_size = 3L)
  )

  optimizer$optimize(instance)

  expect_true(instance$is_terminated)
  expect_gte(instance$archive$n_evals, 15L)
})

