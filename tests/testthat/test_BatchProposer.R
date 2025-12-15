test_that("BatchProposer basic construction", {
  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 10L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 10L, k = 0L)

  proposer <- BatchProposer$new(optimizer = optimizer, terminator = terminator)

  expect_r6(proposer, "BatchProposer")
  expect_r6(proposer, "AcqOptimizer")
  expect_r6(proposer$optimizer, "OptimizerBatch")
  expect_r6(proposer$terminator, "Terminator")
  expect_true(is.numeric(proposer$pool_factor))
  expect_function(proposer$batch_strategy)
})

test_that("BatchProposer inherits AcqOptimizer parameters", {
  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 10L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 10L, k = 0L)

  proposer <- BatchProposer$new(optimizer = optimizer, terminator = terminator)

  param_ids <- proposer$param_set$ids()

  expect_true("n_candidates" %in% param_ids)
  expect_true("warmstart" %in% param_ids)
  expect_true("skip_already_evaluated" %in% param_ids)
  expect_true("catch_errors" %in% param_ids)
  expect_true("logging_level" %in% param_ids)
})

test_that("BatchProposer requires acq_function before optimize", {
  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 10L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 10L, k = 0L)

  proposer <- BatchProposer$new(optimizer = optimizer, terminator = terminator)

  expect_error(
    proposer$optimize(),
    "acq_function must be set"
  )
})

test_that("BatchProposer optimize returns correct number of candidates", {
  search_space <- ps(x = p_dbl(lower = -5, upper = 5))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = bbotk::TerminatorEvals$new()
  )
  instance$terminator$param_set$set_values(n_evals = 20L, k = 0L)

  instance$eval_batch(data.table(x = c(-3, -1, 0, 1, 3)))

  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.featureless"))
  learner$param_set$set_values(n_bootstrap = 3L)
  surrogate <- SurrogateLearner$new(learner = learner, archive = instance$archive)
  surrogate$update()

  acq_function <- AcqFunctionEI$new(surrogate = surrogate)
  acq_function$update()

  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 100L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 100L, k = 0L)

  proposer <- BatchProposer$new(
    optimizer = optimizer,
    terminator = terminator,
    acq_function = acq_function,
    batch_strategy = batch_strategy_greedy(),
    pool_factor = 10L
  )

  proposer$param_set$set_values(n_candidates = 3L)
  candidates <- proposer$optimize()

  expect_data_table(candidates, nrows = 3L)
  expect_true(all(c("x", acq_function$id) %in% names(candidates)))
  expect_numeric(candidates$x, lower = -5, upper = 5, any.missing = FALSE)
})

test_that("BatchProposer passes direction-adjusted scores to batch_strategy", {
  search_space <- ps(x = p_dbl(lower = -5, upper = 5))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = bbotk::TerminatorEvals$new()
  )
  instance$terminator$param_set$set_values(n_evals = 10L, k = 0L)

  instance$eval_batch(data.table(x = c(-2, 0, 2)))

  learner <- lrn("regr.bootstrap_se", learner = lrn("regr.featureless"))
  learner$param_set$set_values(n_bootstrap = 3L)
  surrogate <- SurrogateLearner$new(learner = learner, archive = instance$archive)
  surrogate$update()

  acq_function <- AcqFunctionEI$new(surrogate = surrogate)  # direction = maximize
  acq_function$update()

  captured <- new.env(parent = emptyenv())
  captured$scores <- NULL
  custom_strategy <- function(candidates, scores, batch_size, surrogate, archive, search_space) {
    captured$scores <- scores
    seq_len(batch_size)
  }

  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 100L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 100L, k = 0L)

  proposer <- BatchProposer$new(
    optimizer = optimizer,
    terminator = terminator,
    acq_function = acq_function,
    batch_strategy = custom_strategy,
    pool_factor = 2L
  )
  proposer$param_set$set_values(n_candidates = 2L)

  proposer$optimize()

  expect_true(!is.null(captured$scores))
  # EI is non-negative; with maximize direction, scores are negated so lower is better.
  expect_true(all(captured$scores <= 0))
})

test_that("BatchProposer print method works", {
  optimizer <- bbotk::OptimizerBatchRandomSearch$new()
  optimizer$param_set$set_values(batch_size = 10L)

  terminator <- bbotk::TerminatorEvals$new()
  terminator$param_set$set_values(n_evals = 10L, k = 0L)

  proposer <- BatchProposer$new(optimizer = optimizer, terminator = terminator)

  expect_output(print(proposer), "BatchProposer")
  expect_output(print(proposer), "Pool factor")
})
