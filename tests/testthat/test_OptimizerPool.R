test_that("OptimizerPool basic construction", {
  optimizer <- OptimizerPool$new()

  expect_r6(optimizer, "OptimizerPool")
  expect_r6(optimizer, "OptimizerBatch")
  expect_r6(optimizer, "Optimizer")

  expect_equal(optimizer$id, "pool")
  expect_equal(optimizer$label, "Pool-Based Optimizer")
  expect_set_equal(
    optimizer$param_set$ids(),
    c("candidate_generator", "pool_size")
  )
})

test_that("OptimizerPool parameters have correct defaults", {
  optimizer <- OptimizerPool$new()

  expect_function(optimizer$param_set$values$candidate_generator)
  expect_null(optimizer$param_set$values$pool_size)
})

test_that("OptimizerPool can change parameters", {
  optimizer <- OptimizerPool$new()

  random_gen <- candidate_generator_random()
  optimizer$param_set$set_values(
    candidate_generator = random_gen,
    pool_size = 100L
  )

  expect_identical(optimizer$param_set$values$candidate_generator, random_gen)
  expect_equal(optimizer$param_set$values$pool_size, 100L)
})

test_that("OptimizerPool works with simple optimization", {
  # Create simple objective
  search_space <- ps(x = p_dbl(lower = -5, upper = 5))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  # Create instance
  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 1)
  )

  # Create optimizer with small pool
  optimizer <- OptimizerPool$new()
  optimizer$param_set$set_values(
    candidate_generator = candidate_generator_random(),
    pool_size = 10L
  )

  # Optimize
  optimizer$optimize(instance)

  # Check results
  expect_equal(instance$archive$n_evals, 10L)
  expect_data_table(instance$archive$data, nrows = 10L)
  expect_names(names(instance$archive$data), must.include = c("x", "y"))
})

test_that("OptimizerPool calculates pool_size from terminator when not specified", {
  # Create simple objective
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  # Create instance with 15 max evaluations
  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 15)
  )

  # Create optimizer without explicit pool_size
  # Should use remaining evals from terminator (15 - 0 = 15)
  optimizer <- OptimizerPool$new()
  optimizer$param_set$set_values(
    candidate_generator = candidate_generator_random(),
    pool_size = NULL
  )

  # Optimize
  optimizer$optimize(instance)

  # Should generate all 15 candidates (terminator max - current evals)
  expect_equal(instance$archive$n_evals, 15L)
})

test_that("OptimizerPool works with different generators", {
  search_space <- ps(
    x1 = p_dbl(lower = -5, upper = 5),
    x2 = p_dbl(lower = -5, upper = 5)
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x1^2 + xs$x2^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  generators <- list(
    lhs = candidate_generator_lhs(),
    random = candidate_generator_random(),
    sobol = candidate_generator_sobol()
  )

  for (gen_name in names(generators)) {
    instance <- OptimInstanceBatchSingleCrit$new(
      objective = objective,
      search_space = search_space,
      terminator = trm("evals", n_evals = 1)
    )

    optimizer <- OptimizerPool$new()
    optimizer$param_set$set_values(
      candidate_generator = generators[[gen_name]],
      pool_size = 20L
    )

    optimizer$optimize(instance)

    expect_equal(instance$archive$n_evals, 20L, info = gen_name)
    expect_data_table(instance$archive$data, nrows = 20L, info = gen_name)
  }
})

test_that("opt('pool') dictionary accessor works", {
  optimizer <- opt("pool")

  expect_r6(optimizer, "OptimizerPool")
  expect_equal(optimizer$id, "pool")

  # Can set parameters after creation
  sobol_gen <- candidate_generator_sobol()
  optimizer$param_set$set_values(
    candidate_generator = sobol_gen,
    pool_size = 50L
  )

  expect_identical(optimizer$param_set$values$candidate_generator, sobol_gen)
  expect_equal(optimizer$param_set$values$pool_size, 50L)
})
