test_that("OptimizerSample basic construction", {
  optimizer <- OptimizerSample$new()

  expect_r6(optimizer, "OptimizerSample")
  expect_r6(optimizer, "OptimizerBatch")
  expect_r6(optimizer, "Optimizer")

  expect_equal(optimizer$id, "sample")
  expect_equal(optimizer$label, "Sample-Based Optimizer")
  expect_set_equal(
    optimizer$param_set$ids(),
    c("candidate_generator", "sample_size")
  )
})

test_that("OptimizerSample parameters have correct defaults", {
  optimizer <- OptimizerSample$new()

  expect_function(optimizer$param_set$values$candidate_generator)
  expect_null(optimizer$param_set$values$sample_size)
})

test_that("OptimizerSample can change parameters", {
  optimizer <- OptimizerSample$new()

  random_gen <- candidate_generator_random()
  optimizer$param_set$set_values(
    candidate_generator = random_gen,
    sample_size = 100L
  )

  expect_identical(optimizer$param_set$values$candidate_generator, random_gen)
  expect_equal(optimizer$param_set$values$sample_size, 100L)
})

test_that("OptimizerSample works with simple optimization", {
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

  # Create optimizer with small sample
  optimizer <- OptimizerSample$new()
  optimizer$param_set$set_values(
    candidate_generator = candidate_generator_random(),
    sample_size = 10L
  )

  # Optimize
  optimizer$optimize(instance)

  # Check results
  expect_equal(instance$archive$n_evals, 10L)
  expect_data_table(instance$archive$data, nrows = 10L)
  expect_names(names(instance$archive$data), must.include = c("x", "y"))
})

test_that("OptimizerSample calculates sample_size from terminator when not specified", {
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

  # Create optimizer without explicit sample_size
  # Should use remaining evals from terminator (15 - 0 = 15)
  optimizer <- OptimizerSample$new()
  optimizer$param_set$set_values(
    candidate_generator = candidate_generator_random(),
    sample_size = NULL
  )

  # Optimize
  optimizer$optimize(instance)

  # Should generate all 15 candidates (terminator max - current evals)
  expect_equal(instance$archive$n_evals, 15L)
})

test_that("OptimizerSample works with different generators", {
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

    optimizer <- OptimizerSample$new()
    optimizer$param_set$set_values(
      candidate_generator = generators[[gen_name]],
      sample_size = 20L
    )

    optimizer$optimize(instance)

    expect_equal(instance$archive$n_evals, 20L, info = gen_name)
    expect_data_table(instance$archive$data, nrows = 20L, info = gen_name)
  }
})

test_that("OptimizerSample works with extra trafos that create ParamUty objective inputs", {
  search_space <- ps(
    x = p_int(lower = 1L, upper = 3L),
    .extra_trafo = function(x) {
      list(
        x = x$x,
        payload = list(x$x^2)
      )
    }
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) {
      list(y = xs$payload[[1L]])
    },
    domain = ps(
      x = p_int(lower = 1L, upper = 3L),
      payload = p_uty()
    ),
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = TRUE
  )
  instance <- SearchInstance$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 3L),
    check_values = TRUE
  )

  optimizer <- OptimizerSample$new()
  optimizer$param_set$set_values(
    candidate_generator = candidate_generator_grid(resolution = 3L),
    sample_size = 3L
  )

  optimizer$optimize(instance)

  archive_data <- copy(instance$archive$data)
  setorder(archive_data, x)
  expect_equal(archive_data$x, 1:3)
  expect_equal(archive_data$y, c(1, 4, 9))
  expect_equal(
    vapply(archive_data$x_domain, function(xs) xs$payload[[1L]], numeric(1L)),
    c(1, 4, 9)
  )
})

test_that("opt('sample') dictionary accessor works", {
  optimizer <- opt("sample")

  expect_r6(optimizer, "OptimizerSample")
  expect_equal(optimizer$id, "sample")

  # Can set parameters after creation
  sobol_gen <- candidate_generator_sobol()
  optimizer$param_set$set_values(
    candidate_generator = sobol_gen,
    sample_size = 50L
  )

  expect_identical(optimizer$param_set$values$candidate_generator, sobol_gen)
  expect_equal(optimizer$param_set$values$sample_size, 50L)
})
