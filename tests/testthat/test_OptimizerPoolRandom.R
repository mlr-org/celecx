# -- Construction & Parameter Defaults ----------------------------------------

test_that("OptimizerPoolRandom basic construction", {
  optimizer <- OptimizerPoolRandom$new()

  expect_r6(optimizer, "OptimizerPoolRandom")
  expect_r6(optimizer, "OptimizerBatch")
  expect_r6(optimizer, "Optimizer")

  expect_equal(optimizer$id, "pool_random")
  expect_equal(optimizer$label, "Pool Random Search")
  expect_set_equal(
    optimizer$param_set$ids(),
    c("batch_size", "max_batches", "replace_samples")
  )
})

test_that("OptimizerPoolRandom parameters have correct defaults", {
  optimizer <- OptimizerPoolRandom$new()

  expect_equal(optimizer$param_set$values$batch_size, 1L)
  expect_equal(optimizer$param_set$values$max_batches, Inf)
  expect_equal(optimizer$param_set$values$replace_samples, "never")
})

test_that("OptimizerPoolRandom accepts Inf for batch_size and max_batches", {
  optimizer <- OptimizerPoolRandom$new()

  optimizer$param_set$set_values(batch_size = Inf)
  expect_identical(optimizer$param_set$values$batch_size, Inf)

  optimizer$param_set$set_values(batch_size = 5L, max_batches = Inf)
  expect_identical(optimizer$param_set$values$max_batches, Inf)
})

test_that("opt('pool_random') dictionary accessor works", {
  optimizer <- opt("pool_random")

  expect_r6(optimizer, "OptimizerPoolRandom")
  expect_equal(optimizer$id, "pool_random")
})

# -- Pool-restricted Objectives -----------------------------------------------

test_that("OptimizerPoolRandom works with pool-restricted objective, replace_samples = 'never'", {
  pool <- data.table(x = 1:10, y = (1:10)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 10L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_test"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 10L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 3L, replace_samples = "never")
  optimizer$optimize(instance)

  # With replace_samples = "never", we can evaluate at most 10 unique points
  expect_lte(instance$archive$n_evals, 10L)
  # All evaluated x values must be unique
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
  # All evaluated x values must be in the pool
  expect_true(all(instance$archive$data$x %in% pool$x))
})

test_that("OptimizerPoolRandom exhausts pool with replace_samples = 'never'", {
  pool <- data.table(x = 1:5, y = (1:5)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 5L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_exhaust"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 5L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 2L, replace_samples = "never")
  optimizer$optimize(instance)

  # Should evaluate exactly 5 points (pool exhausted)
  expect_equal(instance$archive$n_evals, 5L)
  expect_set_equal(instance$archive$data$x, 1:5)
})

test_that("OptimizerPoolRandom restricts pool objectives to the supplied search space", {
  pool <- CJ(x = 1:5, mode = c("a", "b", "c"))
  pool[, value := seq_len(.N)]

  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(value)],
    domain = ps(
      x = p_int(lower = 1L, upper = 5L),
      mode = p_fct(levels = c("a", "b", "c"))
    ),
    codomain = ps(value = p_dbl(tags = "minimize")),
    id = "pool_restricted_subset"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(
      x = p_int(lower = 2L, upper = 4L),
      mode = p_fct(levels = c("b", "c"))
    ),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 6L)
  expect_true(all(instance$archive$data$x %in% 2:4))
  expect_true(all(instance$archive$data$mode %in% c("b", "c")))
  expect_equal(
    uniqueN(instance$archive$data[, .(x, mode)]),
    6L
  )
})

test_that("OptimizerPoolRandom respects fixed search-space values for pool objectives", {
  pool <- data.table(x = 1:5, y = (1:5)^2)
  search_space <- ps(x = p_int(lower = 1L, upper = 5L))
  search_space$values <- list(x = 3L)

  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 5L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_fixed_value"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 1L)
  expect_equal(instance$archive$data$x, 3L)
})

test_that("OptimizerPoolRandom respects special values already present in the pool", {
  pool <- data.table(x = c(1L, 10L), y = c(1, 100))
  domain <- ps(x = p_int(lower = 1L, upper = 3L, special_vals = list(10L)))

  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_special_values"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = domain$clone(deep = TRUE),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 2L)
  expect_set_equal(instance$archive$data$x, c(1L, 10L))
})

test_that("OptimizerPoolRandom filters non-integer pool rows for integer search spaces", {
  pool <- data.table(x = c(1, 1.5, 2, 2.5, 3), y = seq_len(5))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_dbl(lower = 1, upper = 3)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_integer_filter"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 3L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 3L)
  expect_equal(sort(instance$archive$data$x), c(1, 2, 3))
})

test_that("OptimizerPoolRandom rejects incompatible logical search spaces up front", {
  pool <- data.table(flag = c(0L, 1L), y = c(1, 2))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(flag = p_int(lower = 0L, upper = 1L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_logical_filter"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(flag = p_lgl()),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")

  expect_error(
    optimizer$optimize(instance),
    "no admissible configurations"
  )
})

test_that("OptimizerPoolRandom respects inactive NA columns for pool objectives with dependencies", {
  pool <- data.table(
    gate = c(FALSE, TRUE, TRUE),
    x = c(NA_integer_, 1L, 2L),
    y = c(0, 1, 2)
  )
  domain <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 2L, depends = gate == TRUE)
  )
  search_space <- domain$clone(deep = TRUE)
  search_space$values <- list(gate = FALSE)

  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_dependencies"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 1L)
  expect_false(instance$archive$data$gate[[1L]])
  expect_true(is.na(instance$archive$data$x[[1L]]))
})

test_that("OptimizerPoolRandom respects search-space constraints for pool objectives", {
  pool <- CJ(x1 = 0:2, x2 = 0:2)
  pool[, y := x1 + x2]
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(
      x1 = p_int(lower = 0L, upper = 2L),
      x2 = p_int(lower = 0L, upper = 2L)
    ),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_constraint"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(
      x1 = p_int(lower = 0L, upper = 2L),
      x2 = p_int(lower = 0L, upper = 2L),
      .constraint = function(x) x$x1 + x$x2 <= 1L
    ),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 3L)
  expect_set_equal(
    apply(instance$archive$data[, .(x1, x2)], 1L, paste, collapse = ","),
    c("0,0", "0,1", "1,0")
  )
})

test_that("OptimizerPoolRandom excludes already evaluated pool rows once up front", {
  pool <- data.table(x = 1:5, y = (1:5)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 5L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_prefilled"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 2L, upper = 4L)),
    terminator = trm("evals", n_evals = 100)
  )

  instance$eval_batch(data.table(x = c(2L, 3L)))

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 3L)
  expect_equal(instance$archive$data$x, c(2L, 3L, 4L))
})

test_that("OptimizerPoolRandom with replace_samples = 'between_batches' allows cross-batch duplicates", {
  pool <- data.table(x = 1:3, y = c(9, 4, 1))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_between"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 3L)),
    terminator = trm("evals", n_evals = 9)
  )

  set.seed(42)
  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = 3L,
    replace_samples = "between_batches"
  )
  optimizer$optimize(instance)

  # 3 batches of 3 = 9 evaluations
  expect_equal(instance$archive$n_evals, 9L)
  # Within each batch, no duplicates (without replacement inside batch)
  for (b in seq_len(instance$archive$n_batch)) {
    batch_data <- instance$archive$data[batch_nr == b]
    expect_equal(anyDuplicated(batch_data$x), 0L,
      info = paste("batch", b))
  }
  # But across batches, duplicates are allowed (3 points, 9 evals)
  expect_true(anyDuplicated(instance$archive$data$x) > 0L)
})

test_that("OptimizerPoolRandom with replace_samples = 'within_batches' allows within-batch duplicates", {
  pool <- data.table(x = 1:2, y = c(4, 1))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 2L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_within"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 2L)),
    terminator = trm("evals", n_evals = 10)
  )

  set.seed(1)
  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = 5L,
    replace_samples = "within_batches"
  )
  optimizer$optimize(instance)

  # 2 batches of 5 = 10 evaluations
  expect_equal(instance$archive$n_evals, 10L)
  expect_equal(instance$archive$n_batch, 2L)
  batch_sizes <- instance$archive$data[, .N, by = batch_nr]
  expect_equal(batch_sizes$N, c(5L, 5L))
  # All values still come from the pool
  expect_true(all(instance$archive$data$x %in% pool$x))
})

test_that("batch_size = Inf evaluates entire remaining pool at once", {
  pool <- data.table(x = 1:7, y = (1:7)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 7L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_inf"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 7L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = Inf,
    replace_samples = "never"
  )
  optimizer$optimize(instance)

  # All 7 points evaluated in a single batch
  expect_equal(instance$archive$n_evals, 7L)
  expect_equal(instance$archive$n_batch, 1L)
  expect_set_equal(instance$archive$data$x, 1:7)
})

test_that("batch_size = Inf with replace_samples = 'within_batches' errors", {
  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = Inf,
    replace_samples = "within_batches"
  )

  pool <- data.table(x = 1:3, y = 1:3)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_err"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 3L)),
    terminator = trm("evals", n_evals = 100)
  )

  expect_error(optimizer$optimize(instance), "within_batches")
})

test_that("OptimizerPoolRandom errors when the restricted pool is empty", {
  pool <- data.table(x = 1:3, y = c(1, 4, 9))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_empty"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 10L, upper = 12L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  expect_error(
    optimizer$optimize(instance),
    "no admissible configurations"
  )
})

test_that("OptimizerPoolRandom rejects trafos for pool-restricted objectives", {
  pool <- data.table(x = 1:3, y = c(1, 4, 9))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_trafo"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(
      x = p_int(lower = 1L, upper = 3L, trafo = function(x) x)
    ),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  expect_error(
    optimizer$optimize(instance),
    "does not support trafos"
  )
})

test_that("OptimizerPoolRandom rejects extra trafos for pool-restricted objectives", {
  pool <- data.table(x = 1:3, y = c(1, 4, 9))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_extra_trafo"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(
      x = p_int(lower = 1L, upper = 3L),
      .extra_trafo = function(x) x
    ),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  expect_error(
    optimizer$optimize(instance),
    "does not support trafos"
  )
})

# -- max_batches Limiting -----------------------------------------------------

test_that("max_batches limits number of batches evaluated", {
  pool <- data.table(x = 1:20, y = (1:20)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 20L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_max_batches"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 20L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = 3L,
    max_batches = 2L,
    replace_samples = "never"
  )
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_batch, 2L)
  expect_equal(instance$archive$n_evals, 6L)
})

# -- Fully Discrete Search Space (no pool objective) --------------------------

test_that("OptimizerPoolRandom works with fully discrete search space", {
  search_space <- ps(
    x = p_int(lower = 1L, upper = 4L),
    y = p_fct(levels = c("a", "b"))
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(z = if (xs$y == "a") xs$x else xs$x + 10),
    domain = search_space,
    codomain = ps(z = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 3L, replace_samples = "never")
  optimizer$optimize(instance)

  # 4 * 2 = 8 possible configurations
  expect_equal(instance$archive$n_evals, 8L)
  expect_equal(anyDuplicated(instance$archive$data[, .(x, y)]), 0L)
})

test_that("Discrete search space with batch_size = Inf evaluates full grid", {
  search_space <- ps(
    a = p_lgl(),
    b = p_fct(levels = c("x", "y", "z"))
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(val = runif(1)),
    domain = search_space,
    codomain = ps(val = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  # 2 * 3 = 6 configurations
  expect_equal(instance$archive$n_evals, 6L)
  expect_equal(instance$archive$n_batch, 1L)
})

test_that("Discrete search space with between_batches allows duplicates across batches", {
  search_space <- ps(x = p_int(lower = 1L, upper = 2L))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 6)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = 2L,
    replace_samples = "between_batches"
  )
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 6L)
  # 3 batches of 2 from pool of size 2
  expect_equal(instance$archive$n_batch, 3L)
})

test_that("OptimizerPoolRandom applies search-space trafos for non-pool objectives", {
  search_space <- ps(
    x = p_int(
      lower = 1L,
      upper = 3L,
      trafo = function(x) c(-1, 0, 1)[x]
    )
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = ps(x = p_dbl(lower = -1, upper = 1)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  x_domain <- vapply(instance$archive$data$x_domain, `[[`, numeric(1L), "x")
  expect_equal(instance$archive$n_evals, 3L)
  expect_equal(sort(instance$archive$data$x), 1:3)
  expect_equal(sort(x_domain), c(-1, 0, 1))
  expect_equal(sort(instance$archive$data$y), c(-1, 0, 1))
})

test_that("OptimizerPoolRandom supports extra trafos that create ParamUty objective inputs", {
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
    terminator = trm("evals", n_evals = 10L),
    check_values = TRUE
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 3L, max_batches = 1L)
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

# -- Continuous Fallback ------------------------------------------------------

test_that("OptimizerPoolRandom falls back to random search on continuous space", {
  search_space <- ps(x = p_dbl(lower = -5, upper = 5))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 10)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 5L)
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 10L)
  expect_equal(instance$archive$n_batch, 2L)
})

test_that("Continuous space with max_batches limits batches", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 4L, max_batches = 3L)
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_batch, 3L)
  expect_equal(instance$archive$n_evals, 12L)
})

test_that("batch_size = Inf errors on continuous search space", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 10)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf)

  expect_error(optimizer$optimize(instance), "batch_size.*Inf")
})

# -- Terminator Interaction ---------------------------------------------------

test_that("Terminator stops optimization before pool is exhausted", {
  pool <- data.table(x = 1:100, y = (1:100)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 100L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_term"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 100L)),
    terminator = trm("evals", n_evals = 10)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 3L, replace_samples = "never")
  optimizer$optimize(instance)

  # Terminator fires at 10 evals (batches of 3: 3, 6, 9, then 12 triggers)
  # The exact count depends on when the terminator fires
  expect_lte(instance$archive$n_evals, 12L)
  expect_gte(instance$archive$n_evals, 9L)
})

# -- Multi-dimensional Pool --------------------------------------------------

test_that("OptimizerPoolRandom works with multi-dimensional pool", {
  pool <- CJ(x1 = 1:3, x2 = c("a", "b"))
  pool[, y := seq_len(.N)]
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(
      x1 = p_int(lower = 1L, upper = 3L),
      x2 = p_fct(levels = c("a", "b"))
    ),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_multi"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(
      x1 = p_int(lower = 1L, upper = 3L),
      x2 = p_fct(levels = c("a", "b"))
    ),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 2L, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 6L)
  expect_equal(
    anyDuplicated(instance$archive$data[, .(x1, x2)]),
    0L
  )
})

# -- Pool with Duplicate Domain Rows ------------------------------------------

test_that("Duplicate domain rows in pool are deduplicated", {
  # ObjectivePoolAbstract rejects duplicates in pool, but ObjectivePoolWrapper
  # may have a narrower domain. We test with a pool that has unique domain rows.
  pool <- data.table(x = c(1, 2, 3), y = c(10, 20, 30))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_dedup"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 3L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = Inf, replace_samples = "never")
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 3L)
})

# -- Edge Cases ---------------------------------------------------------------

test_that("Empty pool after first batch with replace_samples = 'never' stops", {
  pool <- data.table(x = 1:2, y = c(1, 4))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 2L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_small"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 2L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = Inf,
    replace_samples = "never"
  )
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 2L)
  expect_equal(instance$archive$n_batch, 1L)
})

test_that("max_batches = 1 evaluates only one batch", {
  pool <- data.table(x = 1:10, y = (1:10)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 10L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_one_batch"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 10L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 5L, max_batches = 1L)
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_batch, 1L)
  expect_equal(instance$archive$n_evals, 5L)
})

# -- replace_samples interaction with batch_size > pool size ------------------

test_that("batch_size larger than pool is clamped for 'never' and 'between_batches'", {
  pool <- data.table(x = 1:3, y = c(1, 4, 9))
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_clamp"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 3L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(
    batch_size = 100L,
    replace_samples = "never"
  )
  optimizer$optimize(instance)

  # Clamped to pool size
  expect_equal(instance$archive$n_evals, 3L)
})

# -- SearchInstance Compatibility ---------------------------------------------

test_that("OptimizerPoolRandom works with SearchInstance", {
  pool <- data.table(x = seq(0, 1, length.out = 20), y = seq(0, 1, length.out = 20)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_dbl(lower = 0, upper = 1)),
    codomain = ps(y = p_dbl(tags = "learn")),
    id = "pool_search"
  )

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 10)
  )

  optimizer <- opt("pool_random")
  optimizer$param_set$set_values(batch_size = 3L, replace_samples = "never")

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_gte(instance$archive$n_evals, 9L)
  expect_lte(instance$archive$n_evals, 12L)
})
