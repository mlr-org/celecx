# -- Minimal concrete subclass for testing the abstract base class -----------

OptimizerPoolTest <- R6::R6Class("OptimizerPoolTest",
  inherit = OptimizerPoolAbstract,

  public = list(
    # track calls for assertions
    discrete_calls = NULL,
    continuous_calls = NULL,

    initialize = function(grid_expansion_limit = 1e7L) {
      self$discrete_calls <- list()
      self$continuous_calls <- list()
      super$initialize(
        id = "pool_test",
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl"),
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = character(),
        label = "Pool Test Optimizer",
        grid_expansion_limit = grid_expansion_limit
      )
    }
  ),

  private = list(
    .optimize_discrete = function(inst, candidates) {
      self$discrete_calls <- c(self$discrete_calls, list(list(
        inst = inst,
        candidates = copy(candidates)
      )))
      # evaluate all candidates in one batch, like a simple exhaustive search
      inst$eval_batch(candidates)
    },

    .optimize_continuous = function(inst) {
      self$continuous_calls <- c(self$continuous_calls, list(list(inst = inst)))
      sampler <- SamplerUnif$new(inst$search_space)
      design <- sampler$sample(1L)
      inst$eval_batch(design$data)
    }
  )
)

# -- Construction & Inheritance ------------------------------------------------

test_that("OptimizerPoolAbstract cannot be used directly (abstract methods)", {
  optimizer <- OptimizerPoolAbstract$new(
    id = "abstract_test",
    param_classes = "ParamDbl",
    properties = "single-crit"
  )

  expect_r6(optimizer, "OptimizerPoolAbstract")
  expect_r6(optimizer, "OptimizerSearchAbstract")
  expect_r6(optimizer, "OptimizerBatch")

  # abstract methods error
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 5)
  )

  expect_error(optimizer$optimize(instance), "abstract")
})

test_that("Concrete subclass inherits from OptimizerPoolAbstract", {
  optimizer <- OptimizerPoolTest$new()
  expect_r6(optimizer, "OptimizerPoolAbstract")
  expect_r6(optimizer, "OptimizerSearchAbstract")
})

test_that("grid_expansion_limit is accessible via active binding", {
  optimizer <- OptimizerPoolTest$new(grid_expansion_limit = 500L)
  expect_equal(optimizer$grid_expansion_limit, 500L)
})

# -- Discrete dispatch ---------------------------------------------------------

test_that("OptimizerPoolAbstract dispatches to .optimize_discrete for pool objectives", {
  pool <- data.table(x = 1:5, y = (1:5)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 5L)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_dispatch"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = 5L)),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- OptimizerPoolTest$new()
  optimizer$optimize(instance)

  expect_length(optimizer$discrete_calls, 1L)
  expect_length(optimizer$continuous_calls, 0L)
  # candidates should be the full pool (all 5 rows valid)
  expect_equal(nrow(optimizer$discrete_calls[[1L]]$candidates), 5L)
  expect_set_equal(optimizer$discrete_calls[[1L]]$candidates$x, 1:5)
})

test_that("OptimizerPoolAbstract dispatches to .optimize_discrete for fully discrete spaces", {
  search_space <- ps(
    x = p_int(lower = 1L, upper = 3L),
    y = p_fct(levels = c("a", "b"))
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(z = 1),
    domain = search_space,
    codomain = ps(z = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- OptimizerPoolTest$new()
  optimizer$optimize(instance)

  expect_length(optimizer$discrete_calls, 1L)
  expect_length(optimizer$continuous_calls, 0L)
  # 3 * 2 = 6 grid candidates
  expect_equal(nrow(optimizer$discrete_calls[[1L]]$candidates), 6L)
})

# -- Continuous dispatch -------------------------------------------------------

test_that("OptimizerPoolAbstract dispatches to .optimize_continuous for continuous spaces", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 5)
  )

  optimizer <- OptimizerPoolTest$new()
  optimizer$optimize(instance)

  expect_length(optimizer$continuous_calls, 1L)
  expect_length(optimizer$discrete_calls, 0L)
})

# -- Pool candidate filtering --------------------------------------------------

test_that("OptimizerPoolAbstract filters pool candidates by search space bounds", {
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
    id = "pool_filter"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(
      x = p_int(lower = 2L, upper = 3L),
      mode = p_fct(levels = c("a", "b"))
    ),
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- OptimizerPoolTest$new()
  optimizer$optimize(instance)

  candidates <- optimizer$discrete_calls[[1L]]$candidates
  expect_equal(nrow(candidates), 4L)
  expect_true(all(candidates$x %in% 2:3))
  expect_true(all(candidates$mode %in% c("a", "b")))
})

test_that("OptimizerPoolAbstract errors on empty pool after filtering", {
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

  optimizer <- OptimizerPoolTest$new()
  expect_error(optimizer$optimize(instance), "no admissible configurations")
})

test_that("OptimizerPoolAbstract rejects trafos for pool-restricted objectives", {
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

  optimizer <- OptimizerPoolTest$new()
  expect_error(optimizer$optimize(instance), "does not support trafos")
})

test_that("OptimizerPoolAbstract respects constraints in pool filtering", {
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

  optimizer <- OptimizerPoolTest$new()
  optimizer$optimize(instance)

  candidates <- optimizer$discrete_calls[[1L]]$candidates
  expect_equal(nrow(candidates), 3L)
  sums <- candidates$x1 + candidates$x2
  expect_true(all(sums <= 1L))
})

test_that("OptimizerPoolAbstract respects dependencies in pool filtering", {
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
    id = "pool_deps"
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- OptimizerPoolTest$new()
  optimizer$optimize(instance)

  candidates <- optimizer$discrete_calls[[1L]]$candidates
  expect_equal(nrow(candidates), 1L)
  expect_false(candidates$gate[[1L]])
  expect_true(is.na(candidates$x[[1L]]))
})

# -- Grid expansion limit ------------------------------------------------------

test_that("grid_expansion_limit is respected for discrete non-pool spaces", {
  # 4 * 3 = 12 grid points > limit of 10
  search_space <- ps(
    x = p_int(lower = 1L, upper = 4L),
    y = p_fct(levels = c("a", "b", "c"))
  )
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(z = 1),
    domain = search_space,
    codomain = ps(z = p_dbl(tags = "minimize"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- OptimizerPoolTest$new(grid_expansion_limit = 10L)
  expect_error(optimizer$optimize(instance), "exceed")
})

# -- SearchInstance compatibility ----------------------------------------------

test_that("OptimizerPoolAbstract subclass works with SearchInstance", {
  pool <- data.table(x = 1:5, y = (1:5)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = 5L)),
    codomain = ps(y = p_dbl(tags = "learn")),
    id = "pool_search"
  )

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 100)
  )

  optimizer <- OptimizerPoolTest$new()
  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 5L)
})
