OptimizerPoolSampleTestSampler <- R6Class("OptimizerPoolSampleTestSampler",
  inherit = SpaceSampler,

  public = list(
    initialize = function(deterministic = TRUE) {
      super$initialize(
        id = if (deterministic) "test_deterministic" else "test_nondeterministic",
        deterministic = deterministic,
        label = "Test Sampler",
        man = NA_character_
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (is.null(pool)) {
        return(generate_design_random(search_space, n)$data)
      }

      feature_ids <- search_space$ids()
      candidates <- if (is.null(known_pool)) {
        pool
      } else {
        pool[!known_pool, on = feature_ids]
      }
      if (nrow(candidates) < n) {
        candidates <- pool
      }

      candidates[seq_len(n)]
    }
  )
)

optimizer_pool_sample_test_instance <- function(n_pool = 4L, n_evals = 100L) {
  pool <- data.table(x = seq_len(n_pool), y = seq_len(n_pool)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = n_pool)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    id = "pool_sample_test"
  )

  OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = n_pool)),
    terminator = trm("evals", n_evals = n_evals)
  )
}

test_that("OptimizerPoolSample basic construction", {
  optimizer <- OptimizerPoolSample$new()

  expect_r6(optimizer, "OptimizerPoolSample")
  expect_r6(optimizer, "OptimizerPoolAbstract")
  expect_r6(optimizer, "OptimizerBatch")
  expect_r6(optimizer$space_sampler, "SpaceSamplerUniform")

  expect_equal(optimizer$id, "pool_sample")
  expect_equal(optimizer$label, "Pool Sample Search")
  expect_set_equal(
    optimizer$param_set$ids(),
    c("batch_size", "max_batches", "replace_samples", "pass_known_pool")
  )
})

test_that("OptimizerPoolSample parameters have correct defaults", {
  optimizer <- OptimizerPoolSample$new()

  expect_equal(optimizer$param_set$values$batch_size, 1L)
  expect_equal(optimizer$param_set$values$max_batches, Inf)
  expect_equal(optimizer$param_set$values$replace_samples, "never")
  expect_true(optimizer$param_set$values$pass_known_pool)
})

test_that("opt('pool_sample') dictionary accessor works", {
  optimizer <- opt("pool_sample")

  expect_r6(optimizer, "OptimizerPoolSample")
  expect_equal(optimizer$id, "pool_sample")
})

test_that("OptimizerPoolSample forwards sampler parameters after deep clone", {
  optimizer <- OptimizerPoolSample$new(
    space_sampler = SpaceSamplerGSx$new(distance = clx_ald("gower"))
  )

  expect_true(inherits(optimizer$param_set, "ParamSetCollection"))
  expect_setequal(
    optimizer$param_set$ids(),
    c("batch_size", "max_batches", "replace_samples", "pass_known_pool", "space_sampler.scale")
  )
  expect_identical(optimizer$param_set$values$space_sampler.scale, "minmax_auto")

  optimizer$param_set$values$space_sampler.scale <- "off"
  expect_identical(optimizer$space_sampler$param_set$values$scale, "off")

  clone <- optimizer$clone(deep = TRUE)

  expect_true(inherits(clone$param_set, "ParamSetCollection"))
  expect_identical(clone$param_set$values$space_sampler.scale, "off")
  expect_identical(clone$space_sampler$param_set$values$scale, "off")

  clone$param_set$values$space_sampler.scale <- "standardize"
  expect_identical(clone$space_sampler$param_set$values$scale, "standardize")
  expect_identical(optimizer$space_sampler$param_set$values$scale, "off")

  clone$space_sampler$param_set$values$scale <- "minmax_space"
  expect_identical(clone$param_set$values$space_sampler.scale, "minmax_space")
  expect_identical(optimizer$param_set$values$space_sampler.scale, "off")
})

test_that("OptimizerPoolSample with replace_samples = 'never' shrinks the pool", {
  instance <- optimizer_pool_sample_test_instance()
  optimizer <- OptimizerPoolSample$new(space_sampler = OptimizerPoolSampleTestSampler$new())
  optimizer$param_set$set_values(
    batch_size = 2L,
    max_batches = 2L,
    replace_samples = "never",
    pass_known_pool = FALSE
  )

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 4L)
  expect_equal(instance$archive$data$x, 1:4)
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
})

test_that("OptimizerPoolSample passes known_pool to deterministic samplers", {
  instance <- optimizer_pool_sample_test_instance()
  optimizer <- OptimizerPoolSample$new(space_sampler = OptimizerPoolSampleTestSampler$new())
  optimizer$param_set$set_values(
    batch_size = 2L,
    max_batches = 2L,
    replace_samples = "between_batches",
    pass_known_pool = TRUE
  )

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 4L)
  expect_equal(instance$archive$data$x, 1:4)
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
})

test_that("OptimizerPoolSample warns when deterministic batches can repeat", {
  instance <- optimizer_pool_sample_test_instance()
  optimizer <- OptimizerPoolSample$new(space_sampler = OptimizerPoolSampleTestSampler$new())
  optimizer$param_set$set_values(
    batch_size = 2L,
    max_batches = 2L,
    replace_samples = "between_batches",
    pass_known_pool = FALSE
  )

  expect_warning(
    optimizer$optimize(instance),
    "every batch may contain the same sample"
  )

  expect_equal(instance$archive$n_evals, 4L)
  expect_equal(instance$archive$data$x, c(1L, 2L, 1L, 2L))
  expect_true(anyDuplicated(instance$archive$data$x) > 0L)
})

test_that("OptimizerPoolSample does not warn for nondeterministic samplers", {
  instance <- optimizer_pool_sample_test_instance()
  optimizer <- OptimizerPoolSample$new(
    space_sampler = OptimizerPoolSampleTestSampler$new(deterministic = FALSE)
  )
  optimizer$param_set$set_values(
    batch_size = 2L,
    max_batches = 2L,
    replace_samples = "between_batches",
    pass_known_pool = FALSE
  )

  expect_warning(optimizer$optimize(instance), NA)
  expect_equal(instance$archive$n_evals, 4L)
})

test_that("OptimizerPoolSample falls back to sampler search on continuous space", {
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

  optimizer <- OptimizerPoolSample$new()
  optimizer$param_set$set_values(batch_size = 4L, max_batches = 3L)
  optimizer$optimize(instance)

  expect_equal(instance$archive$n_evals, 12L)
  expect_equal(instance$archive$n_batch, 3L)
})

test_that("OptimizerPoolSample rejects batch_size = Inf on continuous space", {
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

  optimizer <- OptimizerPoolSample$new()
  optimizer$param_set$set_values(batch_size = Inf)

  expect_error(optimizer$optimize(instance), "batch_size.*Inf")
})
