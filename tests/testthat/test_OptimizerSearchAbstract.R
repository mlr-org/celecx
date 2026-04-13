# Concrete subclass for testing the abstract base class
TestOptimizerSearch <- R6Class("TestOptimizerSearch",
  inherit = OptimizerSearchAbstract,
  public = list(
    initialize = function() {
      super$initialize(
        id = "test_search",
        param_set = ps(batch_size = p_int(lower = 1L, init = 1L)),
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl"),
        properties = c("single-crit", "multi-crit"),
        packages = character(),
        label = "Test Search Optimizer"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      sampler <- SamplerUnif$new(inst$search_space)
      repeat {
        design <- sampler$sample(self$param_set$values$batch_size)
        inst$eval_batch(design$data)
      }
    }
  )
)


test_that("OptimizerSearchAbstract inherits from OptimizerBatch", {
  opt <- TestOptimizerSearch$new()
  expect_r6(opt, "OptimizerSearchAbstract")
  expect_r6(opt, "OptimizerBatch")
  expect_r6(opt, "Optimizer")
})

test_that("OptimizerSearchAbstract works with OptimInstance and assigns result", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -5, upper = 5)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    search_space = ps(x = p_dbl(lower = -5, upper = 5)),
    terminator = trm("evals", n_evals = 10)
  )

  opt <- TestOptimizerSearch$new()
  opt$param_set$set_values(batch_size = 5L)
  opt$optimize(instance)

  expect_equal(instance$archive$n_evals, 10L)
  # Result should be assigned (best point)
  expect_data_table(instance$result, nrows = 1L)
  expect_names(names(instance$result), must.include = "x")
})

test_that("OptimizerSearchAbstract works with SearchInstance (no result assignment)", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -5, upper = 5)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 10)
  )

  opt <- TestOptimizerSearch$new()
  opt$param_set$set_values(batch_size = 5L)

  # Should not error
  opt$optimize(instance)

  expect_gte(instance$archive$n_evals, 10L)
})

test_that("OptimizerSearchAbstract rejects unsupported ParamUty search spaces", {
  objective <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = vapply(xdt$payload, function(payload) {
        payload$value
      }, numeric(1L)))
    },
    domain = ps(
      x = p_int(lower = 1L, upper = 3L),
      payload = p_uty()
    ),
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )
  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("none"),
    check_values = FALSE
  )

  opt <- TestOptimizerSearch$new()

  expect_error(
    opt$optimize(instance),
    "ParamUty"
  )
})

test_that("Existing optimizers inherit from OptimizerSearchAbstract", {
  expect_r6(OptimizerPoolRandom$new(), "OptimizerSearchAbstract")
})
