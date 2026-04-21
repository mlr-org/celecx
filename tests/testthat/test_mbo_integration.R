# =============================================================================
# Tests for live optimizer integration with non-optimizing codomains
# =============================================================================

test_that("optimizer_active_learning uses ResultAssignerNull for learn codomains", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -2, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 8L)
  )

  optimizer <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    batch_size = 2L,
    acq_evals = 8L
  )

  expect_r6(optimizer$result_assigner, "ResultAssignerNull")
  expect_no_error(optimizer$optimize(instance))
  expect_true(instance$is_terminated)
  expect_equal(instance$archive$n_evals, 8L)
  expect_null(instance$result)
})
