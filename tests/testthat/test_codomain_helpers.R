
# Tests for codomain_helpers.R
#
# Note: As of bbotk >= 0.9.0, Codomain natively supports "learn" tags,
# so many validation functions are no longer needed. These tests focus
# on the helper functions that remain.

# =============================================================================
# codomain_target_ids tests
# =============================================================================

test_that("codomain_target_ids returns all target IDs", {
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "maximize"),
    y3 = p_dbl(tags = "learn"),
    extra = p_dbl()  # no target tag
  )
  ids <- codomain_target_ids(codomain)
  expect_equal(sort(ids), c("y1", "y2", "y3"))
})

test_that("codomain_target_ids returns empty for no targets", {
  codomain <- ps(y = p_dbl())
  ids <- codomain_target_ids(codomain)
  expect_equal(ids, character(0))
})

# =============================================================================
# codomain_optimize_ids tests
# =============================================================================

test_that("codomain_optimize_ids returns only minimize/maximize IDs", {
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "maximize"),
    y3 = p_dbl(tags = "learn")
  )
  ids <- codomain_optimize_ids(codomain)
  expect_equal(sort(ids), c("y1", "y2"))
})

test_that("codomain_optimize_ids excludes learn targets", {
  codomain <- ps(y = p_dbl(tags = "learn"))
  ids <- codomain_optimize_ids(codomain)
  expect_equal(ids, character(0))
})

# =============================================================================
# codomain_learn_ids tests
# =============================================================================

test_that("codomain_learn_ids returns only learn IDs", {
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "learn")
  )
  ids <- codomain_learn_ids(codomain)
  expect_equal(ids, "y2")
})

test_that("codomain_learn_ids excludes optimize targets", {
  codomain <- ps(y = p_dbl(tags = "minimize"))
  ids <- codomain_learn_ids(codomain)
  expect_equal(ids, character(0))
})

# =============================================================================
# codomain_has_learn tests
# =============================================================================

test_that("codomain_has_learn returns TRUE when learn tag present", {
  codomain <- ps(y = p_dbl(tags = "learn"))
  expect_true(codomain_has_learn(codomain))
})

test_that("codomain_has_learn returns FALSE when no learn tag", {
  codomain <- ps(y = p_dbl(tags = "minimize"))
  expect_false(codomain_has_learn(codomain))
})

# =============================================================================
# codomain_has_optimize tests
# =============================================================================

test_that("codomain_has_optimize returns TRUE when minimize tag present", {
  codomain <- ps(y = p_dbl(tags = "minimize"))
  expect_true(codomain_has_optimize(codomain))
})

test_that("codomain_has_optimize returns TRUE when maximize tag present", {
  codomain <- ps(y = p_dbl(tags = "maximize"))
  expect_true(codomain_has_optimize(codomain))
})

test_that("codomain_has_optimize returns FALSE when only learn tag", {
  codomain <- ps(y = p_dbl(tags = "learn"))
  expect_false(codomain_has_optimize(codomain))
})

# =============================================================================
# codomain_goal tests
# =============================================================================

test_that("codomain_goal returns 'optimize' for minimize-only", {
  codomain <- ps(y = p_dbl(tags = "minimize"))
  expect_equal(codomain_goal(codomain), "optimize")
})

test_that("codomain_goal returns 'optimize' for maximize-only", {
  codomain <- ps(y = p_dbl(tags = "maximize"))
  expect_equal(codomain_goal(codomain), "optimize")
})

test_that("codomain_goal returns 'learn' for learn-only", {
  codomain <- ps(y = p_dbl(tags = "learn"))
  expect_equal(codomain_goal(codomain), "learn")
})

test_that("codomain_goal returns 'both' for mixed targets", {
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "learn")
  )
  expect_equal(codomain_goal(codomain), "both")
})

test_that("codomain_goal errors when no targets", {
  codomain <- ps(y = p_dbl())
  expect_error(codomain_goal(codomain), "no targets")
})
