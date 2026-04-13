
# Tests for utils_codomain.R

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
# codomain_goals tests
# =============================================================================

test_that("codomain_goals returns 'minimize' for minimize-only", {
  codomain <- ps(y = p_dbl(tags = "minimize"))
  expect_equal(codomain_goals(codomain), "minimize")
})

test_that("codomain_goals returns 'maximize' for maximize-only", {
  codomain <- ps(y = p_dbl(tags = "maximize"))
  expect_equal(codomain_goals(codomain), "maximize")
})

test_that("codomain_goals returns 'learn' for learn-only", {
  codomain <- ps(y = p_dbl(tags = "learn"))
  expect_equal(codomain_goals(codomain), "learn")
})

test_that("codomain_goals returns all tags for mixed targets", {
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "learn")
  )
  expect_equal(codomain_goals(codomain), c("minimize", "learn"))
})

test_that("codomain_goals returns empty for no targets", {
  codomain <- ps(y = p_dbl())
  expect_equal(codomain_goals(codomain), character(0))
})

test_that("codomain_goals deduplicates tags", {
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "minimize")
  )
  expect_equal(codomain_goals(codomain), "minimize")
})
