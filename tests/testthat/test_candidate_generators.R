# =============================================================================
# Tests for candidate_generators.R
# =============================================================================

# Helper function to create test search spaces
create_numeric_search_space <- function() {
  ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = -5, upper = 5)
  )
}

create_mixed_search_space <- function() {
  ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_int(lower = 1L, upper = 10L),
    x3 = p_fct(levels = c("a", "b", "c"))
  )
}

create_small_search_space <- function() {
  ps(x = p_dbl(lower = 0, upper = 1))
}


# =============================================================================
# candidate_generator_lhs Tests
# =============================================================================

test_that("candidate_generator_lhs returns a function", {
  gen <- candidate_generator_lhs()
  expect_function(gen)
})

test_that("candidate_generator_lhs generates correct number of candidates", {
  gen <- candidate_generator_lhs()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 50)
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 50)
})

test_that("candidate_generator_lhs generates correct columns", {
  gen <- candidate_generator_lhs()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 20)
  expect_names(names(candidates), identical.to = c("x1", "x2"))
})

test_that("candidate_generator_lhs respects bounds", {
  gen <- candidate_generator_lhs()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 100)
  expect_true(all(candidates$x1 >= 0 & candidates$x1 <= 1))
  expect_true(all(candidates$x2 >= -5 & candidates$x2 <= 5))
})

test_that("candidate_generator_lhs works with mixed types", {
  gen <- candidate_generator_lhs()
  search_space <- create_mixed_search_space()

  candidates <- gen(search_space, 30)
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 30)
  expect_names(names(candidates), identical.to = c("x1", "x2", "x3"))

  # Check types
  expect_true(is.numeric(candidates$x1))
  expect_true(is.integer(candidates$x2))
  expect_true(is.factor(candidates$x3) || is.character(candidates$x3))
})

test_that("candidate_generator_lhs produces space-filling samples", {
  gen <- candidate_generator_lhs()
  search_space <- create_small_search_space()

  # LHS should cover the space well - check that samples are distributed
  candidates <- gen(search_space, 10)

  # In LHS, each of 10 bins should have exactly one sample
  # So sorted values should be roughly evenly spaced
  sorted_x <- sort(candidates$x)
  diffs <- diff(sorted_x)

  # All gaps should be somewhat similar (not huge variance)
  expect_true(max(diffs) / mean(diffs) < 3)  # No gap more than 3x average
})


# =============================================================================
# candidate_generator_random Tests
# =============================================================================

test_that("candidate_generator_random returns a function", {
  gen <- candidate_generator_random()
  expect_function(gen)
})

test_that("candidate_generator_random generates correct number of candidates", {
  gen <- candidate_generator_random()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 50)
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 50)
})

test_that("candidate_generator_random generates correct columns", {
  gen <- candidate_generator_random()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 20)
  expect_names(names(candidates), identical.to = c("x1", "x2"))
})

test_that("candidate_generator_random respects bounds", {
  gen <- candidate_generator_random()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 100)
  expect_true(all(candidates$x1 >= 0 & candidates$x1 <= 1))
  expect_true(all(candidates$x2 >= -5 & candidates$x2 <= 5))
})

test_that("candidate_generator_random works with mixed types", {
  gen <- candidate_generator_random()
  search_space <- create_mixed_search_space()

  candidates <- gen(search_space, 30)
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 30)
  expect_names(names(candidates), identical.to = c("x1", "x2", "x3"))
})


# =============================================================================
# candidate_generator_sobol Tests
# =============================================================================

test_that("candidate_generator_sobol returns a function", {
  gen <- candidate_generator_sobol()
  expect_function(gen)
})

test_that("candidate_generator_sobol generates correct number of candidates", {
  gen <- candidate_generator_sobol()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 50)
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 50)
})

test_that("candidate_generator_sobol generates correct columns", {
  gen <- candidate_generator_sobol()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 20)
  expect_names(names(candidates), identical.to = c("x1", "x2"))
})

test_that("candidate_generator_sobol respects bounds", {
  gen <- candidate_generator_sobol()
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, 100)
  expect_true(all(candidates$x1 >= 0 & candidates$x1 <= 1))
  expect_true(all(candidates$x2 >= -5 & candidates$x2 <= 5))
})

test_that("candidate_generator_sobol works with mixed types", {
  gen <- candidate_generator_sobol()
  search_space <- create_mixed_search_space()

  candidates <- gen(search_space, 30)
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 30)
})


# =============================================================================
# candidate_generator_grid Tests
# =============================================================================

test_that("candidate_generator_grid returns a function", {
  gen <- candidate_generator_grid()
  expect_function(gen)
})

test_that("candidate_generator_grid accepts resolution parameter", {
  gen <- candidate_generator_grid(resolution = 5L)
  expect_function(gen)
})

test_that("candidate_generator_grid generates grid of correct size", {
  gen <- candidate_generator_grid(resolution = 5L)
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, n = 100)  # n is ignored for grids
  # 2 dimensions with resolution 5 = 5^2 = 25 points
  expect_data_table(candidates)
  expect_equal(nrow(candidates), 25)
})

test_that("candidate_generator_grid generates correct columns", {
  gen <- candidate_generator_grid(resolution = 3L)
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, n = 10)
  expect_names(names(candidates), identical.to = c("x1", "x2"))
})

test_that("candidate_generator_grid respects bounds", {
  gen <- candidate_generator_grid(resolution = 5L)
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, n = 10)
  expect_true(all(candidates$x1 >= 0 & candidates$x1 <= 1))
  expect_true(all(candidates$x2 >= -5 & candidates$x2 <= 5))
})

test_that("candidate_generator_grid includes boundary values", {
  gen <- candidate_generator_grid(resolution = 5L)
  search_space <- create_small_search_space()

  candidates <- gen(search_space, n = 10)
  expect_equal(min(candidates$x), 0)
  expect_equal(max(candidates$x), 1)
})

test_that("candidate_generator_grid works with named resolutions", {
  gen <- candidate_generator_grid(resolution = c(x1 = 3L, x2 = 5L))
  search_space <- create_numeric_search_space()

  candidates <- gen(search_space, n = 10)
  # 3 * 5 = 15 points
  expect_equal(nrow(candidates), 15)
})

test_that("candidate_generator_grid validates resolution parameter", {
  expect_error(candidate_generator_grid(resolution = 1L))  # too small
  expect_error(candidate_generator_grid(resolution = -1L))  # negative
})


# =============================================================================
# Edge Cases
# =============================================================================

test_that("generators handle n = 1", {
  search_space <- create_numeric_search_space()

  lhs_gen <- candidate_generator_lhs()
  expect_equal(nrow(lhs_gen(search_space, 1)), 1)

  random_gen <- candidate_generator_random()
  expect_equal(nrow(random_gen(search_space, 1)), 1)

  sobol_gen <- candidate_generator_sobol()
  expect_equal(nrow(sobol_gen(search_space, 1)), 1)
})

test_that("generators handle large n", {
  search_space <- create_small_search_space()

  lhs_gen <- candidate_generator_lhs()
  candidates <- lhs_gen(search_space, 1000)
  expect_equal(nrow(candidates), 1000)

  random_gen <- candidate_generator_random()
  candidates <- random_gen(search_space, 1000)
  expect_equal(nrow(candidates), 1000)
})

test_that("generators handle single-dimension search space", {
  search_space <- create_small_search_space()

  lhs_gen <- candidate_generator_lhs()
  candidates <- lhs_gen(search_space, 10)
  expect_equal(nrow(candidates), 10)
  expect_names(names(candidates), identical.to = "x")

  random_gen <- candidate_generator_random()
  candidates <- random_gen(search_space, 10)
  expect_equal(nrow(candidates), 10)
})
