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

expect_predict_grid_size_equal <- function(search_space,
    resolution,
    info = NULL) {
  design_reference <- if (is.null(names(resolution))) {
    generate_design_grid(search_space, resolution = resolution[[1L]])
  } else {
    generate_design_grid(search_space, param_resolutions = resolution)
  }
  design_celecx <- if (is.null(names(resolution))) {
    generate_design_grid_celecx(search_space, resolution = resolution[[1L]])
  } else {
    generate_design_grid_celecx(search_space, param_resolutions = resolution)
  }

  predicted_size <- if (is.null(names(resolution))) {
    predict_grid_size(search_space, resolution = resolution)
  } else {
    predict_grid_size(search_space, param_resolutions = resolution)
  }

  expect_equal(predicted_size, nrow(design_reference$data), info = info)
  expect_equal(predicted_size, nrow(design_celecx$data), info = info)
}


expect_predict_grid_size_upper_limited <- function(search_space,
    resolution,
    upper_limits = NULL,
    info = NULL) {
  design_reference <- if (is.null(names(resolution))) {
    generate_design_grid(search_space, resolution = resolution[[1L]])
  } else {
    generate_design_grid(search_space, param_resolutions = resolution)
  }
  exact_size <- nrow(design_reference$data)
  if (is.null(upper_limits)) {
    upper_limits <- unique(c(0, max(exact_size - 1L, 0L), exact_size, exact_size + 1L))
  }

  for (upper_limit in upper_limits) {
    predicted_size <- if (is.null(names(resolution))) {
      predict_grid_size(search_space, resolution = resolution, upper_limit = upper_limit)
    } else {
      predict_grid_size(search_space, param_resolutions = resolution, upper_limit = upper_limit)
    }

    expect_equal(
      predicted_size,
      min(exact_size, upper_limit),
      info = paste(info, "upper_limit =", upper_limit)
    )
  }
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
  expect_no_error(candidate_generator_grid(resolution = 1L))
  expect_error(candidate_generator_grid(resolution = 0L))  # zero
  expect_error(candidate_generator_grid(resolution = -1L))  # negative
})

test_that("candidate_generator_grid with resolution 1 still expands categorical params fully", {
  gen <- candidate_generator_grid(resolution = 1L)
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 1),
    mode = p_fct(levels = c("a", "b", "c"))
  )

  candidates <- gen(search_space, n = 10)
  expect_equal(nrow(candidates), 3L)
})

test_that("predict_grid_size matches generated grids without dependencies", {
  cases <- list(
    "dbl" = list(
      search_space = ps(x = p_dbl(lower = 0, upper = 1)),
      resolution = 5L
    ),
    "constant_dbl" = list(
      search_space = ps(x = p_dbl(lower = 1, upper = 1)),
      resolution = 7L
    ),
    "int" = list(
      search_space = ps(x = p_int(lower = 1L, upper = 3L)),
      resolution = 5L
    ),
    "categorical_only" = list(
      search_space = ps(
        x = p_fct(levels = c("a", "b", "c")),
        y = p_lgl()
      ),
      resolution = 10L
    ),
    "resolution_one_mixed" = list(
      search_space = ps(
        x = p_dbl(lower = 0, upper = 1),
        y = p_fct(levels = c("a", "b", "c"))
      ),
      resolution = 1L
    ),
    "mixed_named_resolution" = list(
      search_space = ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_int(lower = 1L, upper = 3L),
        x3 = p_fct(levels = c("a", "b", "c", "d")),
        x4 = p_lgl()
      ),
      resolution = c(x1 = 4L, x2 = 8L)
    ),
    "logscale" = list(
      search_space = ps(
        x = p_dbl(lower = 1, upper = 100, logscale = TRUE),
        y = p_int(lower = 1L, upper = 9L, logscale = TRUE)
      ),
      resolution = 4L
    ),
    "duplicate_collapse" = list(
      search_space = ps(
        x1 = p_int(lower = 0L, upper = 1L),
        x2 = p_int(lower = 0L, upper = 1L),
        x3 = p_int(lower = 0L, upper = 1L),
        x4 = p_int(lower = 0L, upper = 1L)
      ),
      resolution = 10L
    ),
    "fixed_values" = list(
      search_space = {
        search_space <- ps(
          x = p_int(lower = 1L, upper = 3L),
          y = p_dbl(lower = -1, upper = 1),
          z = p_fct(levels = c("a", "b"))
        )
        search_space$values <- list(x = 2L, z = "b")
        search_space
      },
      resolution = 4L
    )
  )

  iwalk(cases, function(case, case_id) {
    expect_predict_grid_size_equal(
      search_space = case$search_space,
      resolution = case$resolution,
      info = case_id
    )
  })
})

test_that("predict_grid_size matches generated grids with dependencies", {
  cases <- list(
    "logical_gate" = list(
      search_space = ps(
        gate = p_lgl(),
        x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
      ),
      resolution = 5L
    ),
    "factor_anyof" = list(
      search_space = ps(
        mode = p_fct(levels = c("a", "b", "c")),
        x = p_int(lower = 1L, upper = 2L, depends = mode %in% c("a", "c"))
      ),
      resolution = 3L
    ),
    "chain" = list(
      search_space = ps(
        a = p_int(lower = 1L, upper = 5L),
        b = p_int(lower = 1L, upper = 5L, depends = a == 1L),
        c = p_int(lower = 1L, upper = 5L, depends = b == 1L),
        d = p_int(lower = 1L, upper = 5L, depends = c == 1L),
        e = p_int(lower = 1L, upper = 5L, depends = d == 1L)
      ),
      resolution = 5L
    ),
    "multiple_parents" = list(
      search_space = {
        search_space <- ps(
          a = p_int(lower = 1L, upper = 2L),
          b = p_int(lower = 1L, upper = 2L),
          c = p_int(lower = 1L, upper = 2L)
        )
        search_space$add_dep("c", "a", CondEqual(1L))
        search_space$add_dep("c", "b", CondEqual(1L))
        search_space
      },
      resolution = 3L
    ),
    "same_parent_multiple_conditions" = list(
      search_space = {
        search_space <- ps(
          mode = p_fct(levels = c("a", "b", "c")),
          x = p_int(lower = 1L, upper = 3L)
        )
        search_space$add_dep("x", "mode", CondAnyOf(c("a", "b")))
        search_space$add_dep("x", "mode", CondEqual("a"))
        search_space
      },
      resolution = 4L
    ),
    "parent_after_child_in_id_order" = list(
      search_space = {
        search_space <- ps(
          a = p_int(lower = 1L, upper = 2L),
          b = p_int(lower = 1L, upper = 2L),
          c = p_int(lower = 1L, upper = 2L)
        )
        search_space$add_dep("a", "c", CondEqual(1L))
        search_space
      },
      resolution = 3L
    ),
    "override_with_dependencies" = list(
      search_space = ps(
        a = p_int(lower = 1L, upper = 3L),
        b = p_int(lower = 1L, upper = 3L, depends = a == 1L),
        c = p_lgl(depends = b == 2L)
      ),
      resolution = c(a = 4L, b = 5L)
    ),
    "named_resolution_only_with_dependencies" = list(
      search_space = ps(
        flag = p_lgl(),
        x = p_dbl(lower = 0, upper = 1, depends = flag == TRUE)
      ),
      resolution = c(x = 4L)
    )
  )

  iwalk(cases, function(case, case_id) {
    expect_predict_grid_size_equal(
      search_space = case$search_space,
      resolution = case$resolution,
      info = case_id
    )
  })
})

test_that("predict_grid_size upper_limit clips cheap sizes", {
  cases <- list(
    "dbl" = list(
      search_space = ps(
        x1 = p_dbl(lower = 0, upper = 1),
        x2 = p_dbl(lower = -1, upper = 1)
      ),
      resolution = 5L
    ),
    "mixed_named_resolution" = list(
      search_space = ps(
        x = p_dbl(lower = 0, upper = 1),
        y = p_int(lower = 1L, upper = 3L),
        z = p_fct(levels = c("a", "b"))
      ),
      resolution = c(x = 4L, y = 5L)
    ),
    "resolution_one_mixed" = list(
      search_space = ps(
        x = p_dbl(lower = 0, upper = 1),
        y = p_fct(levels = c("a", "b", "c"))
      ),
      resolution = 1L
    )
  )

  iwalk(cases, function(case, case_id) {
    expect_predict_grid_size_upper_limited(
      search_space = case$search_space,
      resolution = case$resolution,
      info = case_id
    )
  })
})

test_that("predict_grid_size upper_limit clips dependency-aware sizes without off-by-one errors", {
  cases <- list(
    "logical_gate" = list(
      search_space = ps(
        gate = p_lgl(),
        x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
      ),
      resolution = 5L
    ),
    "multiple_parents" = list(
      search_space = {
        search_space <- ps(
          a = p_int(lower = 1L, upper = 2L),
          b = p_int(lower = 1L, upper = 2L),
          c = p_int(lower = 1L, upper = 2L)
        )
        search_space$add_dep("c", "a", CondEqual(1L))
        search_space$add_dep("c", "b", CondEqual(1L))
        search_space
      },
      resolution = 3L
    ),
    "same_parent_multiple_conditions" = list(
      search_space = {
        search_space <- ps(
          mode = p_fct(levels = c("a", "b", "c")),
          x = p_int(lower = 1L, upper = 3L)
        )
        search_space$add_dep("x", "mode", CondAnyOf(c("a", "b")))
        search_space$add_dep("x", "mode", CondEqual("a"))
        search_space
      },
      resolution = 4L
    ),
    "parent_after_child_in_id_order" = list(
      search_space = {
        search_space <- ps(
          a = p_int(lower = 1L, upper = 2L),
          b = p_int(lower = 1L, upper = 2L),
          c = p_int(lower = 1L, upper = 2L)
        )
        search_space$add_dep("a", "c", CondEqual(1L))
        search_space
      },
      resolution = 3L
    ),
    "override_with_dependencies" = list(
      search_space = ps(
        a = p_int(lower = 1L, upper = 3L),
        b = p_int(lower = 1L, upper = 3L, depends = a == 1L),
        c = p_lgl(depends = b == 2L)
      ),
      resolution = c(a = 4L, b = 5L)
    )
  )

  iwalk(cases, function(case, case_id) {
    expect_predict_grid_size_upper_limited(
      search_space = case$search_space,
      resolution = case$resolution,
      info = case_id
    )
  })
})

test_that("predict_grid_size validates named resolutions for numeric parameters", {
  search_space <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_int(lower = 1L, upper = 3L),
    x3 = p_fct(levels = c("a", "b"))
  )

  expect_error(
    predict_grid_size(search_space, param_resolutions = c(x1 = 4L)),
    "Resolution settings missing"
  )
})

test_that("candidate_generator_grid checks limit before generating the grid", {
  gen <- candidate_generator_grid(resolution = 10L, limit = 99999L)
  search_space <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1),
    x3 = p_dbl(lower = 0, upper = 1),
    x4 = p_dbl(lower = 0, upper = 1),
    x5 = p_dbl(lower = 0, upper = 1)
  )

  expect_error(gen(search_space, n = 1L), "limit")
})

test_that("candidate_generator_grid limit uses realized grid size", {
  gen_ok <- candidate_generator_grid(resolution = 10L, limit = 6L)
  gen_fail <- candidate_generator_grid(resolution = 10L, limit = 5L)
  search_space <- ps(
    x = p_int(lower = 1L, upper = 3L),
    y = p_fct(levels = c("a", "b"))
  )

  expect_equal(nrow(gen_ok(search_space, n = 1L)), 6)
  expect_error(gen_fail(search_space, n = 1L), "limit")
})

test_that("candidate_generator_grid limit uses dependency-pruned size", {
  gen_ok <- candidate_generator_grid(resolution = 10L, limit = 4L)
  gen_fail <- candidate_generator_grid(resolution = 10L, limit = 3L)
  search_space <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
  )

  expect_equal(nrow(gen_ok(search_space, n = 1L)), 4L)
  expect_error(gen_fail(search_space, n = 1L), "limit")
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
