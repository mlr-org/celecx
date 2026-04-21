# =============================================================================
# Tests for generate_design_grid_celecx.R
# =============================================================================

sort_design_data <- function(data) {
  # Compare designs as sets of rows because generate_design_grid_celecx() does not preserve paradox row order.
  data <- copy(data)
  if (nrow(data) > 0L) {
    setorderv(data, names(data))
  }
  data
}

expect_generate_design_grid_equal <- function(param_set,
    resolution = NULL,
    param_resolutions = NULL,
    info = NULL) {
  design_reference <- paradox::generate_design_grid(
    param_set,
    resolution = resolution,
    param_resolutions = param_resolutions
  )
  design_celecx <- generate_design_grid_celecx(
    param_set,
    resolution = resolution,
    param_resolutions = param_resolutions
  )

  expect_equal(
    sort_design_data(design_celecx$data),
    sort_design_data(design_reference$data),
    info = info
  )
}


test_that("generate_design_grid_celecx matches paradox without dependencies", {
  param_sets <- list(
    "dbl_fct" = list(
      param_set = ps(
        x = p_dbl(lower = 0, upper = 1),
        y = p_fct(levels = c("a", "b", "c"))
      ),
      resolution = 4L
    ),
    "int_duplicates" = list(
      param_set = ps(
        x = p_int(lower = 1L, upper = 3L),
        y = p_int(lower = 0L, upper = 1L)
      ),
      resolution = 5L
    ),
    "mixed_named_resolution" = list(
      param_set = ps(
        x = p_dbl(lower = 0, upper = 1),
        y = p_int(lower = 1L, upper = 4L),
        z = p_lgl()
      ),
      resolution = 2L,
      param_resolutions = c(y = 5L)
    ),
    "categorical_only" = list(
      param_set = ps(
        x = p_fct(levels = c("a", "b")),
        y = p_lgl()
      )
    ),
    "named_resolution_only" = list(
      param_set = ps(
        f = p_fct(levels = c("a", "b")),
        d = p_dbl(lower = 0, upper = 1)
      ),
      param_resolutions = c(d = 3L)
    ),
    "fixed_values" = list(
      param_set = {
        param_set <- ps(
          x = p_int(lower = 1L, upper = 3L),
          y = p_dbl(lower = -1, upper = 1),
          z = p_fct(levels = c("a", "b"))
        )
        param_set$values <- list(x = 2L, z = "b")
        param_set
      },
      resolution = 4L
    ),
    "logscale" = list(
      param_set = ps(
        x = p_dbl(lower = 1, upper = 100, logscale = TRUE),
        y = p_int(lower = 1L, upper = 9L, logscale = TRUE)
      ),
      resolution = 4L
    ),
    "zero_resolution" = list(
      param_set = ps(
        x = p_dbl(lower = 0, upper = 1),
        y = p_lgl()
      ),
      resolution = 0L
    ),
    "auto_trafo_factor" = list(
      param_set = ps(
        x = p_fct(c(a = 1, b = 2, c = 2.5), trafo = function(x) x^2),
        y = p_int(lower = 1L, upper = 2L)
      ),
      resolution = 2L
    )
  )

  iwalk(param_sets, function(case, case_id) {
    expect_generate_design_grid_equal(
      param_set = case$param_set,
      resolution = case$resolution,
      param_resolutions = case$param_resolutions,
      info = case_id
    )
  })
})

test_that("generate_design_grid_celecx matches paradox with dependencies", {
  param_sets <- list(
    "logical_gate" = list(
      param_set = ps(
        gate = p_lgl(),
        x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
      ),
      resolution = 3L
    ),
    "factor_anyof" = list(
      param_set = ps(
        mode = p_fct(levels = c("a", "b", "c")),
        x = p_int(lower = 1L, upper = 2L, depends = mode %in% c("a", "c"))
      ),
      resolution = 2L
    ),
    "chain" = list(
      param_set = ps(
        a = p_int(lower = 1L, upper = 3L),
        b = p_int(lower = 1L, upper = 3L, depends = a == 1L),
        c = p_int(lower = 1L, upper = 3L, depends = b == 1L)
      ),
      resolution = 3L
    ),
    "multiple_parents" = list(
      param_set = {
        param_set <- ps(
          a = p_int(lower = 1L, upper = 2L),
          b = p_int(lower = 1L, upper = 2L),
          c = p_int(lower = 1L, upper = 2L)
        )
        param_set$add_dep("c", "a", CondEqual(1L))
        param_set$add_dep("c", "b", CondEqual(1L))
        param_set
      },
      resolution = 2L
    ),
    "same_parent_multiple_conditions" = list(
      param_set = {
        param_set <- ps(
          mode = p_fct(levels = c("a", "b", "c")),
          x = p_int(lower = 1L, upper = 2L)
        )
        param_set$add_dep("x", "mode", CondAnyOf(c("a", "b")))
        param_set$add_dep("x", "mode", CondEqual("a"))
        param_set
      },
      resolution = 2L
    ),
    "parent_after_child_in_id_order" = list(
      param_set = {
        param_set <- ps(
          a = p_int(lower = 1L, upper = 2L),
          b = p_int(lower = 1L, upper = 2L),
          c = p_int(lower = 1L, upper = 2L)
        )
        param_set$add_dep("a", "c", CondEqual(1L))
        param_set
      },
      resolution = 2L
    ),
    "branching_with_fixed_values" = list(
      param_set = {
        param_set <- ps(
          a = p_fct(levels = c("keep", "drop")),
          b = p_int(lower = 1L, upper = 3L, depends = a == "keep"),
          c = p_dbl(lower = 0, upper = 1, depends = b %in% c(1L, 2L))
        )
        param_set$values <- list(a = "keep", b = 2L, c = 0.25)
        param_set
      },
      resolution = 3L
    ),
    "override_with_dependencies" = list(
      param_set = ps(
        a = p_int(lower = 1L, upper = 3L),
        b = p_int(lower = 1L, upper = 3L, depends = a == 1L),
        c = p_lgl(depends = b == 2L)
      ),
      resolution = 2L,
      param_resolutions = c(a = 4L, b = 5L)
    ),
    "named_resolution_only_with_dependencies" = list(
      param_set = ps(
        flag = p_lgl(),
        x = p_dbl(lower = 0, upper = 1, depends = flag == TRUE)
      ),
      param_resolutions = c(x = 4L)
    )
  )

  iwalk(param_sets, function(case, case_id) {
    expect_generate_design_grid_equal(
      param_set = case$param_set,
      resolution = case$resolution,
      param_resolutions = case$param_resolutions,
      info = case_id
    )
  })
})

test_that("generate_design_grid_celecx matches paradox error behaviour", {
  param_set_cycle <- ps(
    a = p_int(lower = 1L, upper = 2L),
    b = p_int(lower = 1L, upper = 2L)
  )
  param_set_cycle$add_dep("a", "b", CondEqual(1L))
  param_set_cycle$add_dep("b", "a", CondEqual(1L))

  error_reference <- tryCatch(
    {
      paradox::generate_design_grid(param_set_cycle, resolution = 2L)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  error_celecx <- tryCatch(
    {
      generate_design_grid_celecx(param_set_cycle, resolution = 2L)
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  expect_identical(error_celecx, error_reference)

  param_set_missing <- ps(
    x = p_dbl(lower = 0, upper = 1),
    y = p_int(lower = 1L, upper = 3L)
  )
  expect_error(
    generate_design_grid_celecx(param_set_missing, param_resolutions = c(x = 3L)),
    "Resolution settings missing"
  )
})

test_that("generate_design_grid_celecx handles large duplicate collapse", {
  param_set <- ps(
    x1 = p_int(lower = 0L, upper = 1L),
    x2 = p_int(lower = 0L, upper = 1L),
    x3 = p_int(lower = 0L, upper = 1L),
    x4 = p_int(lower = 0L, upper = 1L),
    x5 = p_int(lower = 0L, upper = 1L),
    x6 = p_int(lower = 0L, upper = 1L)
  )

  design <- generate_design_grid_celecx(param_set, resolution = 100L)

  expect_equal(nrow(design$data), 64L)
  expect_true(all(map_lgl(design$transpose(), param_set$test)))
})

test_that("generate_design_grid_celecx handles deep dependency chains", {
  param_list <- named_list(sprintf("x%i", seq_len(8L)))
  for (i in seq_along(param_list)) {
    param_list[[i]] <- p_int(lower = 1L, upper = 10L)
  }
  param_set <- ParamSet$new(param_list)
  for (i in 2:8) {
    param_set$add_dep(
      sprintf("x%i", i),
      sprintf("x%i", i - 1L),
      CondEqual(1L)
    )
  }

  design <- generate_design_grid_celecx(param_set, resolution = 10L)

  expect_equal(nrow(design$data), 73L)
  expect_true(all(map_lgl(design$transpose(), param_set$test)))
})

test_that("generate_design_grid_celecx enforces upper_limit without dependencies", {
  param_set <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1),
    x3 = p_dbl(lower = 0, upper = 1),
    x4 = p_dbl(lower = 0, upper = 1),
    x5 = p_dbl(lower = 0, upper = 1)
  )

  expect_error(
    generate_design_grid_celecx(param_set, resolution = 10L, upper_limit = 99999L),
    "upper_limit"
  )
})

test_that("generate_design_grid_celecx upper_limit uses realized and dependency-pruned size", {
  param_set_realized <- ps(
    x = p_int(lower = 1L, upper = 3L),
    y = p_fct(levels = c("a", "b"))
  )
  design <- generate_design_grid_celecx(param_set_realized, resolution = 10L, upper_limit = 6L)
  expect_equal(nrow(design$data), 6L)
  expect_error(
    generate_design_grid_celecx(param_set_realized, resolution = 10L, upper_limit = 5L),
    "upper_limit"
  )

  param_set_dep <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
  )
  design <- generate_design_grid_celecx(param_set_dep, resolution = 10L, upper_limit = 4L)
  expect_equal(nrow(design$data), 4L)
  expect_error(
    generate_design_grid_celecx(param_set_dep, resolution = 10L, upper_limit = 3L),
    "upper_limit"
  )
})
