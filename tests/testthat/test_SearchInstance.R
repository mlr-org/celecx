
# Tests for SearchInstance.R

# =============================================================================
# Helper functions
# =============================================================================

# Create a simple test dataset
create_test_dataset <- function(n = 20) {
  set.seed(42)
  data.table(
    x1 = runif(n, 0, 1),
    x2 = runif(n, 0, 1),
    y = rnorm(n)
  )
}

# Create a simple ObjectiveDataset for testing
# Note: ObjectiveDataset inherits from bbotk Objective, which validates codomain
# through Codomain$new(). This only accepts "minimize" or "maximize" tags.
create_test_objective <- function(codomain_tag = "minimize") {
  dt <- create_test_dataset()
  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = codomain_tag))

  ObjectiveDataset$new(dataset = dt, domain = domain, codomain = codomain)
}

# Create a simple function-based objective for testing
# Note: bbotk's ObjectiveRFun validates codomain through Codomain$new() which
# only accepts "minimize" or "maximize" tags. "learn" tags are NOT supported
# for standard bbotk objectives.
create_function_objective <- function(codomain_tag = "minimize") {
  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = codomain_tag))

  ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x1^2 + xs$x2^2),
    domain = domain,
    codomain = codomain
  )
}


# =============================================================================
# Initialization Tests
# =============================================================================

test_that("SearchInstance initializes with basic objective", {
  objective <- create_function_objective()
  terminator <- trm("none")

  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  expect_r6(instance, "SearchInstance")
  expect_r6(instance$objective, "Objective")
  expect_r6(instance$search_space, "ParamSet")
  expect_r6(instance$archive, "ArchiveBatch")
  expect_r6(instance$terminator, "Terminator")
})

test_that("SearchInstance uses objective domain as search_space by default", {
  objective <- create_function_objective()
  terminator <- trm("none")

  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  expect_equal(instance$search_space$ids(), objective$domain$ids())
  expect_equal(instance$search_space$length, objective$domain$length)
})

test_that("SearchInstance accepts custom search_space", {
  objective <- create_function_objective()
  custom_search_space <- ps(x1 = p_dbl(lower = 0.2, upper = 0.8))
  terminator <- trm("none")

  instance <- SearchInstance$new(
    objective = objective,
    search_space = custom_search_space,
    terminator = terminator
  )

  expect_equal(instance$search_space$ids(), "x1")
  expect_equal(instance$search_space$lower[["x1"]], 0.2)
})

test_that("SearchInstance clone deep clones search_space", {
  objective <- create_function_objective()
  custom_search_space <- ps(x1 = p_dbl(lower = 0.2, upper = 0.8))
  terminator <- trm("none")

  instance <- SearchInstance$new(
    objective = objective,
    search_space = custom_search_space,
    terminator = terminator
  )

  # Clone the instance
  cloned <- instance$clone(deep = TRUE)

  # Cloned instance's search_space should be independent
  expect_false(identical(instance$search_space, cloned$search_space))
})

test_that("SearchInstance accepts maximize tag in codomain", {
  objective <- create_function_objective(codomain_tag = "maximize")
  terminator <- trm("none")

  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  expect_r6(instance, "SearchInstance")
  expect_equal(codomain_goal(instance$objective$codomain), "optimize")
})

test_that("SearchInstance check_values defaults to TRUE (validates inputs)", {
  objective <- create_function_objective()
  terminator <- trm("none")

  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # check_values = TRUE means out-of-bounds values should error
  xdt <- data.table(x1 = 1.5, x2 = 0.2)
  expect_error(instance$eval_batch(xdt))
})

test_that("SearchInstance check_values = FALSE skips validation", {
  # Create objective that also skips validation
  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = "minimize"))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x1^2 + xs$x2^2),
    domain = domain,
    codomain = codomain,
    check_values = FALSE
  )

  terminator <- trm("none")
  instance <- SearchInstance$new(
    objective = objective,
    terminator = terminator,
    check_values = FALSE
  )

  # Out of bounds values should work when check_values = FALSE
  xdt <- data.table(x1 = 1.5, x2 = 0.2)
  ydt <- instance$eval_batch(xdt)
  expect_data_table(ydt)
})

test_that("SearchInstance rejects invalid objective", {
  terminator <- trm("none")

  expect_error(
    SearchInstance$new(objective = "not_an_objective", terminator = terminator),
    "Assertion.*failed"
  )
})

test_that("SearchInstance rejects invalid terminator", {
  objective <- create_function_objective()

  expect_error(
    SearchInstance$new(objective = objective, terminator = "not_a_terminator"),
    "Assertion.*failed"
  )
})


# =============================================================================
# Archive Tests
# =============================================================================

test_that("SearchInstance creates ArchiveBatch with correct search_space", {
  objective <- create_function_objective()
  terminator <- trm("none")

  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  expect_equal(instance$archive$search_space$ids(), objective$domain$ids())
})

test_that("SearchInstance creates ArchiveBatch with correct codomain", {
  objective <- create_function_objective()
  terminator <- trm("none")

  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # Archive's codomain should have the target
  expect_equal(instance$archive$codomain$target_ids, "y")
  expect_equal(instance$archive$codomain$direction, c(y = 1L))
})

test_that("SearchInstance accepts pre-existing archive", {
  objective <- create_function_objective()
  search_space <- objective$domain$clone(deep = TRUE)

  pre_archive <- ArchiveBatch$new(
    search_space = search_space,
    codomain = objective$codomain
  )

  terminator <- trm("none")

  instance <- SearchInstance$new(
    objective = objective,
    terminator = terminator,
    archive = pre_archive
  )

  expect_identical(instance$archive, pre_archive)
})


# =============================================================================
# eval_batch Tests
# =============================================================================

test_that("SearchInstance eval_batch evaluates points", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = c(0.1, 0.2), x2 = c(0.3, 0.4))
  ydt <- instance$eval_batch(xdt)

  expect_data_table(ydt, nrows = 2)
  expect_true("y" %in% names(ydt))
})

test_that("SearchInstance eval_batch adds to archive", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  expect_equal(instance$n_evals, 0)
  expect_equal(instance$archive$n_batch, 0)

  xdt <- data.table(x1 = c(0.1, 0.2), x2 = c(0.3, 0.4))
  instance$eval_batch(xdt)

  expect_equal(instance$n_evals, 2)
  expect_equal(instance$archive$n_batch, 1)
})

test_that("SearchInstance eval_batch increments batch number", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt1 <- data.table(x1 = 0.1, x2 = 0.2)
  xdt2 <- data.table(x1 = 0.3, x2 = 0.4)

  instance$eval_batch(xdt1)
  expect_equal(instance$archive$n_batch, 1)

  instance$eval_batch(xdt2)
  expect_equal(instance$archive$n_batch, 2)
  expect_equal(instance$n_evals, 2)
})

test_that("SearchInstance eval_batch returns ydt invisibly", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  result <- withVisible(instance$eval_batch(xdt))

  expect_false(result$visible)
  expect_data_table(result$value)
})

test_that("SearchInstance eval_batch validates xdt against search_space", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(
    objective = objective,
    terminator = terminator,
    check_values = TRUE
  )

  # Out of bounds values
  xdt <- data.table(x1 = 1.5, x2 = 0.2)

  expect_error(instance$eval_batch(xdt))
})

test_that("SearchInstance eval_batch skips validation when check_values = FALSE", {
  # Create objective that also skips validation
  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = "minimize"))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x1^2 + xs$x2^2),
    domain = domain,
    codomain = codomain,
    check_values = FALSE  # Objective also skips validation
  )

  terminator <- trm("none")
  instance <- SearchInstance$new(
    objective = objective,
    terminator = terminator,
    check_values = FALSE
  )

  # Out of bounds values - neither SearchInstance nor objective validates
  xdt <- data.table(x1 = 1.5, x2 = 0.2)

  # This test verifies that when both skip validation, it works
  ydt <- instance$eval_batch(xdt)
  expect_data_table(ydt)
})

test_that("SearchInstance eval_batch requires search_space columns", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # Missing x2 column
  xdt <- data.table(x1 = 0.1)

  expect_error(instance$eval_batch(xdt), "must include")
})

test_that("SearchInstance eval_batch allows extra columns in xdt", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # Extra column 'extra'
  xdt <- data.table(x1 = 0.1, x2 = 0.2, extra = "foo")
  ydt <- instance$eval_batch(xdt)

  expect_data_table(ydt, nrows = 1)
  # Extra column should be in archive
  expect_true("extra" %in% names(instance$archive$data))
})


# =============================================================================
# Termination Tests
# =============================================================================

test_that("SearchInstance is_terminated returns FALSE initially", {
  objective <- create_function_objective()
  terminator <- trm("evals", n_evals = 10)
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  expect_false(instance$is_terminated)
})

test_that("SearchInstance is_terminated returns TRUE when budget exhausted", {
  objective <- create_function_objective()
  terminator <- trm("evals", n_evals = 2)
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = c(0.1, 0.2), x2 = c(0.3, 0.4))
  instance$eval_batch(xdt)

  expect_true(instance$is_terminated)
})

test_that("SearchInstance eval_batch throws search_terminated_error when terminated", {
  objective <- create_function_objective()
  terminator <- trm("evals", n_evals = 1)
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # First batch - should succeed
  xdt1 <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt1)

  # Second batch - should throw terminated error
  xdt2 <- data.table(x1 = 0.3, x2 = 0.4)

  expect_error(
    instance$eval_batch(xdt2),
    class = "search_terminated_error"
  )
})

test_that("search_terminated_error can be caught", {
  objective <- create_function_objective()
  terminator <- trm("evals", n_evals = 1)
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt)

  # Should be able to catch the error
  caught <- FALSE
  tryCatch({
    instance$eval_batch(xdt)
  }, search_terminated_error = function(e) {
    caught <<- TRUE
  })

  expect_true(caught)
})

test_that("search_terminated_error contains informative message", {
  objective <- create_function_objective()
  terminator <- trm("evals", n_evals = 1)
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  err <- search_terminated_error(instance)

  expect_true(grepl("Search", err$message))
  expect_true(grepl("terminated", err$message))
  expect_true(grepl(objective$id, err$message))
})


# =============================================================================
# clear Tests
# =============================================================================

test_that("SearchInstance clear resets archive", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt)
  expect_equal(instance$n_evals, 1)

  instance$clear()
  expect_equal(instance$n_evals, 0)
  expect_equal(instance$archive$n_batch, 0)
})

test_that("SearchInstance clear returns self invisibly", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  result <- withVisible(instance$clear())

  expect_false(result$visible)
  expect_identical(result$value, instance)
})


# =============================================================================
# Active Bindings Tests
# =============================================================================

test_that("SearchInstance archive data contains evaluated points", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt)

  data <- instance$archive$data
  expect_data_table(data, nrows = 1)
  expect_true("x1" %in% names(data))
  expect_true("x2" %in% names(data))
  expect_true("y" %in% names(data))
  expect_true("batch_nr" %in% names(data))
})

test_that("SearchInstance n_evals matches archive", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = c(0.1, 0.2, 0.3), x2 = c(0.4, 0.5, 0.6))
  instance$eval_batch(xdt)

  expect_equal(instance$n_evals, 3)
  expect_equal(instance$archive$n_evals, 3)
})

test_that("SearchInstance archive n_batch tracks batches", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt)
  instance$eval_batch(xdt)

  expect_equal(instance$archive$n_batch, 2)
})


# =============================================================================
# Print Tests
# =============================================================================

test_that("SearchInstance print shows basic info", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  output <- capture.output(instance$print())

  expect_true(any(grepl("SearchInstance", output)))
  expect_true(any(grepl("Objective", output)))
  expect_true(any(grepl("Search space", output)))
  expect_true(any(grepl("Terminator", output)))
})

test_that("SearchInstance print shows evaluations and batches", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt)

  output <- capture.output(instance$print())

  expect_true(any(grepl("Evaluations: 1", output)))
  expect_true(any(grepl("Batches: 1", output)))
})

test_that("SearchInstance print shows goal", {
  objective <- create_function_objective(codomain_tag = "minimize")
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  output <- capture.output(instance$print())

  expect_true(any(grepl("Goal: optimize", output)))
})


# =============================================================================
# Clone Tests
# =============================================================================

test_that("SearchInstance clone creates independent copy", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.1, x2 = 0.2)
  instance$eval_batch(xdt)

  clone <- instance$clone(deep = TRUE)

  # Both should have same data
  expect_equal(clone$n_evals, 1)

  # Add to original
  instance$eval_batch(xdt)

  # Clone should be unchanged
  expect_equal(instance$n_evals, 2)
  expect_equal(clone$n_evals, 1)
})

test_that("SearchInstance clone deep clones objective", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  clone <- instance$clone(deep = TRUE)

  expect_false(identical(instance$objective, clone$objective))
})

test_that("SearchInstance clone deep clones terminator", {
  objective <- create_function_objective()
  terminator <- trm("evals", n_evals = 10)
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  clone <- instance$clone(deep = TRUE)

  # Modify original terminator
  terminator$param_set$values$n_evals <- 100

  # Clone's terminator should be independent
  expect_false(identical(instance$terminator, clone$terminator))
})


# =============================================================================
# search_instance convenience constructor Tests
# =============================================================================

test_that("search_instance creates SearchInstance", {
  objective <- create_function_objective()

  instance <- search_instance(
    objective = objective,
    terminator = trm("none")
  )

  expect_r6(instance, "SearchInstance")
})

test_that("search_instance rejects string terminator", {
  objective <- create_function_objective()

  expect_error(
    search_instance(
      objective = objective,
      terminator = "none"
    )
  )
})

test_that("search_instance passes additional arguments", {
  # Create objective that skips validation
  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = "minimize"))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x1^2 + xs$x2^2),
    domain = domain,
    codomain = codomain,
    check_values = FALSE
  )

  instance <- search_instance(
    objective = objective,
    terminator = bbotk::TerminatorNone$new(),
    check_values = FALSE
  )

  # Should be able to eval out-of-bounds when check_values = FALSE
  xdt <- data.table(x1 = 1.5, x2 = 0.2)
  ydt <- instance$eval_batch(xdt)
  expect_data_table(ydt)
})


# =============================================================================
# Integration with ObjectiveDataset Tests
# =============================================================================

test_that("SearchInstance works with ObjectiveDataset", {
  objective <- create_test_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # Get some points from the dataset
  sample_points <- objective$data[1:3, .(x1, x2)]
  ydt <- instance$eval_batch(sample_points)

  expect_data_table(ydt, nrows = 3)
  expect_equal(instance$n_evals, 3)
})


# =============================================================================
# Edge Cases
# =============================================================================

test_that("SearchInstance handles single evaluation", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  xdt <- data.table(x1 = 0.5, x2 = 0.5)
  ydt <- instance$eval_batch(xdt)

  expect_equal(nrow(ydt), 1)
  expect_equal(instance$n_evals, 1)
})

test_that("SearchInstance handles large batch", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  n <- 100
  xdt <- data.table(
    x1 = runif(n, 0, 1),
    x2 = runif(n, 0, 1)
  )
  ydt <- instance$eval_batch(xdt)

  expect_equal(nrow(ydt), n)
  expect_equal(instance$n_evals, n)
})

test_that("SearchInstance with TerminatorNone never terminates", {
  objective <- create_function_objective()
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  # Evaluate many batches
  xdt <- data.table(x1 = 0.5, x2 = 0.5)
  for (i in 1:10) {
    instance$eval_batch(xdt)
  }

  expect_false(instance$is_terminated)
  expect_equal(instance$archive$n_batch, 10)
})


# =============================================================================
# Codomain Goal Tests
# =============================================================================

test_that("SearchInstance correctly reports optimize goal for minimize", {
  objective <- create_function_objective(codomain_tag = "minimize")
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  output <- capture.output(instance$print())
  expect_true(any(grepl("Goal: optimize", output)))
})

test_that("SearchInstance correctly reports optimize goal for maximize", {
  objective <- create_function_objective(codomain_tag = "maximize")
  terminator <- trm("none")
  instance <- SearchInstance$new(objective = objective, terminator = terminator)

  output <- capture.output(instance$print())
  expect_true(any(grepl("Goal: optimize", output)))
})
