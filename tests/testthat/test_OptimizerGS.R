# =============================================================================
# Tests for OptimizerGS.R
# =============================================================================

# Helper: create a test pool dataset + objective
make_gs_test_setup <- function(n = 20, d = 2, seed = 42) {
  set.seed(seed)
  x <- matrix(runif(n * d), nrow = n, ncol = d)
  colnames(x) <- paste0("x", seq_len(d))
  y <- rowSums(x^2)
  dt <- as.data.table(x)
  dt$y <- y

  domain <- do.call(ps, set_names(
    lapply(seq_len(d), function(i) p_dbl(lower = 0, upper = 1)),
    paste0("x", seq_len(d))
  ))
  codomain <- ps(y = p_dbl(tags = "learn"))

  objective <- ObjectiveDataset$new(
    dataset = dt, domain = domain, codomain = codomain
  )

  list(objective = objective, dt = dt, domain = domain, codomain = codomain)
}

test_that("OptimizerGS with GSx runs end-to-end", {
  setup <- make_gs_test_setup(n = 20, d = 2)

  optimizer <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer$param_set$set_values(n_init = 3L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 8)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 8L)
  expect_data_table(instance$archive$data, nrows = 8)
})

test_that("OptimizerGS with GSy runs end-to-end", {
  setup <- make_gs_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerGS$new(
    scoring = scoring_gsy(),
    learner = learner
  )
  optimizer$param_set$set_values(n_init = 3L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 8)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 8L)
})

test_that("OptimizerGS with iGS runs end-to-end", {
  setup <- make_gs_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerGS$new(
    scoring = scoring_igs(),
    learner = learner
  )
  optimizer$param_set$set_values(n_init = 3L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 8)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 8L)
})

test_that("OptimizerGS with random scoring runs end-to-end", {
  setup <- make_gs_test_setup(n = 20, d = 2)

  optimizer <- OptimizerGS$new(scoring = scoring_random())
  optimizer$param_set$set_values(n_init = 3L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 8)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 8L)
})

test_that("OptimizerGS random and QBC default to random initialization", {
  setup <- make_gs_test_setup(n = 12, d = 2, seed = 123)
  expected_idx <- {
    set.seed(7)
    sample.int(nrow(setup$dt), 3L, replace = FALSE)
  }
  expected_xdt <- setup$dt[expected_idx, .(x1, x2)]

  run_init_only <- function(optimizer, seed) {
    set.seed(seed)
    instance <- SearchInstance$new(
      objective = setup$objective,
      terminator = trm("evals", n_evals = 3)
    )

    tryCatch(
      optimizer$optimize(instance),
      search_terminated_error = function(e) NULL
    )

    instance$archive$data[, .(x1, x2)]
  }

  random_optimizer <- OptimizerGS$new(scoring = scoring_random())
  random_optimizer$param_set$set_values(n_init = 3L)
  expect_equal(run_init_only(random_optimizer, seed = 7), expected_xdt)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)
  qbc_optimizer <- OptimizerGS$new(
    scoring = scoring_qbc(k_qbc = 3L),
    learner = learner
  )
  qbc_optimizer$param_set$set_values(n_init = 3L)
  expect_equal(run_init_only(qbc_optimizer, seed = 7), expected_xdt)
})

test_that("OptimizerGS init_method can override the default initialization", {
  setup <- make_gs_test_setup(n = 12, d = 2, seed = 123)
  expected_idx <- gsx_init(as.matrix(setup$dt[, .(x1, x2)]), n_init = 3L)
  expected_xdt <- setup$dt[expected_idx, .(x1, x2)]

  optimizer <- OptimizerGS$new(scoring = scoring_random())
  optimizer$param_set$set_values(n_init = 3L, init_method = "gsx")

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 3)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$data[, .(x1, x2)], expected_xdt)
})

test_that("OptimizerGS init_method kmeans works", {
  setup <- make_gs_test_setup(n = 20, d = 2)

  optimizer <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer$param_set$set_values(n_init = 3L, init_method = "kmeans")

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 8)
  )

  set.seed(42)
  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 8L)
  expect_data_table(instance$archive$data, nrows = 8)
})

test_that("OptimizerGS n_init defaults to dimension d", {
  setup <- make_gs_test_setup(n = 20, d = 3)

  optimizer <- OptimizerGS$new(scoring = scoring_gsx())
  # n_init not set -> should default to d=3

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 6)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  # First batch should have 3 points (d=3), then 3 more sequential
  expect_equal(instance$archive$n_evals, 6L)
  # First batch has batch_nr = 1 with d=3 evals
  batch_sizes <- instance$archive$data[, .N, by = batch_nr]
  expect_equal(batch_sizes$N[1], 3L)  # init batch
  # Remaining batches should be 1 each
  expect_true(all(batch_sizes$N[-1] == 1L))
})

test_that("OptimizerGS terminates cleanly when budget exhausted", {
  setup <- make_gs_test_setup(n = 50, d = 2)

  optimizer <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer$param_set$set_values(n_init = 2L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 5)
  )

  # Should not error
  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 5L)
})

test_that("OptimizerGS with 1D pool works", {
  setup <- make_gs_test_setup(n = 15, d = 1)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerGS$new(
    scoring = scoring_gsy(),
    learner = learner
  )
  optimizer$param_set$set_values(n_init = 2L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 5)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 5L)
})

test_that("OptimizerGS GSx is deterministic", {
  run_gsx <- function(seed) {
    set.seed(seed)
    setup <- make_gs_test_setup(n = 20, d = 2, seed = 123)
    optimizer <- OptimizerGS$new(scoring = scoring_gsx())
    optimizer$param_set$set_values(n_init = 3L)
    instance <- SearchInstance$new(
      objective = setup$objective,
      terminator = trm("evals", n_evals = 7)
    )
    tryCatch(
      optimizer$optimize(instance),
      search_terminated_error = function(e) NULL
    )
    instance$archive$data
  }

  r1 <- run_gsx(1)
  r2 <- run_gsx(2)
  # GSx is deterministic (no randomness), so results should be identical
  domain_cols <- c("x1", "x2")
  expect_equal(r1[, domain_cols, with = FALSE], r2[, domain_cols, with = FALSE])
})

test_that("OptimizerGS GSx batching matches sequential selection order", {
  setup <- make_gs_test_setup(n = 20, d = 2, seed = 123)

  optimizer_seq <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer_seq$param_set$set_values(n_init = 3L, batch_size = 1L)
  instance_seq <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 9)
  )
  tryCatch(
    optimizer_seq$optimize(instance_seq),
    search_terminated_error = function(e) NULL
  )

  optimizer_batch <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer_batch$param_set$set_values(n_init = 3L, batch_size = 2L)
  instance_batch <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 9)
  )
  tryCatch(
    optimizer_batch$optimize(instance_batch),
    search_terminated_error = function(e) NULL
  )

  domain_cols <- c("x1", "x2")
  expect_equal(
    instance_batch$archive$data[, domain_cols, with = FALSE],
    instance_seq$archive$data[, domain_cols, with = FALSE]
  )

  batch_sizes <- instance_batch$archive$data[, .N, by = batch_nr]
  expect_equal(batch_sizes$N, c(3L, 2L, 2L, 2L))
})

test_that("OptimizerGS random batching evaluates multiple points per iteration", {
  setup <- make_gs_test_setup(n = 20, d = 2, seed = 321)

  set.seed(123)
  optimizer <- OptimizerGS$new(scoring = scoring_random())
  optimizer$param_set$set_values(n_init = 3L, batch_size = 2L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 7)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 7L)
  batch_sizes <- instance$archive$data[, .N, by = batch_nr]
  expect_equal(batch_sizes$N, c(3L, 2L, 2L))
})

test_that("OptimizerGS errors for unsupported batchable model-based methods", {
  setup <- make_gs_test_setup(n = 20, d = 2)
  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  expect_error({
    optimizer <- OptimizerGS$new(scoring = scoring_gsy(), learner = learner)
    optimizer$param_set$set_values(n_init = 3L, batch_size = 2L)
    instance <- SearchInstance$new(
      objective = setup$objective,
      terminator = trm("evals", n_evals = 8)
    )
    optimizer$optimize(instance)
  }, "not yet implemented")

  expect_error({
    optimizer <- OptimizerGS$new(scoring = scoring_igs(), learner = learner)
    optimizer$param_set$set_values(n_init = 3L, batch_size = 2L)
    instance <- SearchInstance$new(
      objective = setup$objective,
      terminator = trm("evals", n_evals = 8)
    )
    optimizer$optimize(instance)
  }, "not yet implemented")

  expect_error({
    optimizer <- OptimizerGS$new(scoring = scoring_qbc(k_qbc = 3L), learner = learner)
    optimizer$param_set$set_values(n_init = 3L, batch_size = 2L)
    instance <- SearchInstance$new(
      objective = setup$objective,
      terminator = trm("evals", n_evals = 8)
    )
    optimizer$optimize(instance)
  }, "not yet implemented")
})

test_that("OptimizerGS stops when pool is exhausted", {
  setup <- make_gs_test_setup(n = 5, d = 1)

  optimizer <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer$param_set$set_values(n_init = 2L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 100)  # more than pool size
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  # Should have evaluated all 5 pool points
  expect_equal(instance$archive$n_evals, 5L)
})

test_that("OptimizerGS deduplicates the working pool", {
  dt <- data.table(
    x = c(0, 0, 0.5, 1, 1),
    y = c(0, 0, 0.25, 1, 1)
  )

  domain <- ps(x = p_dbl(lower = 0, upper = 1))
  codomain <- ps(y = p_dbl(tags = "learn"))
  objective <- ObjectiveDataset$new(dataset = dt, domain = domain, codomain = codomain)

  optimizer <- OptimizerGS$new(scoring = scoring_gsx())
  optimizer$param_set$set_values(n_init = 2L, batch_size = 2L)

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 10)
  )

  expect_no_warning(
    tryCatch(
      optimizer$optimize(instance),
      search_terminated_error = function(e) NULL
    )
  )

  expect_equal(instance$archive$n_evals, 3L)
  expect_equal(uniqueN(instance$archive$data$x), 3L)
})

test_that("OptimizerGS QBC runs end-to-end", {
  setup <- make_gs_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  set.seed(42)
  optimizer <- OptimizerGS$new(
    scoring = scoring_qbc(k_qbc = 3L),
    learner = learner
  )
  optimizer$param_set$set_values(n_init = 3L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 6)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 6L)
})
