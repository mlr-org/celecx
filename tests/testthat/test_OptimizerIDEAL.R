# =============================================================================
# Tests for OptimizerIDEAL.R
# =============================================================================

# Helper: create a test pool dataset + objective
make_ideal_test_setup <- function(n = 20, d = 2, seed = 42) {
  set.seed(seed)
  x <- matrix(runif(n * d), nrow = n, ncol = d)
  colnames(x) <- paste0("x", seq_len(d))
  y <- rowSums(sin(x * pi))
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

make_ideal_function_objective <- function() {
  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 1)
  )
  codomain <- ps(y = p_dbl(tags = "minimize"))

  ObjectiveRFun$new(
    fun = function(xs) {
      list(y = (xs$x1 - 0.3)^2 + (xs$x2 - 0.7)^2)
    },
    domain = domain,
    codomain = codomain
  )
}

make_grid_continuous_optimizer <- function(resolution = 11L) {
  force(resolution)

  function(search_space, score_function) {
    grid <- generate_design_grid(search_space, resolution = resolution)$data
    scores <- vapply(seq_len(nrow(grid)), function(i) {
      score_function(unlist(grid[i, ], use.names = FALSE))
    }, numeric(1L))
    grid[which.max(scores)]
  }
}

test_that("OptimizerIDEAL runs end-to-end", {
  setup <- make_ideal_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 1)

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

test_that("OptimizerIDEAL persists state in data_extra", {
  setup <- make_ideal_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 1)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 6)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  # Check data_extra
  extra <- instance$archive$data_extra[["OptimizerIDEAL"]]
  expect_list(extra)
  expect_equal(extra$mode, "pool")
  expect_true("attempted_idx" %in% names(extra))
  expect_true("attempted_x_raw" %in% names(extra))
  expect_true("attempted_y" %in% names(extra))
  expect_true("Q" %in% names(extra))
  expect_true("scaler" %in% names(extra))
  expect_true("target_scale" %in% names(extra))

  # attempted_idx should match n_evals
  expect_length(extra$attempted_idx, 6L)
  expect_equal(nrow(extra$attempted_x_raw), 6L)
  # All successful (deterministic)
  expect_length(extra$Q, 6L)
  expect_equal(
    as.data.table(extra$attempted_x_raw),
    instance$archive$data[, c("x1", "x2"), with = FALSE]
  )
})

test_that("OptimizerIDEAL n_init defaults to dimension d", {
  setup <- make_ideal_test_setup(n = 20, d = 3)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  # n_init not set -> should default to d=3

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 6)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 6L)
  # First batch has d=3 points
  batch_sizes <- instance$archive$data[, .N, by = batch_nr]
  expect_equal(batch_sizes$N[1], 3L)
})

test_that("OptimizerIDEAL init_method gsx works", {
  setup <- make_ideal_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 1, init_method = "gsx")

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 8)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 8L)
  # First batch should match gsx_init order
  pool_x <- as.matrix(setup$dt[, .(x1, x2)])
  scaler <- affine_scale_features(pool_x)
  expected_init <- gsx_init(scaler$scaled, 3L)
  init_xdt <- instance$archive$data[1:3, .(x1, x2)]
  expected_xdt <- setup$dt[expected_init, .(x1, x2)]
  expect_equal(init_xdt, expected_xdt)
})

test_that("OptimizerIDEAL init_method random works", {
  setup <- make_ideal_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 1, init_method = "random")

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
})

test_that("OptimizerIDEAL with delta=0 is pure exploitation", {
  setup <- make_ideal_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 0)

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

test_that("OptimizerIDEAL with large delta is more explorative", {
  setup <- make_ideal_test_setup(n = 30, d = 2, seed = 123)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  # Run with small delta
  set.seed(1)
  opt_small <- OptimizerIDEAL$new(learner = learner)
  opt_small$param_set$set_values(n_init = 3L, delta = 0.01)
  inst_small <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 10)
  )
  tryCatch(
    opt_small$optimize(inst_small),
    search_terminated_error = function(e) NULL
  )

  # Run with large delta
  set.seed(1)
  opt_large <- OptimizerIDEAL$new(learner = learner)
  opt_large$param_set$set_values(n_init = 3L, delta = 100)
  inst_large <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 10)
  )
  tryCatch(
    opt_large$optimize(inst_large),
    search_terminated_error = function(e) NULL
  )

  # Both should complete successfully
  expect_equal(inst_small$archive$n_evals, 10L)
  expect_equal(inst_large$archive$n_evals, 10L)

  # They should select different points (at least some)
  x_small <- inst_small$archive$data[, c("x1", "x2"), with = FALSE]
  x_large <- inst_large$archive$data[, c("x1", "x2"), with = FALSE]
  expect_false(isTRUE(all.equal(x_small, x_large)))
})

test_that("OptimizerIDEAL batching evaluates multiple points per iteration", {
  setup <- make_ideal_test_setup(n = 20, d = 2, seed = 123)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 1, batch_size = 2L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 9)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 9L)
  batch_sizes <- instance$archive$data[, .N, by = batch_nr]
  expect_equal(batch_sizes$N, c(3L, 2L, 2L, 2L))
})

test_that("OptimizerIDEAL stops when pool is exhausted", {
  setup <- make_ideal_test_setup(n = 5, d = 1)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 2L)

  instance <- SearchInstance$new(
    objective = setup$objective,
    terminator = trm("evals", n_evals = 100)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 5L)
})

test_that("OptimizerIDEAL deduplicates the working pool", {
  dt <- data.table(
    x = c(0, 0, 1, 1),
    y = c(0, 0, 1, 1)
  )

  domain <- ps(x = p_dbl(lower = 0, upper = 1))
  codomain <- ps(y = p_dbl(tags = "learn"))
  objective <- ObjectiveDataset$new(dataset = dt, domain = domain, codomain = codomain)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 3L, delta = 1, batch_size = 2L)

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

  expect_equal(instance$archive$n_evals, 2L)
  expect_equal(uniqueN(instance$archive$data$x), 2L)
})

test_that("OptimizerIDEAL with 1D pool works", {
  setup <- make_ideal_test_setup(n = 15, d = 1)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(learner = learner)
  optimizer$param_set$set_values(n_init = 2L, delta = 1)

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

test_that("OptimizerIDEAL supports population mode with injected optimizer", {
  objective <- make_ideal_function_objective()
  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)

  optimizer <- OptimizerIDEAL$new(
    learner = learner,
    continuous_optimizer = make_grid_continuous_optimizer(resolution = 9L)
  )
  optimizer$param_set$set_values(n_init = 3L, delta = 1)

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 6)
  )

  tryCatch(
    optimizer$optimize(instance),
    search_terminated_error = function(e) NULL
  )

  expect_equal(instance$archive$n_evals, 6L)
  extra <- instance$archive$data_extra[["OptimizerIDEAL"]]
  expect_equal(extra$mode, "population")
  expect_equal(nrow(extra$attempted_x_raw), 6L)
  expect_length(extra$Q, 6L)
})

test_that("OptimizerIDEAL population mode rejects non-numeric search spaces", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = 1),
    domain = ps(x = p_fct(levels = c("a", "b"))),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  optimizer <- OptimizerIDEAL$new(
    learner = lrn("regr.featureless"),
    continuous_optimizer = make_grid_continuous_optimizer()
  )
  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 3)
  )

  expect_error(
    optimizer$optimize(instance),
    "does not support param types|bounded numeric search space"
  )
})

test_that("OptimizerIDEAL population mode rejects unbounded search spaces", {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = ps(x = p_dbl(lower = -Inf, upper = 1)),
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )

  optimizer <- OptimizerIDEAL$new(
    learner = lrn("regr.featureless"),
    continuous_optimizer = make_grid_continuous_optimizer()
  )
  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 3),
    check_values = FALSE
  )

  expect_error(
    optimizer$optimize(instance),
    "finite lower and upper bounds"
  )
})
