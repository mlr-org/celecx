# =============================================================================
# Tests for optimizer_pool_al.R and optimize_active integration
# =============================================================================

# Helper
make_pool_al_test_setup <- function(n = 20, d = 2, seed = 42) {
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

test_that("optimizer_pool_al creates GSx optimizer", {
  opt <- optimizer_pool_al("gsx")
  expect_r6(opt, "OptimizerGS")
  expect_null(opt$learner)
})

test_that("optimizer_pool_al creates GSy optimizer with learner", {
  learner <- lrn("regr.rpart")
  opt <- optimizer_pool_al("gsy", learner = learner)
  expect_r6(opt, "OptimizerGS")
  expect_r6(opt$learner, "LearnerRegr")
})

test_that("optimizer_pool_al creates iGS optimizer", {
  learner <- lrn("regr.rpart")
  opt <- optimizer_pool_al("igs", learner = learner)
  expect_r6(opt, "OptimizerGS")
})

test_that("optimizer_pool_al creates QBC optimizer", {
  learner <- lrn("regr.rpart")
  opt <- optimizer_pool_al("qbc", learner = learner, k_qbc = 3L)
  expect_r6(opt, "OptimizerGS")
  expect_equal(opt$param_set$get_values()$init_method, "random")
})

test_that("optimizer_pool_al creates random optimizer", {
  opt <- optimizer_pool_al("random")
  expect_r6(opt, "OptimizerGS")
  expect_null(opt$learner)
  expect_equal(opt$param_set$get_values()$init_method, "random")
})

test_that("optimizer_pool_al creates IDEAL optimizer", {
  learner <- lrn("regr.rpart")
  opt <- optimizer_pool_al("ideal", learner = learner, delta = 2, n_init = 5L)
  expect_r6(opt, "OptimizerIDEAL")
  expect_equal(opt$param_set$get_values()$delta, 2)
  expect_equal(opt$param_set$get_values()$n_init, 5L)
})

test_that("optimizer_pool_al errors without learner for model-based methods", {
  expect_error(optimizer_pool_al("gsy"))
  expect_error(optimizer_pool_al("igs"))
  expect_error(optimizer_pool_al("qbc"))
  expect_error(optimizer_pool_al("ideal"))
})

test_that("optimizer_pool_al respects n_init and batch_size", {
  opt <- optimizer_pool_al("gsx", n_init = 7L, batch_size = 1L)
  pv <- opt$param_set$get_values()
  expect_equal(pv$n_init, 7L)
  expect_equal(pv$batch_size, 1L)
  expect_equal(pv$init_method, "gsx")
})

test_that("optimizer_pool_al allows init_method overrides for GS family methods", {
  opt <- optimizer_pool_al("random", init_method = "gsx")
  expect_equal(opt$param_set$get_values()$init_method, "gsx")
})

test_that("optimize_active works with explicit GS optimizer", {
  setup <- make_pool_al_test_setup(n = 20, d = 2)

  optimizer <- optimizer_pool_al("gsx", n_init = 3L)

  result <- tryCatch(
    optimize_active(
      objective = setup$objective,
      term_evals = 6L,
      optimizer = optimizer
    ),
    search_terminated_error = function(e) NULL
  )

  expect_list(result)
  expect_r6(result$instance, "SearchInstance")
  expect_equal(result$instance$archive$n_evals, 6L)
})

test_that("optimize_active works with explicit IDEAL optimizer", {
  setup <- make_pool_al_test_setup(n = 20, d = 2)

  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)
  optimizer <- optimizer_pool_al("ideal", learner = learner, n_init = 3L)

  result <- tryCatch(
    optimize_active(
      objective = setup$objective,
      term_evals = 6L,
      optimizer = optimizer
    ),
    search_terminated_error = function(e) NULL
  )

  expect_list(result)
  expect_r6(result$instance, "SearchInstance")
  expect_equal(result$instance$archive$n_evals, 6L)
})

test_that("optimize_active with metrics_tracker works for GS", {
  setup <- make_pool_al_test_setup(n = 20, d = 2)

  tracker <- MetricsTracker$new(
    metrics = list(best_y = metric_best_y)
  )

  optimizer <- optimizer_pool_al("gsx", n_init = 3L)

  result <- tryCatch(
    optimize_active(
      objective = setup$objective,
      term_evals = 6L,
      optimizer = optimizer,
      metrics_tracker = tracker
    ),
    search_terminated_error = function(e) NULL
  )

  # Tracker should have logged batches
  expect_gt(tracker$n_batches, 0L)
})
