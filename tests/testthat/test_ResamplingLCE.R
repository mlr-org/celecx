test_that("ResamplingLCEHoldout basic split", {
  dt <- data.table(n_evals = 1:20, metric = rnorm(20))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  r <- rsmp("lce_holdout", ratio = 0.7)
  r$instantiate(task)

  expect_equal(r$iters, 1L)
  train <- r$train_set(1)
  test <- r$test_set(1)

  expect_equal(length(train), 14L)
  expect_equal(length(test), 6L)
  expect_length(intersect(train, test), 0L)
  expect_setequal(c(train, test), task$row_ids)
})

test_that("ResamplingLCEHoldout respects temporal order", {
  dt <- data.table(n_evals = c(10, 20, 30, 40, 50), metric = 1:5)
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  r <- rsmp("lce_holdout", ratio = 0.6)
  r$instantiate(task)

  train <- r$train_set(1)
  test <- r$test_set(1)

  # Extract n_evals for train and test
  train_n <- task$data(rows = train, cols = "n_evals")$n_evals
  test_n <- task$data(rows = test, cols = "n_evals")$n_evals

  # All train n_evals should be < all test n_evals
  expect_true(max(train_n) < min(test_n))
})

test_that("ResamplingLCEHoldout with key column", {
  dt <- data.table(
    n_evals = rep(1:10, 2),
    run = factor(rep(c("a", "b"), each = 10)),
    metric = rnorm(20)
  )
  task <- as_task_lce(dt, target = "metric", order = "n_evals",
    key = "run", id = "t")

  r <- rsmp("lce_holdout", ratio = 0.5)
  r$instantiate(task)

  train <- r$train_set(1)
  test <- r$test_set(1)

  # Should have 5 train + 5 test from each curve = 10 + 10
  expect_equal(length(train), 10L)
  expect_equal(length(test), 10L)
})

test_that("ResamplingLCECV expanding window", {
  dt <- data.table(n_evals = 1:20, metric = rnorm(20))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  cv <- rsmp("lce_cv", folds = 3, horizon = 2, window_size = 5, step_size = 2)
  cv$instantiate(task)

  expect_equal(cv$iters, 3L)

  for (i in 1:3) {
    train <- cv$train_set(i)
    test <- cv$test_set(i)

    expect_length(test, 2L)
    expect_length(intersect(train, test), 0L)

    # Train should always start from beginning (expanding)
    expect_true(min(train) == 1L)

    # Temporal: train before test
    train_n <- task$data(rows = train, cols = "n_evals")$n_evals
    test_n <- task$data(rows = test, cols = "n_evals")$n_evals
    expect_true(max(train_n) < min(test_n))
  }

  # Expanding: later folds should have more training data
  expect_true(length(cv$train_set(3)) > length(cv$train_set(1)))
})

test_that("ResamplingLCECV fixed window", {
  dt <- data.table(n_evals = 1:20, metric = rnorm(20))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  cv <- rsmp("lce_cv", folds = 3, horizon = 2, window_size = 5,
    step_size = 2, fixed_window = TRUE)
  cv$instantiate(task)

  # All folds should have same training size (fixed window)
  for (i in 1:3) {
    expect_equal(length(cv$train_set(i)), 5L)
    expect_equal(length(cv$test_set(i)), 2L)
  }
})

test_that("ResamplingLCECV validates parameters", {
  dt <- data.table(n_evals = 1:5, metric = rnorm(5))
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  # window_size + horizon > n
  cv <- rsmp("lce_cv", folds = 1, horizon = 3, window_size = 4)
  expect_error(cv$instantiate(task), "exceeds observations")

  # Too many folds
  cv2 <- rsmp("lce_cv", folds = 10, horizon = 1, window_size = 2)
  expect_error(cv2$instantiate(task), "exceeds maximum feasible folds")
})

test_that("ResamplingLCECV with key column", {
  dt <- data.table(
    n_evals = rep(1:15, 2),
    run = factor(rep(c("a", "b"), each = 15)),
    metric = rnorm(30)
  )
  task <- as_task_lce(dt, target = "metric", order = "n_evals",
    key = "run", id = "t")

  cv <- rsmp("lce_cv", folds = 3, horizon = 1, window_size = 5)
  cv$instantiate(task)

  for (i in 1:3) {
    train <- cv$train_set(i)
    test <- cv$test_set(i)

    # Each fold should have data from both runs
    train_runs <- task$data(rows = train, cols = "run")$run
    test_runs <- task$data(rows = test, cols = "run")$run

    expect_true("a" %in% train_runs)
    expect_true("b" %in% train_runs)
    # Test should have 1 observation per curve = 2 total
    expect_equal(length(test), 2L)
  }
})
