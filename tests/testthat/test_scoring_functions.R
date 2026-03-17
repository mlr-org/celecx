# =============================================================================
# Tests for scoring_functions.R
# =============================================================================

# Helper: create a simple pool and fitted learner for testing
make_scoring_test_data <- function() {
  # 10 points in 1D, linearly spaced
  pool_x <- matrix(seq(0, 9, by = 1), nrow = 10, ncol = 1,
    dimnames = list(NULL, "x"))
  pool_y <- pool_x[, 1]^2 / 10  # quadratic target

  queried_idx <- c(1L, 5L, 10L)
  unqueried_idx <- setdiff(seq_len(10), queried_idx)
  queried_y <- pool_y[queried_idx]

  # Train a simple learner on scaled features
  train_dt <- data.table(x = pool_x[queried_idx, 1], y = queried_y)
  task <- as_task_regr(train_dt, target = "y")
  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)
  learner$train(task)

  list(
    pool_x = pool_x,
    pool_y = pool_y,
    queried_idx = queried_idx,
    unqueried_idx = unqueried_idx,
    queried_y = queried_y,
    learner = learner,
    target_id = "y"
  )
}

test_that("scoring_random returns correct length", {
  d <- make_scoring_test_data()
  fn <- scoring_random()
  scores <- fn(d$pool_x, d$unqueried_idx, NULL, d$queried_idx, d$queried_y,
    d$target_id)
  expect_length(scores, length(d$unqueried_idx))
  expect_true(all(is.finite(scores)))
})

test_that("scoring_gsx returns correct length and positive scores", {
  d <- make_scoring_test_data()
  fn <- scoring_gsx()
  scores <- fn(d$pool_x, d$unqueried_idx, NULL, d$queried_idx, d$queried_y,
    d$target_id)
  expect_length(scores, length(d$unqueried_idx))
  expect_true(all(scores >= 0))
})

test_that("scoring_gsx selects farthest point from queried set", {
  # Queried: indices 1 (x=0) and 10 (x=9)
  pool_x <- matrix(seq(0, 9, by = 1), nrow = 10, ncol = 1,
    dimnames = list(NULL, "x"))
  queried_idx <- c(1L, 10L)
  unqueried_idx <- 2:9
  queried_y <- c(0, 81)

  fn <- scoring_gsx()
  scores <- fn(pool_x, unqueried_idx, NULL, queried_idx, queried_y, "y")

  # Point 5 (x=4) and 6 (x=5) should have highest scores
  # min dist for x=4: min(4, 5) = 4
  # min dist for x=5: min(5, 4) = 4
  # These should be equal and highest
  best_local <- which.max(scores)
  best_pool_idx <- unqueried_idx[best_local]
  # Should be index 5 or 6 (both have min dist = 4, tie-break: smallest)
  expect_true(best_pool_idx %in% c(5L, 6L))
})

test_that("scoring_gsy returns correct length", {
  d <- make_scoring_test_data()
  fn <- scoring_gsy()
  scores <- fn(d$pool_x, d$unqueried_idx, d$learner, d$queried_idx,
    d$queried_y, d$target_id)
  expect_length(scores, length(d$unqueried_idx))
  expect_true(all(scores >= 0))
})

test_that("scoring_igs returns correct length", {
  d <- make_scoring_test_data()
  fn <- scoring_igs()
  scores <- fn(d$pool_x, d$unqueried_idx, d$learner, d$queried_idx,
    d$queried_y, d$target_id)
  expect_length(scores, length(d$unqueried_idx))
  expect_true(all(scores >= 0))
})

test_that("scoring_igs uses product-inside-min, not product of minima", {
  # Construct a case where iGS differs from product-of-separate-minima
  # Queried: m1 at (0, y=10), m2 at (10, y=0)
  # Candidate C at (2, yhat=8):
  #   m1: dx=2, dy=|8-10|=2, prod=4
  #   m2: dx=8, dy=|8-0|=8, prod=64
  #   iGS = min(4, 64) = 4
  #   separate: min_dx=2 * min_dy=2 = 4 (same for this candidate)
  #
  # Candidate D at (4, yhat=3):
  #   m1: dx=4, dy=|3-10|=7, prod=28
  #   m2: dx=6, dy=|3-0|=3, prod=18
  #   iGS = min(28, 18) = 18
  #   separate: min_dx=4 * min_dy=3 = 12

  pool_x <- matrix(c(0, 10, 2, 4), nrow = 4, ncol = 1,
    dimnames = list(NULL, "x"))

  queried_idx <- c(1L, 2L)
  queried_y <- c(10, 0)
  unqueried_idx <- c(3L, 4L)

  # Create a mock learner that predicts specific values
  # Candidate at x=2 predicts 8, at x=4 predicts 3
  train_dt <- data.table(x = c(0, 10, 2, 4), y = c(10, 0, 8, 3))
  task <- as_task_regr(train_dt, target = "y")
  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L, cp = 0)
  learner$train(task)

  fn <- scoring_igs()
  scores <- fn(pool_x, unqueried_idx, learner, queried_idx, queried_y, "y")

  # Get model predictions for verification
  pred <- learner$predict_newdata(data.table(x = c(2, 4)))
  yhat <- pred$response

  # Compute iGS scores manually
  # Candidate 1 (x=2, yhat[1]):
  dx_c1 <- abs(c(0, 10) - 2)  # 2, 8
  dy_c1 <- abs(yhat[1] - c(10, 0))
  igs_c1 <- min(dx_c1 * dy_c1)

  # Candidate 2 (x=4, yhat[2]):
  dx_c2 <- abs(c(0, 10) - 4)  # 4, 6
  dy_c2 <- abs(yhat[2] - c(10, 0))
  igs_c2 <- min(dx_c2 * dy_c2)

  expect_equal(scores[1], igs_c1, tolerance = 1e-6)
  expect_equal(scores[2], igs_c2, tolerance = 1e-6)

  # Compute product-of-minima for comparison
  sep_c1 <- min(dx_c1) * min(dy_c1)
  sep_c2 <- min(dx_c2) * min(dy_c2)

  # The two formulations should give different values
  # (at least for one candidate)
  if (abs(yhat[1] - 8) < 0.1 && abs(yhat[2] - 3) < 0.1) {
    # Only check if learner predictions are close to expected
    expect_false(
      isTRUE(all.equal(c(igs_c1, igs_c2), c(sep_c1, sep_c2))),
      info = "iGS and product-of-minima should differ for this test case"
    )
  }
})

test_that("scoring_qbc returns correct length", {
  d <- make_scoring_test_data()
  set.seed(42)
  fn <- scoring_qbc(k_qbc = 3L)
  scores <- fn(d$pool_x, d$unqueried_idx, d$learner, d$queried_idx,
    d$queried_y, d$target_id)
  expect_length(scores, length(d$unqueried_idx))
  expect_true(all(scores >= 0))
})

test_that("scoring functions work with 2D features", {
  pool_x <- matrix(rnorm(20), nrow = 10, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))
  pool_y <- rowSums(pool_x^2)

  queried_idx <- 1:3
  unqueried_idx <- 4:10
  queried_y <- pool_y[queried_idx]

  train_dt <- as.data.table(pool_x[queried_idx, , drop = FALSE])
  train_dt$y <- queried_y
  task <- as_task_regr(train_dt, target = "y")
  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L)
  learner$train(task)

  for (fn_factory in list(scoring_gsx, scoring_gsy, scoring_igs)) {
    fn <- fn_factory()
    scores <- fn(pool_x, unqueried_idx, learner, queried_idx, queried_y, "y")
    expect_length(scores, 7L)
    expect_true(all(is.finite(scores)))
  }
})
