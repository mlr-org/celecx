# =============================================================================
# Tests for feature_scaling.R
# =============================================================================

test_that("standardize_features centers and scales correctly", {
  pool_x <- matrix(c(1, 2, 3, 10, 20, 30), nrow = 3, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  result <- standardize_features(pool_x)

  # Check center
  expect_equal(result$center, c(x1 = 2, x2 = 20))
  # Check scale (sample sd)
  expect_equal(result$scale, c(x1 = sd(1:3), x2 = sd(c(10, 20, 30))))

  # Scaled values should have mean 0, sd 1
  expect_equal(colMeans(result$scaled), c(x1 = 0, x2 = 0), tolerance = 1e-12)
  expect_equal(apply(result$scaled, 2, sd), c(x1 = 1, x2 = 1), tolerance = 1e-12)

  # Column names preserved
  expect_equal(colnames(result$scaled), c("x1", "x2"))
})

test_that("standardize_features handles zero-variance columns", {
  pool_x <- matrix(c(1, 2, 3, 5, 5, 5), nrow = 3, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  result <- standardize_features(pool_x)

  # Zero-variance column should be all 0
  expect_true(all(result$scaled[, "x2"] == 0))
  expect_true(result$zero_var[2])
  expect_false(result$zero_var[1])

  # Non-zero column should still be standardized
  expect_equal(mean(result$scaled[, "x1"]), 0, tolerance = 1e-12)
})

test_that("standardize_apply reproduces standardize_features", {
  pool_x <- matrix(c(1, 2, 3, 4, 10, 20, 30, 40), nrow = 4, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  scaler <- standardize_features(pool_x)

  # Apply to same data should give same result
  applied <- standardize_apply(pool_x, scaler)
  expect_equal(applied, scaler$scaled, tolerance = 1e-12)

  # Apply to new data
  new_x <- matrix(c(2.5, 25), nrow = 1, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))
  new_scaled <- standardize_apply(new_x, scaler)
  expect_equal(ncol(new_scaled), 2L)
  expect_equal(nrow(new_scaled), 1L)
})

test_that("affine_scale_features maps to [-1, 1]", {
  pool_x <- matrix(c(0, 5, 10, 0, 100, 50), nrow = 3, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  result <- affine_scale_features(pool_x)

  # x1: range [0, 10], mid=5 -> scaled = 2*(x-5)/10
  # 0 -> -1, 5 -> 0, 10 -> 1
  expect_equal(result$scaled[, "x1"], c(-1, 0, 1))

  # x2: range [0, 100], mid=50 -> scaled = 2*(x-50)/100
  # 0 -> -1, 100 -> 1, 50 -> 0
  expect_equal(result$scaled[, "x2"], c(-1, 1, 0))

  # All values should be in [-1, 1]
  expect_true(all(result$scaled >= -1 & result$scaled <= 1))

  # Metadata (as.numeric strips names)
  expect_equal(result$mins, c(0, 0))
  expect_equal(result$maxs, c(10, 100))
})

test_that("affine_scale_features handles zero-range columns", {
  pool_x <- matrix(c(1, 2, 3, 7, 7, 7), nrow = 3, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  result <- affine_scale_features(pool_x)

  # Zero-range column should be all 0
  expect_true(all(result$scaled[, "x2"] == 0))
  # zero_range is computed on-the-fly; verify via mins/maxs
  expect_true(result$mins[2] == result$maxs[2])
  expect_false(result$mins[1] == result$maxs[1])
})

test_that("affine_scale_apply reproduces affine_scale_features", {
  pool_x <- matrix(c(0, 5, 10, 0, 100, 50), nrow = 3, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  scaler <- affine_scale_features(pool_x)

  # Apply to same data
  applied <- affine_scale_apply(pool_x, scaler)
  expect_equal(applied, scaler$scaled, tolerance = 1e-12)

  # Apply to new data: midpoint should map to 0
  mid <- matrix(c(5, 50), nrow = 1, dimnames = list(NULL, c("x1", "x2")))
  expect_equal(affine_scale_apply(mid, scaler), matrix(c(0, 0), nrow = 1,
    dimnames = list(NULL, c("x1", "x2"))))
})

test_that("feature scaling works with single-column matrices", {
  pool_x <- matrix(c(1, 2, 3), nrow = 3, ncol = 1,
    dimnames = list(NULL, "x"))

  # Standardize
  s <- standardize_features(pool_x)
  expect_equal(ncol(s$scaled), 1L)
  expect_equal(mean(s$scaled), 0, tolerance = 1e-12)

  # Affine
  a <- affine_scale_features(pool_x)
  expect_equal(ncol(a$scaled), 1L)
  expect_equal(range(a$scaled), c(-1, 1))
})
