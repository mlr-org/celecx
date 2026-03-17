# =============================================================================
# Tests for idw_geometry.R
# =============================================================================

test_that("idw_geometry exact match gives v_k=1, z=0", {
  attempted <- matrix(c(0, 0, 1, 1), nrow = 2, ncol = 2, byrow = TRUE)

  # Query at exact location of first attempted point

  result <- idw_geometry(c(0, 0), attempted)
  expect_equal(result$v, c(1, 0))
  expect_equal(result$z, 0)

  # Query at exact location of second attempted point
  result2 <- idw_geometry(c(1, 1), attempted)
  expect_equal(result2$v, c(0, 1))
  expect_equal(result2$z, 0)
})

test_that("idw_geometry weights sum to 1", {
  attempted <- matrix(c(0, 1, 2, 3), nrow = 4, ncol = 1)
  result <- idw_geometry(c(1.5), attempted)
  expect_equal(sum(result$v), 1, tolerance = 1e-12)
})

test_that("idw_geometry z is large for distant points", {
  attempted <- matrix(c(0, 0), nrow = 1, ncol = 2)

  # Near point: small z
  near <- idw_geometry(c(0.1, 0.1), attempted)
  # Far point: large z
  far <- idw_geometry(c(10, 10), attempted)

  expect_gt(far$z, near$z)
})

test_that("idw_geometry z is bounded in [0, 1]", {
  attempted <- matrix(c(0, 1, 2), nrow = 3, ncol = 1)

  # Various query points
  for (x in c(-10, 0.5, 1.5, 100)) {
    result <- idw_geometry(c(x), attempted)
    expect_gte(result$z, 0)
    expect_lte(result$z, 1)
  }
})

test_that("idw_geometry uses exponential damping", {
  attempted <- matrix(c(0, 10), nrow = 2, ncol = 1)

  # Query at x=1 (close to 0, far from 10)
  result <- idw_geometry(c(1), attempted)
  # Weight for point 0 (d2=1) should be much larger than point 10 (d2=81)
  # w_0 = exp(-1)/1 = 0.368, w_10 = exp(-81)/81 ≈ 0
  expect_gt(result$v[1], 0.99)
})

test_that("idw_geometry handles single attempted point", {
  attempted <- matrix(c(5), nrow = 1, ncol = 1)

  result <- idw_geometry(c(3), attempted)
  expect_equal(result$v, 1)  # only one weight, must be 1
  # z = (2/pi) * atan(1/W) where W = exp(-4)/4
  expect_gt(result$z, 0)
})

test_that("idw_acquisition computes correct values", {
  # Simple 1D case: one attempted point at 0 with y=0, model predicts 1 at x=2
  attempted_x <- matrix(c(0), nrow = 1, ncol = 1)
  attempted_y <- c(0)
  Q <- 1L

  candidates <- matrix(c(2), nrow = 1, ncol = 1)
  candidates_yhat <- c(1)

  result <- idw_acquisition(candidates, candidates_yhat, attempted_x,
    attempted_y, Q, delta = 1)

  # v = 1 (only one attempted point), z = 0 if exact match, > 0 otherwise
  # s2 = v[Q] * (y[Q] - yhat)^2 = 1 * (0 - 1)^2 = 1
  geom <- idw_geometry(c(2), attempted_x)
  expected_s2 <- geom$v[1] * (0 - 1)^2
  expected <- expected_s2 + 1 * geom$z
  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("idw_acquisition with delta=0 ignores exploration", {
  attempted_x <- matrix(c(0, 5), nrow = 2, ncol = 1)
  attempted_y <- c(0, 10)
  Q <- 1:2

  candidates <- matrix(c(2, 8), nrow = 2, ncol = 1)
  candidates_yhat <- c(4, 16)

  result_d0 <- idw_acquisition(candidates, candidates_yhat, attempted_x,
    attempted_y, Q, delta = 0)
  result_d1 <- idw_acquisition(candidates, candidates_yhat, attempted_x,
    attempted_y, Q, delta = 1)

  # With delta=0, no exploration term
  # With delta>0, scores should be higher (z >= 0)
  expect_true(all(result_d1 >= result_d0 - 1e-12))
})

test_that("idw_acquisition uses only Q for uncertainty", {
  # Two attempted points, but only first is successful
  attempted_x <- matrix(c(0, 5), nrow = 2, ncol = 1)
  attempted_y <- c(0, NA)  # second failed
  Q <- 1L  # only first is successful

  candidates <- matrix(c(2), nrow = 1, ncol = 1)
  candidates_yhat <- c(1)

  # Should not error despite NA in attempted_y
  result <- idw_acquisition(candidates, candidates_yhat, attempted_x,
    attempted_y, Q, delta = 1)
  expect_length(result, 1L)
  expect_true(is.finite(result))
})

test_that("idw_acquisition at attempted point gives z=0 contribution", {
  attempted_x <- matrix(c(0, 5), nrow = 2, ncol = 1)
  attempted_y <- c(0, 10)
  Q <- 1:2

  # Query at exact attempted location
  candidates <- matrix(c(0), nrow = 1, ncol = 1)
  candidates_yhat <- c(0)

  result <- idw_acquisition(candidates, candidates_yhat, attempted_x,
    attempted_y, Q, delta = 100)

  # z=0 at exact match, and v_1=1 so s2 = (0-0)^2 = 0
  expect_equal(result, 0, tolerance = 1e-12)
})

test_that("idw_acquisition respects target_scale normalization", {
  attempted_x <- matrix(c(0, 5), nrow = 2, ncol = 1)
  attempted_y <- c(0, 10)
  Q <- 1:2

  candidates <- matrix(c(2), nrow = 1, ncol = 1)
  candidates_yhat <- c(4)

  result_unit <- idw_acquisition(
    candidates_scaled = candidates,
    candidates_yhat = candidates_yhat,
    attempted_x_scaled = attempted_x,
    attempted_y = attempted_y,
    Q = Q,
    delta = 0,
    target_scale = 1
  )
  result_scaled <- idw_acquisition(
    candidates_scaled = candidates,
    candidates_yhat = candidates_yhat,
    attempted_x_scaled = attempted_x,
    attempted_y = attempted_y,
    Q = Q,
    delta = 0,
    target_scale = 10
  )

  expect_equal(result_scaled, result_unit / 100, tolerance = 1e-12)
})
