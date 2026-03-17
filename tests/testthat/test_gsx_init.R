# =============================================================================
# Tests for gsx_init.R
# =============================================================================

test_that("gsx_init selects centroid-nearest as first point", {
  # Pool: 4 points in 2D
  pool_x <- matrix(c(
    0, 0,   # idx 1
    10, 0,  # idx 2
    5, 0,   # idx 3 -- nearest to centroid (3.75, 0)
    0, 0    # idx 4 (duplicate of 1)
  ), nrow = 4, ncol = 2, byrow = TRUE,
    dimnames = list(NULL, c("x1", "x2")))

  result <- gsx_init(pool_x, n_init = 1L)
  # Centroid = (3.75, 0). Nearest is point 3 at (5, 0) with dist 1.25
  # (closer than point 1/4 at dist 3.75 or point 2 at dist 6.25)
  expect_equal(result, 3L)
})

test_that("gsx_init performs farthest-first after centroid", {
  # Pool: 5 points on a line
  pool_x <- matrix(c(0, 2, 5, 8, 10), nrow = 5, ncol = 1,
    dimnames = list(NULL, "x"))

  result <- gsx_init(pool_x, n_init = 3L)

  # Centroid = 5, nearest is point 3 (x=5)
  expect_equal(result[1], 3L)
  # After selecting x=5: farthest is x=0 or x=10 (both dist 5).
  # Tie-break: smallest index, so point 1 (x=0)
  expect_equal(result[2], 1L)
  # After selecting {5, 0}: min dists are 0->5=0, 2->2=2, 5->0=0, 8->3=3, 10->5=5
  # Wait, point 1 and 3 are selected, so: point 2 min(2,3)=2, point 4 min(8,3)=3, point 5 min(10,5)=5
  # Farthest is point 5 (x=10)
  expect_equal(result[3], 5L)
})

test_that("gsx_init with n_init = N selects all points", {
  pool_x <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1,
    dimnames = list(NULL, "x"))

  result <- gsx_init(pool_x, n_init = 4L)
  expect_length(result, 4L)
  expect_setequal(result, 1:4)
})

test_that("gsx_init with n_init > N is clamped", {
  pool_x <- matrix(c(1, 2), nrow = 2, ncol = 1, dimnames = list(NULL, "x"))

  result <- gsx_init(pool_x, n_init = 10L)
  expect_length(result, 2L)
  expect_setequal(result, 1:2)
})

test_that("gsx_init tie-breaks by smallest index", {
  # Symmetric pool: all equidistant from centroid
  pool_x <- matrix(c(
    -1, 0,  # idx 1
    1, 0,   # idx 2
    0, -1,  # idx 3
    0, 1    # idx 4
  ), nrow = 4, ncol = 2, byrow = TRUE,
    dimnames = list(NULL, c("x1", "x2")))

  result <- gsx_init(pool_x, n_init = 1L)
  # Centroid = (0, 0). All points equidistant (dist=1).
  # Tie-break: smallest index
  expect_equal(result, 1L)
})

test_that("gsx_init works in 2D with known geometry", {
  # Square corners
  pool_x <- matrix(c(
    0, 0,   # idx 1
    0, 10,  # idx 2
    10, 0,  # idx 3
    10, 10  # idx 4
  ), nrow = 4, ncol = 2, byrow = TRUE,
    dimnames = list(NULL, c("x1", "x2")))

  result <- gsx_init(pool_x, n_init = 4L)
  # Centroid = (5, 5). All equidistant (sqrt(50)). First = 1 (tie-break).
  expect_equal(result[1], 1L)
  # After (0,0): farthest is (10,10) at dist sqrt(200). Others at sqrt(100).
  expect_equal(result[2], 4L)
  # After {(0,0), (10,10)}: point 2 min(10, 10)=10, point 3 min(10, 10)=10
  # Tie-break: smallest index
  expect_equal(result[3], 2L)
  expect_equal(result[4], 3L)
})

test_that("kmeans_init returns correct number of points", {
  set.seed(42)
  pool_x <- matrix(rnorm(100), nrow = 50, ncol = 2,
    dimnames = list(NULL, c("x1", "x2")))

  result <- kmeans_init(pool_x, n_init = 5L)
  expect_length(result, 5L)
  expect_true(all(result >= 1 & result <= 50))
  # All unique
  expect_equal(length(unique(result)), 5L)
})

test_that("kmeans_init with n_init = 1 returns centroid-nearest", {
  pool_x <- matrix(c(0, 5, 10), nrow = 3, ncol = 1,
    dimnames = list(NULL, "x"))

  result <- kmeans_init(pool_x, n_init = 1L)
  # Centroid = 5, nearest is point 2
  expect_equal(result, 2L)
})

test_that("kmeans_init with n_init >= N returns all indices", {
  pool_x <- matrix(c(1, 2, 3), nrow = 3, ncol = 1,
    dimnames = list(NULL, "x"))

  result <- kmeans_init(pool_x, n_init = 5L)
  expect_setequal(result, 1:3)
})
