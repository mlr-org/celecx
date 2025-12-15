# =============================================================================
# Tests for batch_strategies.R
# =============================================================================

# Helper function to create test search space
create_numeric_search_space <- function() {
  ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = -5, upper = 5)
  )
}

create_mixed_search_space <- function() {
  ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_int(lower = 1L, upper = 10L),
    x3 = p_fct(levels = c("a", "b", "c"))
  )
}

# Helper to create test candidates and scores
create_test_candidates <- function(n = 20) {
  data.table(
    x1 = runif(n),
    x2 = runif(n, -5, 5)
  )
}


# =============================================================================
# batch_strategy_greedy Tests
# =============================================================================

test_that("batch_strategy_greedy returns a function", {
  strategy <- batch_strategy_greedy()
  expect_function(strategy)
})

test_that("batch_strategy_greedy returns correct number of indices", {
  strategy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(20)
  scores <- runif(20)

  indices <- strategy(candidates, scores, batch_size = 5, NULL, NULL, search_space)

  expect_integerish(indices)
  expect_length(indices, 5)
})

test_that("batch_strategy_greedy selects lowest scores", {
  strategy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(10)
  # Explicit scores where indices 3, 7, 1 are lowest
  scores <- c(0.3, 0.9, 0.1, 0.8, 0.5, 0.6, 0.2, 0.7, 0.4, 1.0)

  indices <- strategy(candidates, scores, batch_size = 3, NULL, NULL, search_space)

  # Should select indices with lowest scores: 3 (0.1), 7 (0.2), 1 (0.3)
  expect_setequal(indices, c(3, 7, 1))
})

test_that("batch_strategy_greedy handles batch_size = 1", {
  strategy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(10)
  scores <- c(0.5, 0.3, 0.9, 0.1, 0.7, 0.2, 0.8, 0.4, 0.6, 1.0)

  indices <- strategy(candidates, scores, batch_size = 1, NULL, NULL, search_space)

  expect_length(indices, 1)
  expect_equal(indices, 4)  # index with score 0.1
})

test_that("batch_strategy_greedy handles batch_size = n_candidates", {
  strategy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(5)
  scores <- runif(5)

  indices <- strategy(candidates, scores, batch_size = 5, NULL, NULL, search_space)

  expect_length(indices, 5)
  expect_setequal(indices, 1:5)
})

test_that("batch_strategy_greedy returns indices in score order", {
  strategy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(10)
  scores <- c(0.5, 0.3, 0.9, 0.1, 0.7, 0.2, 0.8, 0.4, 0.6, 1.0)

  indices <- strategy(candidates, scores, batch_size = 4, NULL, NULL, search_space)

  # Should be in order: 4 (0.1), 6 (0.2), 2 (0.3), 8 (0.4)
  expect_equal(indices, c(4, 6, 2, 8))
})


# =============================================================================
# batch_strategy_local_penalization Tests
# =============================================================================

test_that("batch_strategy_local_penalization returns a function", {
  strategy <- batch_strategy_local_penalization()
  expect_function(strategy)
})

test_that("batch_strategy_local_penalization validates parameters", {
  expect_error(batch_strategy_local_penalization(bandwidth = -0.1))
  expect_error(batch_strategy_local_penalization(bandwidth = 1.5))
  expect_error(batch_strategy_local_penalization(penalization = -1))
})

test_that("batch_strategy_local_penalization returns correct number of indices", {
  strategy <- batch_strategy_local_penalization()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(20)
  scores <- runif(20)

  indices <- strategy(candidates, scores, batch_size = 5, NULL, NULL, search_space)

  expect_integerish(indices)
  expect_length(indices, 5)
})

test_that("batch_strategy_local_penalization selects first point by score", {
  strategy <- batch_strategy_local_penalization()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(10)
  scores <- c(0.5, 0.3, 0.9, 0.1, 0.7, 0.2, 0.8, 0.4, 0.6, 1.0)

  indices <- strategy(candidates, scores, batch_size = 1, NULL, NULL, search_space)

  # First point should be the one with lowest score
  expect_equal(indices, 4)
})

test_that("batch_strategy_local_penalization avoids nearby points", {
  strategy <- batch_strategy_local_penalization(bandwidth = 0.3, penalization = Inf)
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))

  # Clustered candidates - two groups far apart
  candidates <- data.table(x = c(0.0, 0.05, 0.1, 0.9, 0.95, 1.0))
  # Scores favor cluster at x ~ 0 but local pen should diversify
  scores <- c(0.1, 0.15, 0.2, 0.5, 0.55, 0.6)

  indices <- strategy(candidates, scores, batch_size = 2, NULL, NULL, search_space)

  # Should select from both clusters due to penalization
  # First pick: index 1 (best score at x=0)
  # After penalization: indices 2,3 are penalized, so pick from cluster at x~0.9
  expect_true(1 %in% indices)
  expect_true(any(indices %in% 4:6))
})

test_that("batch_strategy_local_penalization with bandwidth=0 equals greedy", {
  strategy_pen <- batch_strategy_local_penalization(bandwidth = 0)
  strategy_greedy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()

  set.seed(123)
  candidates <- create_test_candidates(10)
  scores <- runif(10)

  indices_pen <- strategy_pen(candidates, scores, batch_size = 3, NULL, NULL, search_space)
  indices_greedy <- strategy_greedy(candidates, scores, batch_size = 3, NULL, NULL, search_space)

  expect_equal(indices_pen, indices_greedy)
})


# =============================================================================
# batch_strategy_diversity Tests
# =============================================================================

test_that("batch_strategy_diversity returns a function", {
  strategy <- batch_strategy_diversity()
  expect_function(strategy)
})

test_that("batch_strategy_diversity validates parameters", {
  expect_error(batch_strategy_diversity(diversity_weight = -0.1))
  expect_error(batch_strategy_diversity(diversity_weight = 1.5))
})

test_that("batch_strategy_diversity returns correct number of indices", {
  strategy <- batch_strategy_diversity()
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(20)
  scores <- runif(20)

  indices <- strategy(candidates, scores, batch_size = 5, NULL, NULL, search_space)

  expect_integerish(indices)
  expect_length(indices, 5)
})

test_that("batch_strategy_diversity with weight=0 equals greedy", {
  strategy_div <- batch_strategy_diversity(diversity_weight = 0)
  strategy_greedy <- batch_strategy_greedy()
  search_space <- create_numeric_search_space()

  set.seed(42)
  candidates <- create_test_candidates(10)
  scores <- runif(10)

  indices_div <- strategy_div(candidates, scores, batch_size = 3, NULL, NULL, search_space)
  indices_greedy <- strategy_greedy(candidates, scores, batch_size = 3, NULL, NULL, search_space)

  expect_equal(indices_div, indices_greedy)
})

test_that("batch_strategy_diversity with weight=1 maximizes distances", {
  strategy <- batch_strategy_diversity(diversity_weight = 1)
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))

  # Spread out candidates
  candidates <- data.table(x = seq(0, 1, length.out = 10))
  # All equal scores - so pure diversity
  scores <- rep(0.5, 10)

  indices <- strategy(candidates, scores, batch_size = 3, NULL, NULL, search_space)

  # With pure diversity, should select well-spread points
  # First: any (equal scores), then maximize distance
  # Should include endpoints
  expect_length(indices, 3)
  # Check that selected points are spread out
  selected_x <- candidates$x[indices]
  expect_true(max(selected_x) - min(selected_x) >= 0.5)
})

test_that("batch_strategy_diversity balances acquisition and diversity", {
  strategy <- batch_strategy_diversity(diversity_weight = 0.5)
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))

  # Clustered candidates with varying scores
  candidates <- data.table(x = c(0.0, 0.1, 0.2, 0.5, 0.9, 1.0))
  # Best scores are at left cluster
  scores <- c(0.1, 0.15, 0.2, 0.6, 0.7, 0.8)

  indices <- strategy(candidates, scores, batch_size = 3, NULL, NULL, search_space)

  # Should balance between low scores and diversity
  expect_length(indices, 3)
  # First should be best score (index 1)
  expect_true(1 %in% indices)
})


# =============================================================================
# compute_gower_distance Tests
# =============================================================================

test_that("compute_gower_distance handles numeric variables", {
  search_space <- ps(
    x1 = p_dbl(lower = 0, upper = 1),
    x2 = p_dbl(lower = 0, upper = 10)
  )

  x <- data.table(x1 = c(0, 1), x2 = c(0, 10))
  y <- data.table(x1 = c(0.5), x2 = c(5))

  dist <- compute_gower_distance(x, y, search_space)

  expect_matrix(dist)
  expect_equal(nrow(dist), 2)
  expect_equal(ncol(dist), 1)
  # Both x rows should have distance 0.5 to y (midpoint)
  # d(row1, y) = (|0-0.5|/1 + |0-5|/10) / 2 = (0.5 + 0.5) / 2 = 0.5
  # d(row2, y) = (|1-0.5|/1 + |10-5|/10) / 2 = (0.5 + 0.5) / 2 = 0.5
  expect_equal(dist[1, 1], 0.5, tolerance = 1e-10)
  expect_equal(dist[2, 1], 0.5, tolerance = 1e-10)
})

test_that("compute_gower_distance handles self-distance", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))

  x <- data.table(x = c(0, 0.5, 1))

  dist <- compute_gower_distance(x, search_space = search_space)

  expect_matrix(dist)
  expect_equal(nrow(dist), 3)
  expect_equal(ncol(dist), 3)
  # Diagonal should be 0
  expect_equal(diag(dist), c(0, 0, 0))
  # Symmetric
  expect_equal(dist[1, 2], dist[2, 1])
})

test_that("compute_gower_distance handles categorical variables", {
  search_space <- ps(x = p_fct(levels = c("a", "b", "c")))

  x <- data.table(x = factor(c("a", "b", "c"), levels = c("a", "b", "c")))
  y <- data.table(x = factor(c("a"), levels = c("a", "b", "c")))

  dist <- compute_gower_distance(x, y, search_space)

  # Same category = 0, different = 1
  expect_equal(dist[1, 1], 0)  # a vs a

  expect_equal(dist[2, 1], 1)  # b vs a
  expect_equal(dist[3, 1], 1)  # c vs a
})

test_that("compute_gower_distance handles mixed types", {
  search_space <- create_mixed_search_space()

  x <- data.table(
    x1 = c(0, 1),
    x2 = c(1L, 10L),
    x3 = factor(c("a", "a"), levels = c("a", "b", "c"))
  )
  y <- data.table(
    x1 = 0.5,
    x2 = 5L,
    x3 = factor("b", levels = c("a", "b", "c"))
  )

  dist <- compute_gower_distance(x, y, search_space)

  expect_matrix(dist)
  expect_equal(nrow(dist), 2)
  expect_equal(ncol(dist), 1)
  # Distances should be in [0, 1]
  expect_true(all(dist >= 0 & dist <= 1))
})


# =============================================================================
# Edge Cases
# =============================================================================

test_that("strategies handle all tied scores", {
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(10)
  scores <- rep(0.5, 10)  # all tied

  greedy <- batch_strategy_greedy()
  indices_greedy <- greedy(candidates, scores, batch_size = 3, NULL, NULL, search_space)
  expect_length(indices_greedy, 3)

  diversity <- batch_strategy_diversity()
  indices_div <- diversity(candidates, scores, batch_size = 3, NULL, NULL, search_space)
  expect_length(indices_div, 3)
})

test_that("strategies handle single candidate", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  candidates <- data.table(x = 0.5)
  scores <- 0.3

  greedy <- batch_strategy_greedy()
  expect_equal(greedy(candidates, scores, batch_size = 1, NULL, NULL, search_space), 1)

  diversity <- batch_strategy_diversity()
  expect_equal(diversity(candidates, scores, batch_size = 1, NULL, NULL, search_space), 1)
})

test_that("strategies return unique indices", {
  search_space <- create_numeric_search_space()
  candidates <- create_test_candidates(20)
  scores <- runif(20)

  greedy <- batch_strategy_greedy()
  indices <- greedy(candidates, scores, batch_size = 10, NULL, NULL, search_space)
  expect_length(unique(indices), 10)

  diversity <- batch_strategy_diversity()
  indices <- diversity(candidates, scores, batch_size = 10, NULL, NULL, search_space)
  expect_length(unique(indices), 10)

  local_pen <- batch_strategy_local_penalization()
  indices <- local_pen(candidates, scores, batch_size = 10, NULL, NULL, search_space)
  expect_length(unique(indices), 10)
})
