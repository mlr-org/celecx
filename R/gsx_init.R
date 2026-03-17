#' @title Initialization Functions for Pool-Based Active Learning
#'
#' @description
#' Initialization strategies for selecting the first batch of pool points
#' before model-based scoring begins.
#'
#' - `gsx_init()`: centroid-nearest start, then farthest-first (GS family)
#' - `kmeans_init()`: K-means clustering, nearest-to-centroid (IDEAL)
#'
#' @name pool_init
#' @keywords internal
NULL

#' GSx initialization: centroid-nearest start, then farthest-first.
#'
#' The first point is the pool point nearest to the pool centroid.
#' Each subsequent point maximizes the minimum squared Euclidean distance
#' to all already-selected points. Ties are broken by smallest index.
#'
#' @param pool_x (`matrix`)\cr
#'   Numeric matrix (N x d) of pre-scaled features.
#' @param n_init (`integer(1)`)\cr
#'   Number of points to select (clamped to N).
#' @return `integer` vector of selected row indices (1-based), in selection order.
#' @keywords internal
gsx_init <- function(pool_x, n_init) {
  N <- nrow(pool_x)
  n_init <- min(n_init, N)

  selected <- integer(n_init)

  # Step 1: nearest to centroid (ties: smallest index via which.min)
  centroid <- colMeans(pool_x)
  dists_to_centroid <- rowSums(sweep(pool_x, 2, centroid, "-")^2)
  selected[1L] <- which.min(dists_to_centroid)

  if (n_init == 1L) return(selected)

  # Initialize min squared distances from each point to the selected set
  min_sq_dist <- rowSums(sweep(pool_x, 2, pool_x[selected[1L], ], "-")^2)
  min_sq_dist[selected[1L]] <- -Inf

  # Step 2: farthest-first expansion
  for (k in 2:n_init) {
    best <- which.max(min_sq_dist)  # ties: smallest index
    selected[k] <- best

    new_sq_dist <- rowSums(sweep(pool_x, 2, pool_x[best, ], "-")^2)
    min_sq_dist <- pmin(min_sq_dist, new_sq_dist)
    min_sq_dist[best] <- -Inf
  }

  selected
}

#' K-means initialization for IDEAL.
#'
#' Runs K-means on the scaled pool with K = `n_init` clusters,
#' then greedily assigns the nearest unique pool point to each centroid.
#'
#' @param pool_x (`matrix`)\cr
#'   Numeric matrix (N x d) of pre-scaled features.
#' @param n_init (`integer(1)`)\cr
#'   Number of points to select (clamped to N).
#' @return `integer` vector of selected row indices (1-based).
#' @keywords internal
kmeans_init <- function(pool_x, n_init) {
  N <- nrow(pool_x)
  n_init <- min(n_init, N)

  if (n_init >= N) return(seq_len(N))

  # Edge case: single point
  if (n_init == 1L) {
    centroid <- colMeans(pool_x)
    dists <- rowSums(sweep(pool_x, 2, centroid, "-")^2)
    return(which.min(dists))
  }

  km <- stats::kmeans(pool_x, centers = n_init, nstart = 10)
  centers <- km$centers

  # Greedy nearest-unique-pool-point-to-centroid
  selected <- integer(n_init)
  available <- rep(TRUE, N)

  for (j in seq_len(n_init)) {
    diffs <- sweep(pool_x, 2, centers[j, ], "-")
    dists <- rowSums(diffs^2)
    dists[!available] <- Inf
    best <- which.min(dists)
    selected[j] <- best
    available[best] <- FALSE
  }

  selected
}
