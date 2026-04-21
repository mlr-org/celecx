#' @title Relational K-Means Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include sugar_al_distances.R
#' @include SpaceSamplerDistance.R
#'
#' @name mlr_space_samplers_relational_kmeans
#'
#' @description
#' K-means-style pool sampler driven directly by the pairwise distance matrix.
#'
#' This sampler clusters the fitted pool with the relational k-means objective.
#' For Euclidean distances it is equivalent to ordinary k-means in an unknown
#' embedding. For non-Euclidean distances it remains a heuristic.
#'
#' If `known_pool` is given, those points influence the implicit centroids but
#' are not eligible for selection.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSamplerDistance].
#'
#' @section Construction:
#' ```
#' clx_sps("relational_kmeans")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerRelationalKMeans <- R6Class("SpaceSamplerRelationalKMeans",
  inherit = SpaceSamplerDistance,

  public = list(

    #' @description
    #' Creates a new relational k-means space sampler.
    #'
    #' @param distance ([ALDistance])\cr
    #'   Distance used to construct the pairwise dissimilarity matrix.
    initialize = function(distance = clx_ald("affine")) {
      super$initialize(
        id = "relational_kmeans",
        distance = distance,
        deterministic = FALSE,
        label = "Relational K-Means Space Sampler",
        man = "celecx::mlr_space_samplers_relational_kmeans"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (is.null(pool)) {
        # TODO Add a continuous analogue for relational K-means thinning without a finite pool.
        stopf("SpaceSampler '%s' currently requires a pool", self$id)
      }

      fit_data <- space_sampler_distance_fit_data(pool, known_pool)

      distance_matrix_sq <- space_sampler_self_distance_matrix(self$distance, fit_data$xdt, search_space)^2

      clustering <- space_sampler_relational_kmeans_fit(
        distance_matrix_sq = distance_matrix_sq,
        n = n,
        nstart = 10L
      )
      cluster_distances_sq <- space_sampler_relational_kmeans_distances(
        distance_matrix_sq = distance_matrix_sq,
        cluster = clustering$cluster,
        n = n
      )

      pool[space_sampler_cluster_representatives(cluster_distances_sq[fit_data$pool_idx, , drop = FALSE])]
    }
  )
)

mlr_space_samplers$add("relational_kmeans", SpaceSamplerRelationalKMeans)

space_sampler_relational_kmeans_fit <- function(distance_matrix_sq, n, nstart, max_iter = 100L) {
  n_points <- nrow(distance_matrix_sq)

  if (n == 1L) {
    cluster <- rep(1L, n_points)
    return(list(
      cluster = cluster,
      objective = sum(space_sampler_relational_kmeans_distances(
        distance_matrix_sq = distance_matrix_sq,
        cluster = cluster,
        n = n
      ))
    ))
  }

  best_cluster <- NULL
  best_objective <- Inf

  for (start in seq_len(nstart)) {
    initial_centers <- sample.int(n_points, n)
    cluster <- max.col(-distance_matrix_sq[, initial_centers, drop = FALSE], ties.method = "first")

    for (iteration in seq_len(max_iter)) {
      cluster_distances_sq <- space_sampler_relational_kmeans_distances(
        distance_matrix_sq = distance_matrix_sq,
        cluster = cluster,
        n = n
      )
      new_cluster <- max.col(-cluster_distances_sq, ties.method = "first")
      new_cluster <- space_sampler_relational_kmeans_repair_empty(
        cluster_distances_sq = cluster_distances_sq,
        cluster = new_cluster
      )

      if (identical(new_cluster, cluster)) {
        break
      }

      cluster <- new_cluster
    }

    cluster_distances_sq <- space_sampler_relational_kmeans_distances(
      distance_matrix_sq = distance_matrix_sq,
      cluster = cluster,
      n = n
    )
    objective <- sum(cluster_distances_sq[cbind(seq_len(n_points), cluster)])

    if (objective < best_objective) {
      best_cluster <- cluster
      best_objective <- objective
    }
  }

  list(cluster = best_cluster, objective = best_objective)
}

space_sampler_relational_kmeans_distances <- function(distance_matrix_sq, cluster, n) {
  n_points <- nrow(distance_matrix_sq)
  cluster_distances_sq <- matrix(Inf, nrow = n_points, ncol = n)

  for (j in seq_len(n)) {
    cluster_idx <- which(cluster == j)
    if (!length(cluster_idx)) {
      next
    }

    cluster_size <- length(cluster_idx)
    cluster_distances_sq[, j] <- rowSums(distance_matrix_sq[, cluster_idx, drop = FALSE]) / cluster_size -
      sum(distance_matrix_sq[cluster_idx, cluster_idx, drop = FALSE]) / (2 * cluster_size^2)
  }

  cluster_distances_sq
}

space_sampler_relational_kmeans_repair_empty <- function(cluster_distances_sq, cluster) {
  cluster_size <- tabulate(cluster, nbins = ncol(cluster_distances_sq))
  empty_clusters <- which(cluster_size == 0L)

  if (!length(empty_clusters)) {
    return(cluster)
  }

  point_objective <- cluster_distances_sq[cbind(seq_along(cluster), cluster)]
  moved <- rep(FALSE, length(cluster))

  for (empty_cluster in empty_clusters) {
    candidates <- which(cluster_size[cluster] > 1L & !moved)
    if (!length(candidates)) {
      stop("Could not repair empty relational K-means cluster.")
    }

    best <- candidates[which.max(point_objective[candidates])]
    cluster_size[cluster[best]] <- cluster_size[cluster[best]] - 1L
    cluster[best] <- empty_cluster
    cluster_size[empty_cluster] <- 1L
    moved[best] <- TRUE
  }

  cluster
}
