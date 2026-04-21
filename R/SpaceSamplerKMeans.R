#' @title K-Means Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include sugar_al_distances.R
#' @include SpaceSamplerDistance.R
#'
#' @name mlr_space_samplers_kmeans
#'
#' @description
#' K-means-based pool sampler.
#'
#' The wrapped distance must expose a numeric reference embedding or numeric
#' reference columns after `set_reference_points()`.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSamplerDistance].
#'
#' @section Construction:
#' ```
#' clx_sps("kmeans")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerKMeans <- R6Class("SpaceSamplerKMeans",
  inherit = SpaceSamplerDistance,

  public = list(

    #' @description
    #' Creates a new K-means space sampler.
    #'
    #' @param distance ([ALDistance])\cr
    #'   Distance used to construct the fitted geometry.
    initialize = function(distance = clx_ald("affine")) {
      super$initialize(
        id = "kmeans",
        distance = distance,
        deterministic = FALSE,
        label = "K-Means Space Sampler",
        man = "celecx::mlr_space_samplers_kmeans"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (is.null(pool)) {
        # TODO Add a continuous analogue for K-means thinning without a finite pool.
        stopf("SpaceSampler '%s' currently requires a pool", self$id)
      }

      fit_data <- space_sampler_distance_fit_data(pool, known_pool)
      on.exit(self$distance$clear())
      self$distance$fit_pool(fit_data$xdt, search_space)
      self$distance$set_reference_points(fit_data$xdt)

      geometry_all <- space_sampler_distance_pool_matrix(
        distance = self$distance
      )

      pool[space_sampler_kmeans_indices(
        geometry_all = geometry_all,
        pool_idx = fit_data$pool_idx,
        n = n
      )]
    }
  )
)

mlr_space_samplers$add("kmeans", SpaceSamplerKMeans)



space_sampler_distance_pool_matrix <- function(distance) {
  state <- distance$state

  if (inherits(distance, "ALDistanceGeometry")) {
    return(distance$reference_embedding)
  }

  if (!is.null(state$xdt_reference)) {
    xdt_reference <- state$xdt_reference
    matrix_columns <- imap(xdt_reference, function(col, id) {
      if (!is.numeric(col) && !is.logical(col)) {
        stopf(
          "SpaceSamplerKMeans requires a numeric reference representation, but column '%s' has type '%s'",
          id,
          typeof(col)
        )
      }
      as.numeric(col)
    })

    return(do.call(cbind, matrix_columns))
  }

  stopf(
    "SpaceSamplerKMeans requires distance '%s' to expose a reference embedding or numeric reference columns",
    distance$id
  )
}

space_sampler_kmeans_indices <- function(geometry_all, pool_idx, n) {
  geometry_candidates <- geometry_all[pool_idx, , drop = FALSE]

  if (n == 1L) {
    centroid <- colMeans(geometry_all)
    cluster_distances_sq <- matrix(
      rowSums(sweep(geometry_candidates, 2L, centroid, "-")^2),
      ncol = 1L
    )
    return(space_sampler_cluster_representatives(cluster_distances_sq))
  }

  km <- stats::kmeans(geometry_all, centers = n, nstart = 10L)
  centers <- km$centers

  cluster_distances_sq <- vapply(seq_len(n), function(j) {
    rowSums(sweep(geometry_candidates, 2L, centers[j, ], "-")^2)
  }, numeric(nrow(geometry_candidates)))
  cluster_distances_sq <- matrix(cluster_distances_sq, nrow = nrow(geometry_candidates), ncol = n)

  space_sampler_cluster_representatives(cluster_distances_sq)
}
