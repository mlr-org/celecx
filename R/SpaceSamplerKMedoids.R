#' @title K-Medoids Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include sugar_al_distances.R
#' @include SpaceSamplerDistance.R
#'
#' @name mlr_space_samplers_kmedoids
#'
#' @description
#' K-medoids-style pool sampler based on an arbitrary [ALDistance].
#'
#' Medoids are selected from the candidate pool using a greedy build step
#' followed by swap-based local improvement. The optimization target is the sum
#' of distances to the nearest selected medoid.
#'
#' If `known_pool` is given, those points are treated as fixed medoids and the
#' selected pool points are optimized to complement them.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSamplerDistance].
#'
#' @section Construction:
#' ```
#' clx_sps("kmedoids")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerKMedoids <- R6Class("SpaceSamplerKMedoids",
  inherit = SpaceSamplerDistance,

  public = list(

    #' @description
    #' Creates a new k-medoids space sampler.
    #'
    #' @param distance ([ALDistance])\cr
    #'   Distance used for the medoid objective.
    initialize = function(distance = clx_ald("gower")) {
      super$initialize(
        id = "kmedoids",
        distance = distance,
        deterministic = TRUE,
        label = "K-Medoids Space Sampler",
        man = "celecx::mlr_space_samplers_kmedoids"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (is.null(pool)) {
        # TODO Add a continuous analogue for K-medoids thinning without a finite pool.
        stopf("SpaceSampler '%s' currently requires a pool", self$id)
      }

      fit_data <- space_sampler_distance_fit_data(pool, known_pool)

      distance_matrix <- space_sampler_self_distance_matrix(self$distance, fit_data$xdt, search_space)

      medoid_idx <- space_sampler_kmedoids_build(
        distance_matrix = distance_matrix,
        pool_idx = fit_data$pool_idx,
        n = n,
        fixed_idx = fit_data$known_idx
      )
      medoid_idx <- space_sampler_kmedoids_swap(
        distance_matrix = distance_matrix,
        pool_idx = fit_data$pool_idx,
        medoid_idx = medoid_idx,
        fixed_idx = fit_data$known_idx
      )
      pool[match(medoid_idx, fit_data$pool_idx)]
    }
  )
)

mlr_space_samplers$add("kmedoids", SpaceSamplerKMedoids)

space_sampler_kmedoids_build <- function(distance_matrix, pool_idx, n, fixed_idx = integer(0)) {
  medoid_idx <- integer(n)
  point_cost <- space_sampler_kmedoids_point_cost(
    distance_matrix = distance_matrix,
    medoid_idx = fixed_idx
  )

  for (step in seq_len(n)) {
    available_idx <- setdiff(pool_idx, medoid_idx)
    candidate_cost <- colSums(pmin(distance_matrix[, available_idx, drop = FALSE], point_cost))
    best_idx <- available_idx[[which.min(candidate_cost)]]
    medoid_idx[[step]] <- best_idx
    point_cost <- pmin(point_cost, distance_matrix[, best_idx])
  }

  medoid_idx
}

space_sampler_kmedoids_swap <- function(distance_matrix, pool_idx, medoid_idx, fixed_idx = integer(0)) {
  repeat {
    current_objective <- space_sampler_kmedoids_objective(
      distance_matrix = distance_matrix,
      medoid_idx = c(fixed_idx, medoid_idx)
    )
    best_objective <- current_objective
    best_swap <- NULL

    non_medoids <- setdiff(pool_idx, medoid_idx)

    for (medoid_pos in seq_along(medoid_idx)) {
      base_idx <- c(fixed_idx, medoid_idx[-medoid_pos])
      base_cost <- space_sampler_kmedoids_point_cost(
        distance_matrix = distance_matrix,
        medoid_idx = base_idx
      )

      for (candidate_idx in non_medoids) {
        objective <- sum(pmin(base_cost, distance_matrix[, candidate_idx]))
        if (objective < best_objective) {
          best_objective <- objective
          best_swap <- list(medoid_pos = medoid_pos, candidate_idx = candidate_idx)
        }
      }
    }

    if (is.null(best_swap)) {
      return(medoid_idx)
    }

    medoid_idx[[best_swap$medoid_pos]] <- best_swap$candidate_idx
  }
}

space_sampler_kmedoids_point_cost <- function(distance_matrix, medoid_idx) {
  if (!length(medoid_idx)) {
    return(rep(Inf, nrow(distance_matrix)))
  }

  apply(distance_matrix[, medoid_idx, drop = FALSE], 1L, min)
}

space_sampler_kmedoids_objective <- function(distance_matrix, medoid_idx) {
  sum(space_sampler_kmedoids_point_cost(
    distance_matrix = distance_matrix,
    medoid_idx = medoid_idx
  ))
}
