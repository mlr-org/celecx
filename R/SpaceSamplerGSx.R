#' @title GSx Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include sugar_al_distances.R
#' @include SpaceSamplerDistance.R
#'
#' @name mlr_space_samplers_gsx
#'
#' @description
#' Greedy-sampling-in-input-space sampler.
#'
#' Without `known_pool`, the first selected point is the pool point minimizing
#' the sum of squared distances to all other pool points. Subsequent points are
#' chosen by farthest-first traversal, maximizing the minimum squared distance to
#' the already-selected or known points.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSamplerDistance].
#'
#' @section Construction:
#' ```
#' clx_sps("gsx")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerGSx <- R6Class("SpaceSamplerGSx",
  inherit = SpaceSamplerDistance,

  public = list(

    #' @description
    #' Creates a new GSx space sampler.
    #'
    #' @param distance ([ALDistance])\cr
    #'   Distance used for the GSx selection rule.
    initialize = function(distance = clx_ald("standardize")) {
      super$initialize(
        id = "gsx",
        distance = distance,
        deterministic = FALSE,
        label = "GSx Space Sampler",
        man = "celecx::mlr_space_samplers_gsx"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (is.null(pool)) {
        # TODO Add a continuous analogue for GSx without a finite pool.
        stopf("SpaceSampler '%s' currently requires a pool", self$id)
      }

      fit_data <- space_sampler_distance_fit_data(pool, known_pool)
      on.exit(self$distance$clear())
      self$distance$fit_pool(fit_data$xdt, search_space)
      self$distance$set_reference_points(fit_data$xdt)

      pool_distances_sq <- self$distance$distances(pool, i = fit_data$pool_idx)^2
      known_distances_sq <- if (length(fit_data$known_idx)) {
        self$distance$distances(pool, i = fit_data$known_idx)^2
      }

      pool[space_sampler_gsx_indices(
        pool_distances_sq = pool_distances_sq,
        n = n,
        known_distances_sq = known_distances_sq
      )]
    }
  )
)

mlr_space_samplers$add("gsx", SpaceSamplerGSx)


space_sampler_gsx_indices <- function(pool_distances_sq, n, known_distances_sq = NULL) {
  n_pool <- nrow(pool_distances_sq)
  selected <- integer(n)

  if (!NCOL(known_distances_sq)) {
    selected[1L] <- which.min(rowSums(pool_distances_sq))
    min_sq_dist <- pool_distances_sq[, selected[1L]]
    min_sq_dist[selected[1L]] <- -Inf
    start <- 2L
  } else {
    min_sq_dist <- apply(known_distances_sq, 1L, min)
    start <- 1L
  }

  if (start > n) {
    return(selected)
  }

  for (k in seq.int(start, n)) {
    best <- which.max(min_sq_dist)
    selected[k] <- best
    min_sq_dist <- pmin(min_sq_dist, pool_distances_sq[, best])
    min_sq_dist[best] <- -Inf
  }

  selected
}
