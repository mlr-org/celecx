#' @title Affine Active Learning Distance
#'
#' @include mlr_al_distances.R
#' @include ALDistanceGeometry.R
#'
#' @name mlr_al_distances_affine
#'
#' @description
#' Distance for IDEAL-style active learning methods.
#'
#' All search-space columns must be numeric or integer. Each geometry column is
#' affine-scaled to the interval \eqn{[-1, 1]}. With a finite pool, scaling uses
#' the candidate-pool minima and maxima. With `xdt = NULL`, scaling uses the
#' finite search-space bounds. Zero-range columns and missing values are mapped
#' to 0 in the geometry.
#'
#' @format [R6::R6Class] object inheriting from [ALDistanceGeometry].
#'
#' @section Construction:
#' ```
#' clx_ald("affine")
#' ```
#'
#' @export
#' @family ALDistance
ALDistanceAffine <- R6Class("ALDistanceAffine",
  inherit = ALDistanceGeometry,

  public = list(

    #' @description
    #' Creates an affine active-learning distance.
    initialize = function() {
      super$initialize(
        id = "affine",
        label = "Affine AL Distance",
        man = "celecx::mlr_al_distances_affine"
      )
    }
  ),

  private = list(
    .fit_geometry = function(xdt, search_space) {
      assert_param_set(search_space, no_deps = TRUE)
      if (!search_space$all_numeric) stop("ALDistanceAffine requires a numeric search space")

      if (is.null(xdt)) {
        ids <- search_space$ids()
        column_min <- as.numeric(search_space$lower[ids])
        column_max <- as.numeric(search_space$upper[ids])
        names(column_min) <- ids
        names(column_max) <- ids
        dimension <- length(ids)

        non_finite <- !is.finite(column_min) | !is.finite(column_max)
        if (any(non_finite)) {
          stopf(
            "ALDistanceAffine requires finite search-space bounds for %s when fitted with xdt = NULL",
            str_collapse(ids[non_finite], quote = "'")
          )
        }
      } else {
        raw_geom <- as.matrix(xdt)
        assert_matrix(raw_geom, mode = "numeric")  # should never be a problem if pool & search space are compatible

        column_min <- apply(raw_geom, 2L, min, na.rm = TRUE)
        column_max <- apply(raw_geom, 2L, max, na.rm = TRUE)
        dimension <- ncol(raw_geom)

        non_finite <- !is.finite(column_min) | !is.finite(column_max)
        if (any(non_finite)) {
          column_min[non_finite] <- 0
          column_max[non_finite] <- 0
        }
      }

      column_zero_range <- column_min == column_max

      list(
        dimension = dimension,
        column_min = column_min,
        column_max = column_max,
        column_zero_range = column_zero_range
      )
    },

    .transform = function(xdt, state) {
      geometry_query <- as.matrix(xdt)
      assert_matrix(geometry_query, mode = "numeric")  # should never be a problem if pool & search space are compatible
      al_distance_affine_scale(
        geometry_query,
        mins = state$column_min,
        maxs = state$column_max,
        zero_range = state$column_zero_range
      )
    }
  )
)

al_distance_affine_scale <- function(raw_geom, mins, maxs, zero_range) {
  ranges <- maxs - mins
  mids <- (maxs + mins) / 2

  ranges[zero_range] <- 1

  scaled <- sweep(raw_geom, 2L, mids, "-")
  scaled <- sweep(scaled, 2L, ranges / 2, "/")
  scaled[is.na(scaled)] <- 0
  if (any(zero_range)) {
    scaled[, zero_range] <- 0
  }
  scaled
}

mlr_al_distances$add("affine", ALDistanceAffine)
