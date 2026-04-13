#' @title Geometry-Based Active Learning Distance
#'
#' @include ALDistance.R
#'
#' @description
#' Abstract base class for active-learning distances based on Euclidean
#' distances in a geometry space.
#'
#' Subclasses implement how the pool or search space determines geometry-space
#' scaling and how reference and query points are transformed into that same
#' space.
#'
#' Subclasses must implement the following methods:
#' \itemize{
#'   \item \code{private$.fit_geometry(xdt, search_space)}: Fit the geometry from the pool and search space, or from the search space only when \code{xdt} is \code{NULL}.
#'     The result must contain an integer \code{dimension} giving the number of geometry columns.
#'   \item \code{private$.transform(xdt, state)}: Transform query points into the geometry space.
#'     The result must be a matrix with one row per query point and \code{state$dimension} columns.
#' }
#'
#' @export
#' @family ALDistance
ALDistanceGeometry <- R6Class("ALDistanceGeometry",
  inherit = ALDistance,

  active = list(

    #' @field dimension (`integer(1)` | `NULL`)
    #'   Dimension of the fitted geometry space.
    dimension = function(rhs) {
      assert_ro_binding(rhs)
      if (!self$is_fitted) {
        return(NULL)
      }
      private$.state$dimension
    },

    #' @field reference_embedding (`matrix()` | `NULL`)
    #'   Current reference points transformed into the fitted geometry space.
    #'   `NULL` if the distance has not been fitted or reference points have not
    #'   been set.
    reference_embedding = function(rhs) {
      assert_ro_binding(rhs)
      if (!self$is_fitted) {
        return(NULL)
      }
      private$.state$geometry_reference
    }
  ),

  private = list(
    .fit_pool = function(xdt, search_space) {
      state <- private$.fit_geometry(xdt, search_space)
      assert_list(state)
      assert_names(names(state), must.include = "dimension")
      state$dimension <- assert_count(state$dimension, positive = TRUE, tol = 0)
      state
    },

    .distances = function(xdt, state, i = NULL) {
      geometry_reference <- state$geometry_reference
      geometry_query <- private$.transform(xdt, state = state)
      assert_matrix(
        geometry_query,
        mode = "numeric",
        any.missing = FALSE,
        nrows = nrow(xdt),
        ncols = state$dimension
      )

      if (!is.null(i)) {
        geometry_reference <- geometry_reference[i, , drop = FALSE]
      }

      private$.pairwise_euclidean(geometry_query, geometry_reference)
    },

    .set_reference_points = function(xdt, state) {
      geometry_reference <- private$.transform(xdt, state = state)
      assert_matrix(
        geometry_reference,
        mode = "numeric",
        any.missing = FALSE,
        nrows = nrow(xdt),
        ncols = state$dimension
      )

      state$geometry_reference <- geometry_reference
      state
    },

    .fit_geometry = function(xdt, search_space) {
      stop("Abstract.")
    },

    .transform = function(xdt, state) {
      stop("Abstract.")
    },

    .pairwise_euclidean = function(geometry_query, geometry_reference) {
      norm_sq_query <- rowSums(geometry_query^2)
      norm_sq_reference <- rowSums(geometry_reference^2)
      distances_sq <- outer(norm_sq_query, norm_sq_reference, "+") -
        2 * tcrossprod(geometry_query, geometry_reference)

      sqrt(pmax(distances_sq, 0))
    }
  )
)
