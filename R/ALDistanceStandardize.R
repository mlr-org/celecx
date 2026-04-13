#' @title Standardized Active Learning Distance
#'
#' @include mlr_al_distances.R
#' @include ALDistanceGeometry.R
#'
#' @name mlr_al_distances_standardize
#'
#' @description
#' Distance for the greedy-sampling family of active learning methods.
#'
#' Numeric and integer columns stay numeric, logical and factor columns are
#' one-hot encoded, and every geometry column is standardized to mean 0 and
#' standard deviation 1. With a finite pool, center and scale are estimated from
#' the candidate pool. With `xdt = NULL`, numeric centers and scales are derived
#' from finite search-space bounds under a uniform distribution, and dummy-column
#' centers and scales are derived from uniform categorical levels.
#'
#' Missing or dependency-inactive values are mapped to 0 in the standardized
#' geometry.
#'
#' @format [R6::R6Class] object inheriting from [ALDistanceGeometry].
#'
#' @section Construction:
#' ```
#' clx_ald("standardize")
#' ```
#'
#' @export
#' @family ALDistance
ALDistanceStandardize <- R6Class("ALDistanceStandardize",
  inherit = ALDistanceGeometry,

  public = list(

    #' @description
    #' Creates a standardized active-learning distance.
    initialize = function() {
      super$initialize(
        id = "standardize",
        label = "Standardized AL Distance",
        man = "celecx::mlr_al_distances_standardize"
      )
    }
  ),

  private = list(
    .fit_geometry = function(xdt, search_space) {
      column_specs <- al_distance_standardize_specs(search_space, xdt)
      if (is.null(xdt)) {
        fit_stats <- al_distance_standardize_search_space_stats(search_space, column_specs)
      } else {
        raw_geom <- al_distance_standardize_encode(xdt, column_specs)
        if (!ncol(raw_geom)) {
          stopf("ALDistance '%s' requires at least one geometry column", self$id)
        }

        column_center <- colMeans(raw_geom, na.rm = TRUE)
        column_center[!is.finite(column_center)] <- 0

        column_scale <- apply(raw_geom, 2L, stats::sd, na.rm = TRUE)
        column_zero_var <- !is.finite(column_scale) | column_scale == 0
        column_scale[column_zero_var] <- 1
        fit_stats <- list(
          dimension = ncol(raw_geom),
          column_center = column_center,
          column_scale = column_scale,
          column_zero_var = column_zero_var
        )
      }

      list(
        dimension = fit_stats$dimension,
        column_specs = column_specs,
        column_center = fit_stats$column_center,
        column_scale = fit_stats$column_scale,
        column_zero_var = fit_stats$column_zero_var
      )
    },

    .transform = function(xdt, state) {
      geometry_query <- al_distance_standardize_encode(xdt, state$column_specs)
      al_distance_standardize_scale(
        geometry_query,
        center = state$column_center,
        scale = state$column_scale,
        zero_var = state$column_zero_var
      )
    }
  )
)

al_distance_standardize_specs <- function(search_space, xdt = NULL) {
  ids <- search_space$ids()

  specs <- lapply(ids, function(id) {
    param_class <- search_space$class[[id]]
    col <- if (is.null(xdt)) NULL else xdt[[id]]

    if (param_class %in% c("ParamDbl", "ParamInt")) {
      return(list(
        id = id,
        kind = "numeric",
        param_class = param_class,
        levels = NULL,
        column_names = id
      ))
    }

    if (identical(param_class, "ParamLgl")) {
      levels <- c("FALSE", "TRUE")
      return(list(
        id = id,
        kind = "dummy",
        param_class = param_class,
        levels = levels,
        column_names = paste0(id, "__", levels)
      ))
    }

    if (identical(param_class, "ParamFct")) {
      levels <- search_space$levels[[id]]
      return(list(
        id = id,
        kind = "dummy",
        param_class = param_class,
        levels = levels,
        column_names = paste0(id, "__", make.names(levels, unique = TRUE))
      ))
    }

    if (!is.null(col) && (is.numeric(col) || is.integer(col))) {
      return(list(
        id = id,
        kind = "numeric",
        param_class = param_class,
        levels = NULL,
        column_names = id
      ))
    }

    stopf(
      "ALDistanceStandardize does not support search-space column '%s' of class '%s'",
      id,
      param_class
    )
  })

  set_names(specs, ids)
}

al_distance_standardize_search_space_stats <- function(search_space, specs) {
  assert_param_set(search_space)
  assert_list(specs, names = "strict")

  column_center <- unlist(lapply(specs, function(spec) {
    if (identical(spec$kind, "numeric")) {
      bounds <- al_distance_standardize_numeric_bounds(search_space, spec$id)
      return((bounds$lower + bounds$upper) / 2)
    }

    rep(1 / length(spec$levels), length(spec$levels))
  }), use.names = FALSE)

  column_scale <- unlist(lapply(specs, function(spec) {
    if (identical(spec$kind, "numeric")) {
      bounds <- al_distance_standardize_numeric_bounds(search_space, spec$id)
      return(al_distance_standardize_uniform_scale(
        lower = bounds$lower,
        upper = bounds$upper,
        param_class = spec$param_class
      ))
    }

    p <- 1 / length(spec$levels)
    rep(sqrt(p * (1 - p)), length(spec$levels))
  }), use.names = FALSE)

  column_names <- unlist(map(specs, "column_names"), use.names = FALSE)
  names(column_center) <- column_names
  names(column_scale) <- column_names
  column_zero_var <- !is.finite(column_scale) | column_scale == 0
  column_scale[column_zero_var] <- 1

  list(
    dimension = length(column_center),
    column_center = column_center,
    column_scale = column_scale,
    column_zero_var = column_zero_var
  )
}

al_distance_standardize_numeric_bounds <- function(search_space, id) {
  lower <- search_space$lower[[id]]
  upper <- search_space$upper[[id]]
  if (!is.finite(lower) || !is.finite(upper)) {
    stopf(
      "ALDistanceStandardize requires finite search-space bounds for '%s' when fitted with xdt = NULL",
      id
    )
  }

  list(lower = lower, upper = upper)
}

al_distance_standardize_uniform_scale <- function(lower, upper, param_class) {
  if (lower >= upper) {
    return(0)
  }

  if (identical(param_class, "ParamInt")) {
    n_values <- upper - lower + 1L
    return(sqrt((n_values^2 - 1) / 12))
  }

  (upper - lower) / sqrt(12)
}

al_distance_standardize_encode <- function(xdt, specs) {
  assert_data_table(xdt)
  assert_list(specs, names = "strict")

  encoded <- lapply(specs, function(spec) {
    col <- xdt[[spec$id]]

    if (spec$kind == "numeric") {
      return(matrix(
        as.numeric(col),
        ncol = 1L,
        dimnames = list(NULL, spec$column_names)
      ))
    }

    values <- as.character(col)

    assert_subset(values[!is.na(values)], spec$levels)

    mat <- matrix(
      NA_real_,
      nrow = nrow(xdt),
      ncol = length(spec$levels),
      dimnames = list(NULL, spec$column_names)
    )
    observed <- which(!is.na(values))
    mat[observed, ] <- vapply(spec$levels, function(level) {
      as.numeric(values[observed] == level)
    }, numeric(length(observed)))

    mat
  })

  if (!length(encoded)) {
    return(matrix(numeric(0L), nrow = nrow(xdt), ncol = 0L))
  }

  do.call(cbind, encoded)
}

al_distance_standardize_scale <- function(raw_geom, center, scale, zero_var) {
  scaled <- sweep(raw_geom, 2L, center, "-")
  scaled <- sweep(scaled, 2L, scale, "/")
  scaled[is.na(scaled)] <- 0
  if (any(zero_var)) {
    scaled[, zero_var] <- 0
  }
  scaled
}

mlr_al_distances$add("standardize", ALDistanceStandardize)
