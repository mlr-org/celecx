#' @title Gower Active Learning Distance
#'
#' @include mlr_al_distances.R
#' @include ALDistance.R
#'
#' @name mlr_al_distances_gower
#'
#' @description
#' Distance for mixed-type active-learning search spaces.
#'
#' Numeric and integer columns contribute absolute differences, optionally
#' rescaled according to the `scale` hyperparameter. Logical and factor columns
#' contribute `0` for equal values and `1` for different values. The final
#' distance is the average across all observed per-column contributions.
#'
#' Numeric columns with zero scale are ignored. Missing values are excluded from
#' the pairwise average for the affected column.
#'
#' @format [R6::R6Class] object inheriting from [ALDistance].
#'
#' @section Construction:
#' ```
#' clx_ald("gower")
#' ```
#'
#' The configuration hyperparameter `scale` controls numeric scaling:
#' * `"minmax_auto"` divides by the observed pool range when a pool is supplied,
#'   and by the finite search-space range when fitted with `xdt = NULL`.
#' * `"minmax_space"` divides by the search-space range.
#' * `"minmax_empirical"` divides by the observed pool range.
#' * `"standardize"` divides by the observed pool standard deviation.
#' * `"off"` leaves numeric differences unscaled.
#'
#' @export
#' @family ALDistance
ALDistanceGower <- R6Class("ALDistanceGower",
  inherit = ALDistance,

  public = list(

    #' @description
    #' Creates a new Gower active-learning distance.
    initialize = function() {
      super$initialize(
        id = "gower",
        param_set = ps(
          scale = p_fct(
            levels = c("minmax_auto", "minmax_space", "minmax_empirical", "standardize", "off"),
            init = "minmax_auto",
            tags = "required"
          )
        ),
        label = "Gower AL Distance",
        man = "celecx::mlr_al_distances_gower"
      )
    }
  ),

  private = list(
    .fit_pool = function(xdt, search_space) {
      scale_mode <- self$param_set$get_values()$scale
      specs <- al_distance_gower_specs(
        search_space = search_space,
        xdt = xdt,
        scale_mode = scale_mode
      )
      if (!is.null(xdt)) {
        al_distance_gower_scale_numeric_columns(xdt, specs)
      }

      list(
        xdt_pool = xdt,
        specs = specs
      )
    },

    .distances = function(xdt, state, i = NULL) {
      al_distance_gower_scale_numeric_columns(xdt, state$specs)

      xdt_reference <- state$xdt_reference
      if (!is.null(i)) {
        xdt_reference <- xdt_reference[i]
      }

      al_distance_gower_pairwise(
        xdt_query = xdt,
        xdt_pool = xdt_reference,
        specs = state$specs
      )
    },

    .set_reference_points = function(xdt, state) {
      al_distance_gower_scale_numeric_columns(xdt, state$specs)
      state$xdt_reference <- xdt
      state
    }
  )
)

al_distance_gower_search_space_distances <- function(xdt_query, xdt_reference = NULL, search_space) {
  if (is.null(xdt_reference)) {
    xdt_reference <- xdt_query
  }

  distance <- ALDistanceGower$new()
  distance$param_set$set_values(scale = "minmax_space")
  distance$fit_pool(NULL, search_space)
  distance$set_reference_points(xdt_reference)
  distance$distances(xdt_query)
}

al_distance_gower_specs <- function(search_space, xdt, scale_mode) {
  ids <- search_space$ids()

  specs <- lapply(ids, function(id) {
    param_class <- search_space$class[[id]]
    has_pool <- !is.null(xdt)
    col <- if (has_pool) xdt[[id]] else NULL

    switch(param_class,
      ParamDbl =,
      ParamInt = {
        values <- if (has_pool) {
          al_distance_gower_numeric_values(col, id = id)
        }

        list(
          id = id,
          kind = "numeric",
          scale = al_distance_gower_numeric_scale(
            values = values,
            search_space = search_space,
            id = id,
            scale_mode = scale_mode,
            has_pool = has_pool
          )
        )
      },
      ParamLgl = {
        if (has_pool) {
          al_distance_gower_categorical_values(
            col,
            id = id,
            levels = c("FALSE", "TRUE")
          )
        }

        list(
          id = id,
          kind = "categorical",
          levels = c("FALSE", "TRUE")
        )
      },
      ParamFct = {
        levels <- search_space$levels[[id]]
        if (has_pool) {
          al_distance_gower_categorical_values(
            col,
            id = id,
            levels = levels
          )
        }

        list(
          id = id,
          kind = "categorical",
          levels = levels
        )
      },
      stopf(
        "ALDistanceGower does not support search-space column '%s' of class '%s'",
        id,
        param_class
      )
    )
  })

  set_names(specs, ids)
}

al_distance_gower_scale_numeric_columns <- function(xdt, specs) {
  assert_data_table(xdt)
  assert_list(specs, names = "strict")

  for (spec in specs) {
    if (!identical(spec$kind, "numeric")) {
      next
    }

    values <- al_distance_gower_numeric_values(xdt[[spec$id]], id = spec$id)
    if (spec$scale > 0) {
      values <- values / spec$scale
    }

    set(xdt, j = spec$id, value = values)
  }

  invisible(xdt)
}

al_distance_gower_numeric_values <- function(col, id) {
  if (!(is.numeric(col) || is.integer(col))) {
    stopf(
      "ALDistanceGower requires numeric or integer data for '%s', but got '%s'",
      id,
      typeof(col)
    )
  }

  as.numeric(col)
}

al_distance_gower_categorical_values <- function(col, id, levels) {
  values <- as.character(col)

  assert_subset(values[!is.na(values)], levels)
  values
}

al_distance_gower_numeric_scale <- function(values, search_space, id, scale_mode, has_pool) {
  observed_values <- if (has_pool) values[!is.na(values)] else numeric(0)

  scale <- switch(scale_mode,
    minmax_auto = {
      if (has_pool) {
        if (!length(observed_values)) {
          return(0)
        }
        diff(range(observed_values))
      } else {
        al_distance_gower_search_space_range(search_space, id, scale_mode)
      }
    },
    minmax_space = {
      al_distance_gower_search_space_range(search_space, id, scale_mode)
    },
    minmax_empirical = {
      if (!has_pool) {
        stopf(
          "ALDistanceGower with scale = 'minmax_empirical' requires a finite pool"
        )
      }
      if (!length(observed_values)) {
        return(0)
      }
      diff(range(observed_values))
    },
    standardize = {
      if (!has_pool) {
        stopf(
          "ALDistanceGower with scale = 'standardize' requires a finite pool"
        )
      }
      if (length(observed_values) <= 1L) 0 else stats::sd(observed_values)
    },
    off = 1,
    stopf("Unknown Gower scaling mode '%s'", scale_mode)
  )

  if (!is.finite(scale) || scale <= 0) {
    return(0)
  }

  scale
}

al_distance_gower_search_space_range <- function(search_space, id, scale_mode) {
  result <- search_space$upper[[id]] - search_space$lower[[id]]
  if (!is.finite(result)) {
    stopf(
      "ALDistanceGower with scale = '%s' requires finite bounds for '%s'",
      scale_mode,
      id
    )
  }
  result
}

al_distance_gower_pairwise <- function(xdt_query, xdt_pool, specs) {
  assert_data_table(xdt_query)
  assert_data_table(xdt_pool)
  assert_list(specs, names = "strict")

  n_query <- nrow(xdt_query)
  n_pool <- nrow(xdt_pool)

  distance_sum <- matrix(0, nrow = n_query, ncol = n_pool)
  weight_sum <- matrix(0, nrow = n_query, ncol = n_pool)

  for (spec in specs) {
    if (identical(spec$kind, "numeric")) {
      query_values <- al_distance_gower_numeric_values(xdt_query[[spec$id]], id = spec$id)
      pool_values <- al_distance_gower_numeric_values(xdt_pool[[spec$id]], id = spec$id)

      if (spec$scale <= 0) {
        next
      }

      observed <- outer(!is.na(query_values), !is.na(pool_values), "&")
      if (!any(observed)) {
        next
      }

      column_distance <- abs(outer(query_values, pool_values, "-"))
      column_distance[!observed] <- 0

      distance_sum <- distance_sum + column_distance
      weight_sum <- weight_sum + observed
      next
    }

    query_values <- al_distance_gower_categorical_values(
      xdt_query[[spec$id]],
      id = spec$id,
      levels = spec$levels
    )
    pool_values <- al_distance_gower_categorical_values(
      xdt_pool[[spec$id]],
      id = spec$id,
      levels = spec$levels
    )

    observed <- outer(!is.na(query_values), !is.na(pool_values), "&")
    if (!any(observed)) {
      next
    }

    column_distance <- outer(query_values, pool_values, "!=")
    column_distance[!observed] <- FALSE

    distance_sum <- distance_sum + column_distance
    weight_sum <- weight_sum + observed
  }

  distances <- matrix(0, nrow = n_query, ncol = n_pool)
  observed_pairs <- weight_sum > 0
  distances[observed_pairs] <- distance_sum[observed_pairs] / weight_sum[observed_pairs]
  distances
}

mlr_al_distances$add("gower", ALDistanceGower)
