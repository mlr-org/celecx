#' @title Generate a Dependency-Aware Grid Design
#'
#' @description
#' Generate the same configurations as [paradox::generate_design_grid()],
#' while avoiding avoidable Cartesian blow-up from duplicated per-parameter
#' grid values and from parameters that are inactive because of dependencies.
#'
#' @param param_set ([`ParamSet`]).
#' @param resolution (`integer(1)`)\cr
#'   Global resolution for all numerical parameters.
#' @param param_resolutions (named `integer()`)\cr
#'   Resolution per [`Domain`], named by parameter ID.
#' @param upper_limit (`integer(1)` | `NULL`)\cr
#'   Optional upper bound on the number of rows to materialize. An error is
#'   raised before building the full grid if the design would exceed this limit.
#'
#' @return [`Design`].
#' @export
generate_design_grid_celecx <- function(param_set,
    resolution = NULL,
    param_resolutions = NULL,
    upper_limit = NULL) {
  # Match paradox's validation and generated configurations; only row order may differ.
  assert_param_set(param_set, no_untyped = TRUE)
  assert_integerish(upper_limit, lower = 0, any.missing = FALSE, len = 1L, null.ok = TRUE)
  upper_limit <- upper_limit %??% Inf

  if (param_set$is_empty) {
    # Preserve paradox's corner-case behavior for empty ParamSets instead of reimplementing it here.
    return(paradox::generate_design_grid(
      param_set,
      resolution = resolution,
      param_resolutions = param_resolutions
    ))
  }

  param_resolution <- celecx_grid_resolutions(
    param_set = param_set,
    resolution = resolution,
    param_resolutions = param_resolutions
  )
  param_grid_values <- celecx_grid_param_values(
    param_set = param_set,
    param_resolution = param_resolution
  )

  data <- if (!param_set$has_deps) {
    predicted_size <- prod(lengths(param_grid_values))
    if (predicted_size > upper_limit) {
      stopf(
        "Grid would contain %s rows, exceeding upper_limit %s",
        format(predicted_size, scientific = FALSE, trim = TRUE),
        format(upper_limit, scientific = FALSE, trim = TRUE)
      )
    }

    cross_join(
      param_grid_values,
      sorted = FALSE
    )
  } else {
    celecx_grid_traverse_dependency_aware(
      param_set = param_set,
      param_grid_values = param_grid_values,
      mode = "grid",
      upper_limit = upper_limit
    )
  }

  Design$new(param_set, data, remove_dupl = FALSE)
}


predict_grid_size <- function(search_space, resolution = NULL, param_resolutions = NULL, upper_limit = NULL) {
  # Predict the exact number of rows produced by generate_design_grid(_celecx()).
  assert_param_set(search_space, no_untyped = TRUE)
  assert_integerish(upper_limit, lower = 0, any.missing = FALSE, len = 1L, null.ok = TRUE)
  upper_limit <- upper_limit %??% Inf
  param_resolution <- celecx_grid_resolutions(search_space, resolution = resolution, param_resolutions = param_resolutions)
  param_grid_values <- celecx_grid_param_values(search_space, param_resolution)

  if (!search_space$has_deps) {
    return(min(prod(lengths(param_grid_values)), upper_limit))
  }

  celecx_grid_traverse_dependency_aware(
    param_set = search_space,
    param_grid_values = param_grid_values,
    mode = "count",
    upper_limit = upper_limit
  )
}


celecx_grid_resolutions <- function(param_set, resolution = NULL, param_resolutions = NULL) {
  # Reconstruct paradox's resolution bookkeeping because it also determines output column order.
  ids <- param_set$ids()
  ids_num <- ids[param_set$is_number]

  param_resolution <- integer(0L)
  if (length(ids_num) > 0L) {
    if (is.null(resolution) && is.null(param_resolutions)) {
      stop("You must specify 'resolution' or 'param_resolutions'!")
    }

    if (!is.null(resolution)) {
      resolution <- assert_count(resolution, coerce = TRUE)
      param_resolution <- set_names(rep.int(resolution, param_set$length), ids)
    }

    if (!is.null(param_resolutions)) {
      assert_integerish(
        param_resolutions,
        lower = 1L,
        any.missing = FALSE,
      )
      assert_names(names(param_resolutions), subset.of = ids_num)
      param_resolution <- insert_named(param_resolution, param_resolutions)
    }

    ids_missing <- setdiff(ids_num, names(param_resolution))
    if (length(ids_missing) > 0L) {
      stopf(
        "Resolution settings missing for some numerical params: %s",
        str_collapse(ids_missing)
      )
    }
  }

  # Append categorical params exactly like paradox::generate_design_grid() does via insert_named().
  insert_named(param_resolution, param_set$nlevels[param_set$is_categ])
}


celecx_grid_param_values <- function(param_set, param_resolution) {
  # Precompute the realized grid values for each parameter before any joining happens.
  fixed_values <- param_set$values

  imap(param_resolution, function(current_resolution, id) {
    if (id %in% names(fixed_values)) {
      return(fixed_values[[id]])
    }

    # Use the actual qunif mapping because rounding, logscale, and trafos can collapse grid points.
    unit_grid <- seq(0, 1, length.out = current_resolution)
    unique(param_set$qunif(set_names(data.table(value = unit_grid), id))[[1L]])
  })
}


celecx_grid_traverse_dependency_aware <- function(param_set,
    param_grid_values,
    mode,
    upper_limit = Inf) {
  # Shared traversal for materialized grids and capped exact-size prediction.
  deps <- param_set$deps
  topo_ids <- param_set_topo_ids(param_set)
  assert_choice(mode, c("grid", "count"))
  count_mode <- mode == "count"
  future_parent_ids <- if (count_mode) {
    celecx_predict_grid_size_future_parent_ids(topo_ids, deps)
  } else {
    NULL
  }

  # Start from one empty partial configuration and grow it one parameter at a time.
  state <- data.table(.count = 1L)

  for (id in topo_ids) {
    is_active <- celecx_grid_active_mask(data = state, deps = deps, id = id)
    if (is.finite(upper_limit)) {
      # Later steps cannot shrink the number of represented configurations, because inactive params
      # still contribute one typed-NA branch. If this step already exceeds the cap, the final result must.
      next_size <- sum(state$.count[!is_active]) + sum(state$.count[is_active]) * length(param_grid_values[[id]])
      if (next_size > upper_limit) {
        if (count_mode) {
          return(upper_limit)
        }

        stopf(
          "Grid would contain at least %s rows, exceeding upper_limit %s",
          format(next_size, scientific = FALSE, trim = TRUE),
          format(upper_limit, scientific = FALSE, trim = TRUE)
        )
      }
    }

    keep_id <- !count_mode || id %in% future_parent_ids[[id]]
    active_state <- celecx_grid_expand_active_rows(
      data = state[is_active],
      id = id,
      values = param_grid_values[[id]],
      keep_id = keep_id
    )
    inactive_state <- celecx_grid_mark_inactive_rows(
      data = state[!is_active],
      id = id,
      storage_type = param_set$storage_type[[id]],
      keep_id = keep_id
    )

    state <- rbind(active_state, inactive_state, use.names = TRUE)
    if (count_mode) {
      keep_ids <- intersect(future_parent_ids[[id]], setdiff(names(state), ".count"))
      state <- celecx_predict_grid_size_reduce_states(
        state = state,
        keep_ids = keep_ids
      )
    }
  }

  if (count_mode) {
    return(min(sum(state$.count), upper_limit))
  }

  state[, .count := NULL]
  # Column order follows the constructed resolution vector in paradox, not always param_set$ids().
  setcolorder(state, names(param_grid_values))
}


celecx_grid_active_mask <- function(data, deps, id) {
  # Determine which partial configurations satisfy all dependency conditions for the current parameter.
  dep_rows <- deps[id, on = "id", nomatch = 0]
  is_active <- rep(TRUE, nrow(data))
  if (nrow(dep_rows) == 0L) {
    return(is_active)
  }

  for (j in seq_row(dep_rows)) {
    parent_values <- data[[dep_rows$on[[j]]]]
    # `condition_test()` also rejects NA parents, which mirrors paradox's recursive dependency pruning.
    is_active <- is_active &
      paradox_condition_test(dep_rows$cond[[j]], parent_values)
  }

  is_active
}


celecx_grid_expand_active_rows <- function(data, id, values, keep_id = TRUE) {
  # Active branches either materialize one row per value or update multiplicities in `.count`.
  if (!nrow(data) || !length(values)) {
    data <- data[0L]
    if (keep_id) {
      # Create empty typed columns so downstream rbind()/Design validation sees the right schema.
      data[, (id) := values[0L]]
    }
    return(data)
  }

  if (!keep_id) {
    return(data[, .count := .count * length(values)])
  }

  n_rows <- nrow(data)
  data <- data[rep(seq_len(n_rows), each = length(values))]
  data[, (id) := rep(values, times = n_rows)]
}


celecx_grid_mark_inactive_rows <- function(data, id, storage_type, keep_id = TRUE) {
  # Inactive branches contribute one typed-NA row unless the value is irrelevant for future dependency checks.
  if (!keep_id) {
    return(data)
  }

  na_value <- param_set_storage_na(storage_type)
  data[, (id) := na_value]
}

celecx_predict_grid_size_future_parent_ids <- function(topo_ids, deps) {
  # Precompute which processed parent columns still matter after each topological step.
  future_parent_ids <- named_list(topo_ids)
  needed_ids <- character(0L)

  for (i in rev(seq_along(topo_ids))) {
    id <- topo_ids[[i]]
    future_parent_ids[[id]] <- needed_ids
    needed_ids <- union(deps[id, on, on = "id", nomatch = 0], needed_ids)
  }

  future_parent_ids
}


celecx_predict_grid_size_reduce_states <- function(state, keep_ids) {
  # Once a parent column is irrelevant for future dependencies, collapse it into the accumulated count.
  state <- state[, c(keep_ids, ".count"), with = FALSE]
  if (!nrow(state)) {
    return(state)
  }

  if (!length(keep_ids)) {
    return(data.table(.count = sum(state$.count)))
  }

  state[, .(.count = sum(.count)), by = keep_ids]
}
