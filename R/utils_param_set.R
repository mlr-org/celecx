# Internal helpers for applying and validating ParamSet semantics directly on
# data.tables without first converting rows to row-lists.
param_set_storage_na <- function(storage_type) {
  switch(storage_type,
    integer = NA_integer_,
    double = NA_real_,
    numeric = NA_real_,
    character = NA_character_,
    logical = NA,
    list = list(NA),  # yes, this is what the `Design` class understands as missingness for p_uty() columns
    stopf("Unsupported storage type '%s'", storage_type)
  )
}


#' @keywords internal
param_set_empty_dt <- function(param_set) {
  assert_param_set(param_set)

  as.data.table(imap(param_set$class, function(param_class, id) {
    switch(param_class,
      ParamDbl = numeric(0),
      ParamInt = integer(0),
      ParamFct = character(0),
      ParamLgl = logical(0),
      ParamUty = vector("list", 0L),
      stopf("Unsupported parameter class '%s' for '%s'", param_class, id)
    )
  }))
}


#' @title Topologically sort parameter IDs by dependency order
#'
#' @description
#' "Parent" parameters that other parameters depend on are sorted first.
#'
#' @param param_set ([paradox::ParamSet])\cr
#'   Parameter set to sort.
#'
#' @return `character()` of parameter IDs in topological order.
#'
#' @keywords internal
param_set_topo_ids <- function(param_set) {
  parents <- NULL
  assert_param_set(param_set)

  if (!param_set$has_deps) {
    return(param_set$ids())
  }

  graph <- param_set$deps[, c("id", "on"), with = FALSE]
  colnames(graph) <- c("id", "parents")
  fillin <- data.table(id = param_set$ids(), parents = list(character(0L)))
  graph <- rbind(graph, fillin[fillin$id %nin% graph$id, ])
  graph <- graph[, list(parents = list(unlist(parents, use.names = FALSE))), by = "id"]
  topo_sort(graph)$id
}


#' @keywords internal
fill_param_set_missing_dt <- function(xdt, param_set) {
  assert_data_table(xdt)
  assert_param_set(param_set)

  missing_ids <- setdiff(param_set$ids(), names(xdt))
  if (!length(missing_ids)) {
    setcolorder(xdt, param_set$ids())
    return(invisible(xdt))
  }

  for (id in missing_ids) {
    set(xdt, j = id, value = param_set_storage_na(param_set$storage_type[[id]]))
  }
  setcolorder(xdt, param_set$ids())

  invisible(xdt)
}

# Appy a search space's fixed values and dependency-inactive values to a data.table
apply_param_set_design_dt <- function(xdt, param_set) {
  id <- NULL
  assert_data_table(xdt)
  assert_param_set(param_set)

  # Mirror the two Design$new() side effects that matter before evaluation:
  # fixed values overwrite columns first, then dependency-inactive values are
  # replaced by typed NA.
  if (length(param_set$values)) {
    iwalk(param_set$values, function(value, id) {
      if (id %in% names(xdt)) {
        set(xdt, j = id, value = value)
      }
    })
  }

  if (!nrow(xdt) || !param_set$has_deps) {
    return(invisible(xdt))
  }

  deps <- param_set$deps
  for (param_id in param_set_topo_ids(param_set)) {
    if (param_id %nin% names(xdt)) {
      next
    }

    dep_rows <- deps[id == param_id]
    if (!nrow(dep_rows)) {
      next
    }

    is_active <- rep(TRUE, nrow(xdt))
    for (j in seq_row(dep_rows)) {
      is_active <- is_active &
        paradox_condition_test(dep_rows$cond[[j]], xdt[[dep_rows$on[[j]]]])
    }

    inactive_idx <- which(!is_active)
    if (length(inactive_idx)) {
      set(
        xdt,
        i = inactive_idx,
        j = param_id,
        value = param_set_storage_na(param_set$storage_type[[param_id]])
      )
    }
  }

  invisible(xdt)
}


#' @keywords internal
assert_param_set_dependencies_dt <- function(xdt, param_set, .dt_name = "xdt") {
  id <- NULL
  assert_data_table(xdt)
  assert_param_set(param_set)
  assert_string(.dt_name)

  if (!nrow(xdt) || !param_set$has_deps) {
    return(invisible(xdt))
  }

  deps <- param_set$deps
  for (param_id in param_set_topo_ids(param_set)) {
    if (param_id %nin% names(xdt)) {
      next
    }

    value_idx <- which(!is.na(xdt[[param_id]]))
    if (!length(value_idx)) {
      next
    }

    dep_rows <- deps[id == param_id]
    if (!nrow(dep_rows)) {
      next
    }

    for (j in seq_row(dep_rows)) {
      parent_id <- dep_rows$on[[j]]
      if (parent_id %nin% names(xdt)) {
        stopf(
          "%s row %i sets parameter '%s' although dependency '%s' cannot be checked because parent '%s' is missing",
          .dt_name,
          value_idx[[1L]],
          param_id,
          condition_as_string(dep_rows$cond[[j]], parent_id),
          parent_id
        )
      }

      parent_col <- xdt[[parent_id]]
      bad_idx <- value_idx[
        is.na(parent_col[value_idx]) |
          !paradox_condition_test(dep_rows$cond[[j]], parent_col[value_idx])
      ]
      if (!length(bad_idx)) {
        next
      }

      first_bad <- bad_idx[[1L]]
      parent_value <- parent_col[[first_bad]]
      if (is.na(parent_value)) {
        stopf(
          "%s row %i sets parameter '%s' although dependency '%s' is not satisfied because '%s' is NA",
          .dt_name,
          first_bad,
          param_id,
          condition_as_string(dep_rows$cond[[j]], parent_id),
          parent_id
        )
      }

      stopf(
        "%s row %i sets parameter '%s' although dependency '%s' is not satisfied (current %s = %s)",
        .dt_name,
        first_bad,
        param_id,
        condition_as_string(dep_rows$cond[[j]], parent_id),
        parent_id,
        as_short_string(parent_value)
      )
    }
  }

  invisible(xdt)
}
