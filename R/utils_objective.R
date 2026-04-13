#' @title Validate Domain and Codomain Structure
#'
#' @description
#' Validates that a domain and codomain have valid structure for use with
#' Objective classes. Uses paradox's `assert_param_set` for type validation
#' and adds checks for non-empty domain and presence of targets.
#'
#' @param domain ([paradox::ParamSet])\cr
#'   Parameter set describing the input space.
#' @param codomain ([paradox::ParamSet])\cr
#'   Parameter set describing the output space.
#'
#' @return Named list with `domain_ids`, `codomain_ids`, and `codomain_target_ids`.
#'
#' @details
#' Uses `assert_param_set(domain, no_untyped = TRUE)` which rejects ParamUty.
#'
#' Requires bbotk >= 0.9.0, as codomains can have "learn" tags for active learning
#' in addition to "minimize"/"maximize".
#'
#' @keywords internal
assert_domain_codomain <- function(domain, codomain) {
  # Use paradox's assert_param_set with no_untyped=TRUE to reject ParamUty
  assert_param_set(domain, no_untyped = TRUE)
  assert_param_set(codomain)

  domain_ids <- domain$ids()
  codomain_ids <- codomain$ids()

  if (length(domain_ids) == 0L) {
    stop("Domain must contain at least one parameter")
  }

  if (length(codomain_ids) == 0L) {
    stop("Codomain must contain at least one parameter")
  }

  # Get target IDs - works with both ParamSet and Codomain
  target_ids <- codomain_target_ids(codomain)
  if (length(target_ids) == 0L) {
    stop("Codomain must contain at least one parameter tagged with 'minimize', 'maximize', or 'learn'")
  }

  list(
    domain_ids = domain_ids,
    codomain_ids = codomain_ids,
    codomain_target_ids = target_ids  # all target IDs (includes learn)
  )
}


#' @title Assert Pool Objective Search-Space Compatibility
#'
#' @description
#' Validates search-space features that pool-backed objectives cannot support.
#'
#' @param objective ([bbotk::Objective])\cr
#'   Objective to evaluate.
#' @param search_space ([paradox::ParamSet])\cr
#'   Search space used to propose points.
#'
#' @return The validated `search_space`, invisibly.
#'
#' @keywords internal
assert_pool_objective_search_space <- function(objective, search_space) {
  assert_r6(objective, "Objective")
  assert_param_set(search_space)

  if (
    "pool_restricted" %in% objective$properties &&
      search_space$has_trafo
  ) {
    stop("For pool-restricted objectives, the search space does not support trafos")
  }

  invisible(search_space)
}


#' @title Detect Objectives with Native data.table Evaluation Semantics
#'
#' @description
#' Returns whether an objective explicitly advertises that its `eval_dt()`
#' implementation preserves the missing-value semantics expected by
#' [SearchInstance].
#'
#' @details
#' This is an explicit S3 capability instead of a method-body heuristic.
#' Merely overriding `eval_dt()` is not sufficient: subclasses can wrap or
#' forward to [bbotk::Objective]'s default `eval_dt()`, which still converts
#' rows with `transpose_list()` and therefore treats dependency-inactive
#' parameters as literal `NA` values instead of dropping them. Native
#' data-table objectives opt in with an S3 method, and subclasses inherit that
#' capability automatically.
#'
#' @param objective ([bbotk::Objective])\cr
#'   Objective to inspect.
#'
#' @return `logical(1)`.
#'
#' @keywords internal
#' @export
objective_uses_dt_eval <- function(objective) {
  assert_r6(objective, "Objective")

  UseMethod("objective_uses_dt_eval")
}

#' @export
objective_uses_dt_eval.Objective <- function(objective) {
  FALSE
}

#' @export
objective_uses_dt_eval.ObjectiveLearner <- function(objective) {
  TRUE
}

#' @export
objective_uses_dt_eval.ObjectivePoolAbstract <- function(objective) {
  TRUE
}

#' @export
objective_uses_dt_eval.ObjectiveRFunDt <- function(objective) {
  TRUE
}


#' @keywords internal
test_param_type_compatible <- function(param_class, actual_type) {
  switch(param_class,
    ParamDbl = actual_type %in% c("numeric", "integer", "double"),
    ParamInt = actual_type %in% c("integer", "numeric", "double"),
    ParamFct = actual_type %in% c("factor", "character"),
    ParamLgl = actual_type == "logical",
    stopf("Unsupported parameter class: %s", param_class)
  )
}


#' @title Assert Parameter Type Compatibility
#'
#' @description
#' Validates that a parameter class is compatible with an actual data type.
#'
#' @param param_id (`character(1)`)\cr
#'   Parameter identifier for error messages.
#' @param param_class (`character(1)`)\cr
#'   ParamSet class (e.g., "ParamDbl", "ParamInt", "ParamFct", "ParamLgl").
#' @param actual_type (`character(1)`)\cr
#'   Actual data type (e.g., "numeric", "integer", "factor", "logical").
#' @param context (`character(1)`)\cr
#'   Context for error messages (e.g., "data column", "training feature").
#'
#' @return `TRUE` invisibly if compatible, otherwise throws an error.
#'
#' @keywords internal
assert_param_type_compatible <- function(param_id, param_class, actual_type, context = "data") {
  if (!test_param_type_compatible(param_class, actual_type)) {
    stopf("Domain parameter '%s' is %s but %s is '%s'",
      param_id, param_class, context, actual_type)
  }

  invisible(TRUE)
}


#' @title Get Column Type from data.table Column
#'
#' @description
#' Returns a normalized type string for a data.table column.
#'
#' @param col Column from a data.table.
#'
#' @return Character string: "numeric", "integer", "factor", "character", "logical", or the raw typeof.
#'
#' @keywords internal
get_col_type <- function(col) {
  if (is.factor(col)) return("factor")
  if (is.logical(col)) return("logical")
  if (is.integer(col)) return("integer")
  if (is.numeric(col)) return("numeric")
  if (is.character(col)) return("character")
  typeof(col)
}

#' @keywords internal
param_set_special_mask <- function(col, special_vals) {
  if (!length(special_vals)) {
    return(rep(FALSE, length(col)))
  }

  vapply(as.list(col), function(value) {
    has_element(special_vals, value)
  }, logical(1L))
}


#' @title Assert `ParamUty` Custom Checks for a data.table Column
#'
#' @description
#' Applies a `ParamUty` parameter's `custom_check` to each non-special entry of
#' a column and raises an informative error for the first failing row.
#'
#' @param col Column values to validate.
#' @param param_id (`character(1)`)\cr
#'   Parameter identifier used in error messages.
#' @param custom_check (`function`)\cr
#'   Custom validation function stored on the `ParamUty`.
#' @param special_vals (`list()`)\cr
#'   Special values that bypass the custom check.
#' @param .dt_name (`character(1)`)\cr
#'   Name of the `data.table` for error messages.
#'
#' @return `TRUE` invisibly if all checked values pass.
#'
#' @keywords internal
assert_param_uty_custom_check <- function(col, param_id, custom_check, special_vals, .dt_name) {
  assert_string(param_id)
  assert_function(custom_check)
  assert_string(.dt_name)

  is_special <- param_set_special_mask(col, special_vals)
  for (row_idx in which(!is_special)) {
    check_result <- custom_check(col[[row_idx]])
    if (isTRUE(check_result)) {
      next
    }

    if (!is.character(check_result) || length(check_result) != 1L || is.na(check_result)) {
      stopf(
        "Custom check for parameter '%s' must return TRUE or a single string, got %s",
        param_id,
        as_short_string(check_result)
      )
    }

    stopf(
      "%s column for parameter '%s' row %i: %s",
      .dt_name,
      param_id,
      row_idx,
      check_result
    )
  }

  invisible(TRUE)
}


#' @title Get `logical(nrow(dt))` mask of valid configurations in a data.table
#'
#' @description
#' Returns a `logical(nrow(dt))` indicating, for each row, whether the configuration is valid in the `ParamSet`.
#'
#' @param dt (`data.table`)\cr
#'   `data.table` to validate.
#' @param param_set ([paradox::ParamSet])\cr
#'   `ParamSet` describing the space to validate against.
#' @keywords internal
param_set_valid_mask_dt <- function(dt, param_set) {
  assert_data_table(dt)
  assert_param_set(param_set, no_untyped = TRUE)
  assert_names(names(dt), permutation.of = param_set$ids())

  keep <- rep(TRUE, nrow(dt))
  if (!nrow(dt)) {
    return(keep)
  }

  for (param_id in param_set$ids()) {
    col <- dt[[param_id]]
    param_class <- param_set$class[[param_id]]
    if (!test_param_type_compatible(param_class, get_col_type(col))) {
      return(rep(FALSE, nrow(dt)))
    }

    is_special <- param_set_special_mask(col, param_set$special_vals[[param_id]])
    is_valid <- is.na(col) | is_special
    is_valid <- is_valid | switch(param_class,
      ParamDbl = {
        lower <- param_set$lower[[param_id]]
        upper <- param_set$upper[[param_id]]
        col >= lower & col <= upper
      },
      ParamInt = {
        lower <- param_set$lower[[param_id]]
        upper <- param_set$upper[[param_id]]
        col == round(col) & col >= lower & col <= upper
      },
      ParamFct = as.character(col) %in% param_set$levels[[param_id]],
      ParamLgl = !is.na(col),
      stopf("Unsupported parameter class: %s", param_class)
    )
    is_valid[is.na(is_valid)] <- FALSE

    keep <- keep & is_valid
    if (!any(keep)) {
      return(keep)
    }
  }

  if (param_set$has_constraint && any(keep)) {
    keep_idx <- which(keep)
    keep[keep_idx] <- param_set$test_constraint_dt(
      dt[keep_idx, param_set$ids(), with = FALSE],
      assert_value = FALSE
    )
  }

  keep
}

#' @title Validate `data.table` Against `ParamSet`
#'
#' @description
#' Validates that a `data.table` is compatible with a given `ParamSet`.
#' Checks column presence, types, bounds, and uniqueness within the domain and
#' does type coercion if necessary.
#'
#' @param dt (`data.table`)\cr
#'   `data.table` to validate.
#' @param param_set ([paradox::ParamSet])\cr
#'   `ParamSet` describing the space to validate against.
#' @param require_uniqueness (`logical(1)`)\cr
#'   Whether to require uniqueness within the space. Default is `TRUE`.
#' @param min_rows (`integer(1)`)\cr
#'   Minimum number of rows required in `dt`. Default is `1L`.
#' @param presence (`character(1)`)\cr
#'   Whether all parameters must be present as columns in `dt`.
#'   Use `"all"` (default) to require all parameters, or `"subset"` to allow
#'   missing parameter columns and validate them as typed `NA` values.
#' @param allow_untyped (`logical(1)`)\cr
#'   Whether to allow `p_uty` in the `ParamSet`. Default is `FALSE`.
#'   `p_uty` columns are not checked for type compatibility. Their
#'   `custom_check` is applied when present.
#' @param allow_extra (`logical(1)`)\cr
#'   Whether columns outside the `ParamSet` are allowed. Default is `TRUE`.
#' @param .param_set_name (`character(1)`)\cr
#'   Name of the `ParamSet` for error messages. Default is `"param_set"`.
#' @param .dt_name (`character(1)`)\cr
#'   Name of the `data.table` for error messages. Default is `"dt"`.
#'
#' @return The validated and possibly coerced `data.table` (modified by reference).
#'
#' @details
#' Extra columns are allowed and are passed through unchanged when
#' `allow_extra = TRUE`.
#' Missing parameter columns are rejected unless `presence = "subset"`, in
#' which case they are validated as typed `NA` values on an internal copy.
#' When `require_uniqueness` is `TRUE`, tables with rows that are duplicated
#' within the space of the `ParamSet` are rejected, even if they differ by other columns.
#'
#' @keywords internal
assert_data_table_param_set <- function(dt, param_set, require_uniqueness = TRUE,
    min_rows = 1L, presence = "all", allow_untyped = FALSE, allow_extra = TRUE,
    .param_set_name = "param_set", .dt_name = "dt") {
  assert_count(min_rows)
  assert_choice(presence, c("all", "subset"))
  assert_flag(allow_extra)
  assert_data_table(dt, min.rows = min_rows)
  assert_param_set(param_set, no_untyped = !allow_untyped)

  param_ids <- param_set$ids()
  if (length(param_ids) == 0L) {
    stopf("%s must contain at least one parameter", .param_set_name)
  }

  extra_param_ids <- setdiff(names(dt), param_ids)
  if (!allow_extra && length(extra_param_ids)) {
    stopf(
      "%s contains columns not present in %s: %s",
      .dt_name,
      .param_set_name,
      str_collapse(extra_param_ids)
    )
  }

  present_param_ids <- intersect(param_ids, names(dt))
  missing_param_ids <- setdiff(param_ids, present_param_ids)
  if (presence == "all" && length(missing_param_ids)) {
    stopf("%s parameters not found in %s: %s", .param_set_name, .dt_name, str_collapse(missing_param_ids))
  }

  present_param_classes <- param_set$class[present_param_ids]
  present_uty_ids <- present_param_ids[present_param_classes == "ParamUty"]
  uty_custom_checks <- list()
  if (length(present_uty_ids)) {
    uty_param_rows <- param_set$params[match(present_uty_ids, param_set$params$id)]
    uty_custom_checks <- set_names(map(uty_param_rows$cargo, "custom_check"), uty_param_rows$id)
    uty_custom_checks <- discard(uty_custom_checks, is.null)
  }

  for (param_id in present_param_ids) {
    param_class <- param_set$class[[param_id]]
    if (param_class == "ParamUty") next
    col <- dt[[param_id]]
    col_type <- get_col_type(col)
    is_special <- param_set_special_mask(col, param_set$special_vals[[param_id]])

    set(dt, j = param_id, value = switch(param_class,
      ParamDbl = {
        lower <- param_set$lower[[param_id]]
        upper <- param_set$upper[[param_id]]
        assert_numeric(col[!is_special], lower = lower, upper = upper,
          .var.name = sprintf("%s column for parameter '%s'", .dt_name, param_id))
        as.numeric(col)
      },
      ParamInt = {
        lower <- param_set$lower[[param_id]]
        upper <- param_set$upper[[param_id]]
        assert_integerish(col[!is_special], tol = 0, lower = lower, upper = upper,
          .var.name = sprintf("%s column for parameter '%s'", .dt_name, param_id))
        as.integer(col)
      },
      ParamFct = {
        assert_param_type_compatible(param_id, param_class, col_type, sprintf("%s column", .dt_name))
        assert_subset(
          as.character(col)[!is_special],
          c(param_set$levels[[param_id]], NA_character_),
          .var.name = sprintf("%s column for parameter '%s'", .dt_name, param_id)
        )
        as.character(col)
      },
      ParamLgl = {
        assert_param_type_compatible(param_id, param_class, col_type, sprintf("%s column", .dt_name))
        col
      },
      stopf("Unsupported parameter class: %s", param_class)
    ))
  }

  if (length(uty_custom_checks)) {
    iwalk(uty_custom_checks, function(custom_check, param_id) {
      assert_param_uty_custom_check(
        col = dt[[param_id]],
        param_id = param_id,
        custom_check = custom_check,
        special_vals = param_set$special_vals[[param_id]],
        .dt_name = .dt_name
      )
    })
  }

  param_dt <- dt[, present_param_ids, with = FALSE]
  if (presence == "subset") {
    param_dt <- copy(param_dt)
    fill_param_set_missing_dt(param_dt, param_set)
  }

  assert_param_set_dependencies_dt(param_dt, param_set, .dt_name = .dt_name)
  if (param_set$has_constraint) {
    # Constraints still use paradox's rowwise checker; dependency handling has
    # already been verified above so only constrained ParamSets pay that cost.
    param_set$assert_dt(param_dt)
  }

  if (require_uniqueness) {
    duplicate_idx <- which(duplicated(param_dt))
    if (length(duplicate_idx) > 0L) {
      first_duplicate <- duplicate_idx[[1L]]
      duplicate_row <- param_dt[first_duplicate]
      duplicate_str <- as_short_string(as.list(duplicate_row))
      stopf("%s contains duplicate configurations: {%s}", .dt_name, duplicate_str)
    }
  }

  invisible(dt)
}
