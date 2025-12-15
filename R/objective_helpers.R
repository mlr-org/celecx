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
#' Note that paradox's `$check_dt()` cannot be used for construction-time validation
#' because we accept data formats that paradox doesn't (e.g., factor columns for
#' ParamFct, which paradox expects as character). Our validation handles the
#' conversion/coercion that prepares data for paradox-compatible queries.
#'
#' As of bbotk >= 0.9.0, codomains can have "learn" tags in addition to
#' "minimize"/"maximize", so we check for any target tags.
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
  compatible <- switch(param_class,
    ParamDbl = actual_type %in% c("numeric", "integer", "double"),
    ParamInt = actual_type %in% c("integer", "numeric", "double"),
    ParamFct = actual_type %in% c("factor", "character"),
    ParamLgl = actual_type == "logical",
    stopf("Unsupported parameter class: %s", param_class)
  )

  if (!compatible) {
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
