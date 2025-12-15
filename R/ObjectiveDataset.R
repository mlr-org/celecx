#' @title Validate Dataset Against Domain and Codomain
#'
#' @description
#' Validates that a dataset is compatible with a given domain and codomain.
#' Checks column presence, types, bounds, and factor levels.
#'
#' @param dt (`data.table`)\cr
#'   The dataset to validate.
#' @param domain ([paradox::ParamSet])\cr
#'   Parameter set describing the input space.
#' @param codomain ([paradox::ParamSet])\cr
#'   Parameter set describing the output space.
#'
#' @return The validated and possibly coerced `data.table` (modified by reference).
#'
#' @details
#' Type expectations:
#' * `ParamDbl` -> numeric column
#' * `ParamInt` -> integerish column (coerced to integer)
#' * `ParamFct` -> factor or character column (character converted to factor with domain levels)
#' * `ParamLgl` -> logical column
#'
#' @keywords internal
assert_dataset_domain_codomain <- function(dt, domain, codomain) {
  assert_data_table(dt, min.rows = 1L)

  # Basic domain/codomain structure validation
  ids <- assert_domain_codomain(domain, codomain)
  domain_ids <- ids$domain_ids
  codomain_ids <- ids$codomain_ids
  codomain_target_ids <- ids$codomain_target_ids

  # Validate that all codomain columns are present
  missing_codomain <- setdiff(codomain_ids, names(dt))
  if (length(missing_codomain)) {
    stopf("Codomain columns not found in dataset: %s", str_collapse(missing_codomain))
  }

  # Validate that all domain columns are present (excluding codomain)
  feature_names <- setdiff(names(dt), codomain_ids)
  missing_domain <- setdiff(domain_ids, feature_names)
  if (length(missing_domain)) {
    stopf("Domain parameters not found in dataset features: %s", str_collapse(missing_domain))
  }

  # Check that domain and dataset features match exactly
  extra_features <- setdiff(feature_names, domain_ids)
  if (length(extra_features)) {
    stopf("Dataset contains feature columns not in domain: %s", str_collapse(extra_features))
  }

  # Process each domain column: validate types and coerce values
  for (param_id in domain_ids) {
    param_class <- domain$class[[param_id]]
    col <- dt[[param_id]]
    col_type <- get_col_type(col)

    set(dt, j = param_id, value = switch(param_class,
      ParamDbl = {
        assert_param_type_compatible(param_id, param_class, col_type, "data column")
        lower <- domain$lower[[param_id]]
        upper <- domain$upper[[param_id]]
        if (any(col < lower, na.rm = TRUE) || any(col > upper, na.rm = TRUE)) {
          stopf("Data values for '%s' are outside domain bounds [%s, %s]", param_id, lower, upper)
        }
        as.numeric(col)
      },
      ParamInt = {
        assert_param_type_compatible(param_id, param_class, col_type, "data column")
        if (!test_integerish(col, tol = sqrt(.Machine$double.eps))) {
          stopf("Data values for '%s' are not integerish", param_id)
        }
        lower <- domain$lower[[param_id]]
        upper <- domain$upper[[param_id]]
        if (any(col < lower, na.rm = TRUE) || any(col > upper, na.rm = TRUE)) {
          stopf("Data values for '%s' are outside domain bounds [%s, %s]", param_id, lower, upper)
        }
        as.integer(col)
      },
      ParamFct = {
        # ParamFct accepts factor or character
        assert_param_type_compatible(param_id, param_class, col_type, "data column")
        domain_levels <- domain$levels[[param_id]]
        if (is.character(col)) {
          unique_vals <- unique(col[!is.na(col)])
          extra_vals <- setdiff(unique_vals, domain_levels)
          if (length(extra_vals)) {
            stopf("Data column '%s' has values not in domain levels: %s (domain levels: %s)",
              param_id, str_collapse(extra_vals), str_collapse(domain_levels))
          }
          factor(col, levels = domain_levels)
        } else {
          # is.factor(col)
          data_levels <- levels(col)
          extra_levels <- setdiff(data_levels, domain_levels)
          if (length(extra_levels)) {
            stopf("Data factor '%s' has levels not in domain: %s (domain levels: %s)",
              param_id, str_collapse(extra_levels), str_collapse(domain_levels))
          }
          # Expand factor levels to match domain
          factor(as.character(col), levels = domain_levels)
        }
      },
      ParamLgl = {
        assert_param_type_compatible(param_id, param_class, col_type, "data column")
        col
      },
      stopf("Unsupported parameter class: %s", param_class)
    ))
  }

  # Validate codomain columns (targets must be numeric)
  for (codomain_id in codomain_ids) {
    col <- dt[[codomain_id]]
    if (codomain_id %in% codomain_target_ids) {
      if (!is.numeric(col)) {
        stopf("Codomain target '%s' must be numeric, but is %s", codomain_id, typeof(col))
      }
      if (codomain$class[[codomain_id]] %in% c("ParamDbl", "ParamInt")) {
        lower <- codomain$lower[[codomain_id]]
        upper <- codomain$upper[[codomain_id]]
        if (is.finite(lower) && any(col < lower, na.rm = TRUE)) {
          stopf("Data values for codomain '%s' are below lower bound %s", codomain_id, lower)
        }
        if (is.finite(upper) && any(col > upper, na.rm = TRUE)) {
          stopf("Data values for codomain '%s' are above upper bound %s", codomain_id, upper)
        }
      }
    }
  }

  invisible(dt)
}


#' @title Objective Function Based on Pre-evaluated Dataset
#'
#' @description
#' An [Objective] subclass where evaluation happens by table lookup in a pre-evaluated dataset.
#' This is useful for "replaying" optimization on historical data or for testing optimization
#' algorithms on known datasets.
#'
#' @details
#' The dataset must contain columns for all parameters in the domain and all targets in the codomain.
#' Evaluation fails with an informative error if a requested configuration is not present in the dataset.
#'
#' Column type expectations:
#' * `ParamDbl` corresponds to numeric columns
#' * `ParamInt` corresponds to integerish columns (coerced to integer)
#' * `ParamFct` corresponds to factor or character columns (character converted to factor)
#' * `ParamLgl` corresponds to logical columns
#'
#' Evaluation uses exact matching (no floating point tolerance). Values are expected to be
#' sampled directly from the dataset without arithmetic modifications.
#'
#' @export
ObjectiveDataset <- R6Class("ObjectiveDataset",
  inherit = Objective,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param dataset (`TaskRegr` | `data.frame` | `data.table`)\cr
    #'   Dataset containing pre-evaluated configurations. Must contain columns matching
    #'   all domain parameters and codomain targets.
    #' @param domain ([paradox::ParamSet])\cr
    #'   Parameter set describing the input space. All parameter IDs must correspond to
    #'   columns in the dataset (excluding codomain columns).
    #' @param codomain ([paradox::ParamSet])\cr
    #'   Parameter set describing the output space. Must contain at least one target tagged
    #'   with `"minimize"`, `"maximize"`, or `"learn"`. Target IDs must correspond to columns in the dataset.
    #' @param id (`character(1)`)\cr
    #'   Identifier for the objective.
    #' @param check_values (`logical(1)`)\cr
    #'   Whether to check validity of input configurations against the domain.
    initialize = function(dataset, domain, codomain, id = "dataset", check_values = TRUE) {
      assert(
        check_data_frame(dataset, min.rows = 1L, min.cols = 2L),
        check_r6(dataset, "TaskRegr")
      )
      assert_param_set(domain)
      assert_param_set(codomain)
      assert_string(id)
      assert_flag(check_values)

      # Extract data.table from TaskRegr or convert data.frame/data.table
      if (inherits(dataset, "TaskRegr")) {
        dt <- copy(dataset$data())
      } else {
        dt <- as.data.table(dataset)
      }

      # Validate and process the dataset
      assert_dataset_domain_codomain(dt, domain, codomain)

      # Store the processed data - use first optimization target for TaskRegr construction
      private$.data <- dt
      private$.target_name <- codomain_optimize_ids(codomain)[[1L]]

      # Call parent constructor
      super$initialize(
        id = id,
        domain = domain,
        codomain = codomain,
        properties = "deterministic",
        constants = ps(),
        check_values = check_values,
        label = "Objective from Pre-evaluated Dataset",
        man = NA_character_
      )
    },

    #' @description
    #' Evaluates multiple input values on the objective function.
    #'
    #' @param xdt (`data.table`)\cr
    #'   A data.table with one configuration per row.
    #'
    #' @return [data.table::data.table()] containing codomain columns.
    eval_dt = function(xdt) {
      if (self$check_values) self$domain$assert_dt(xdt)
      private$.lookup(xdt)
    }
  ),

  active = list(
    #' @field data (`data.table`)\cr
    #' Read-only access to the internal data table.
    data = function(rhs) {
      if (!missing(rhs)) stop("data is read-only")
      private$.data
    },

    #' @field task ([mlr3::TaskRegr])\cr
    #' Returns a TaskRegr constructed from the internal data.
    task = function(rhs) {
      if (!missing(rhs)) stop("task is read-only")
      as_task_regr(copy(private$.data), target = private$.target_name)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (configurations) in the dataset.
    nrow = function(rhs) {
      if (!missing(rhs)) stop("nrow is read-only")
      nrow(private$.data)
    }
  ),

  private = list(
    .data = NULL,
    .target_name = NULL,

    # Perform table lookup
    .lookup = function(xdt) {
      domain_ids <- self$domain$ids()
      codomain_ids <- self$codomain$ids()

      # Use data.table join for efficient lookup
      # Set key on the stored data for the join
      data_keyed <- copy(private$.data)
      setkeyv(data_keyed, domain_ids)

      # Prepare query - ensure column order matches
      query <- xdt[, domain_ids, with = FALSE]

      # Perform the join
      matched <- data_keyed[query, on = domain_ids, nomatch = NA]

      # Check for missing matches
      missing_mask <- is.na(matched[[codomain_ids[1L]]])
      if (any(missing_mask)) {
        first_missing <- which(missing_mask)[1L]
        query_row <- xdt[first_missing, domain_ids, with = FALSE]
        query_str <- str_collapse(sprintf("%s=%s", domain_ids, sapply(query_row, as.character)))
        stopf("Configuration not found in dataset: {%s}", query_str)
      }

      # Check for duplicate matches (data.table join returns all matches)
      if (nrow(matched) > nrow(xdt)) {
        warning("Multiple matches found for some configurations, using first match for each")
        # Keep first match for each query row
        matched <- matched[, .SD[1L], by = domain_ids]
      }

      matched[, codomain_ids, with = FALSE]
    },

    # Override private $.eval_many for list input
    .eval_many = function(xss, ...) {
      xdt <- rbindlist(xss, use.names = TRUE, fill = TRUE)
      private$.lookup(xdt)
    },

    deep_clone = function(name, value) {
      if (name == ".data") {
        return(copy(value))
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    }
  )
)
