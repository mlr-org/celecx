#' @title Abstract Base Class for Pool-Aware Optimizers
#'
#' @description
#' Extends [OptimizerSearchAbstract] with infrastructure for optimizing over
#' pool-restricted objectives ([ObjectivePoolAbstract]) and fully discrete
#' search spaces.
#'
#' Subclasses implement the actual optimization logic by overriding
#' the private methods `.optimize_discrete()` and `.optimize_continuous()`.
#' This base class handles pool detection, candidate filtering, and
#' discrete-grid expansion.
#'
#' @section Extending:
#' Concrete subclasses must implement two private methods:
#' \describe{
#'   \item{`.optimize_discrete(inst, candidates)`}{Called when the search space
#'     is fully discrete or the objective is pool-restricted. `candidates` is a
#'     `data.table` of admissible configurations.}
#'   \item{`.optimize_continuous(inst)`}{Called when the search space contains
#'     continuous parameters and the objective is not pool-restricted.}
#' }
#'
#' @export
OptimizerPoolAbstract <- R6Class("OptimizerPoolAbstract",
  inherit = OptimizerSearchAbstract,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the optimizer.
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Set of control parameters.
    #' @param param_classes (`character()`)\cr
    #'   Supported parameter classes.
    #' @param properties (`character()`)\cr
    #'   Optimizer properties.
    #' @param packages (`character()`)\cr
    #'   Required packages.
    #' @param label (`character(1)`)\cr
    #'   Human-readable label.
    #' @param man (`character(1)`)\cr
    #'   Manual page reference.
    #' @param grid_expansion_limit (`integer(1)`)\cr
    #'   Upper limit on the number of rows to materialize for the full grid for
    #'   discrete search spaces in Objectives that are not pool-restricted.
    #'   An error is raised if the full grid would exceed this limit.
    #'   Default is `1e7L`.
    initialize = function(id, param_set = ps(), param_classes, properties,
        packages = character(), label = NA_character_, man = NA_character_,
        grid_expansion_limit = 1e7L) {
      private$.grid_expansion_limit <- assert_count(grid_expansion_limit, positive = TRUE, coerce = TRUE)

      super$initialize(
        id = id,
        param_set = param_set,
        param_classes = param_classes,
        properties = properties,
        packages = packages,
        label = label,
        man = man
      )
    }
  ),

  active = list(
    #' @field grid_expansion_limit (`integer(1)`)\cr
    #'   Upper limit on the number of rows to materialize for the full grid for
    #'   discrete search spaces in Objectives that are not pool-restricted.
    grid_expansion_limit = function() {
      private$.grid_expansion_limit
    }
  ),

  private = list(
    .grid_expansion_limit = NULL,

    .optimize = function(inst) {
      objective <- inst$objective
      search_space <- inst$search_space
      is_pool <- "pool_restricted" %in% objective$properties
      if (is_pool) {
        assert_pool_objective_search_space(objective, search_space)
        domain_ids <- inst$objective$domain$ids()
        if (!setequal(search_space$ids(), domain_ids)) {
          stopf(
            "pool_random requires search_space ids identical to the objective domain ids for pool-restricted objectives: %s",
            str_collapse(domain_ids)
          )
        }
      }

      is_finite <- is_pool || all(is.finite(search_space$nlevels))
      if (!is_finite) {
        return(private$.optimize_continuous(inst))
      }

      search_space_ids <- search_space$ids()
      if (is_pool) {
        candidates <- private$.pool_candidates(
          objective = objective,
          search_space = search_space
        )
      } else {
        max_nlev <- max(inst$search_space$nlevels)
        candidates <- generate_design_grid_celecx(
          inst$search_space,
          resolution = max_nlev,
          upper_limit = private$.grid_expansion_limit
        )$data[, search_space_ids, with = FALSE]
      }

      private$.optimize_discrete(inst, candidates)
    },

    # Filter pool candidates to only include only configurations that are valid in the search space
    .pool_candidates = function(objective, search_space) {
      pool_xdt <- objective$pool[, search_space$ids(), with = FALSE]
      processed_xdt <- copy(pool_xdt)

      apply_param_set_design_dt(processed_xdt, search_space)

      keep <- rep(TRUE, nrow(pool_xdt))
      for (id in search_space$ids()) {
        original_col <- pool_xdt[[id]]
        processed_col <- processed_xdt[[id]]
        # both NA -> TRUE --> filters out NAs in the `==` expression using `|`
        # neither NA -> equal values -> TRUE
        same <- is.na(original_col) == is.na(processed_col) & (is.na(original_col) | original_col == processed_col)
        keep <- keep & same

        if (!any(keep)) {
          break
        }
      }

      keep_idx <- which(keep)
      keep_idx <- keep_idx[param_set_valid_mask_dt(
        processed_xdt[keep_idx],
        search_space
      )]

      candidates <- pool_xdt[keep_idx]
      if (!nrow(candidates)) {
        stop("Search space leaves no admissible configurations in the objective pool")
      }

      candidates
    },

    # Subclasses must override:
    .optimize_discrete = function(inst, candidates) {
      stop("OptimizerPoolAbstract$.optimize_discrete() is abstract")
    },

    .optimize_continuous = function(inst) {
      stop("OptimizerPoolAbstract$.optimize_continuous() is abstract")
    }
  )
)

#' @include OptimizerSearchAbstract.R
NULL
