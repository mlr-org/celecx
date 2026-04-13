#' @title Objective Based on Pre-evaluated Dataset
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
#' @include ObjectivePoolAbstract.R utils_codomain.R
#' @export
ObjectiveDataset <- R6Class("ObjectiveDataset",
  inherit = ObjectivePoolAbstract,
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
      assert_param_set(codomain)  # most values are validated by the parent constructor, but we need this one before.
      target_name <- codomain_target_ids(codomain)

      # Extract data.table from TaskRegr or convert data.frame/data.table
      if (inherits(dataset, "TaskRegr")) {
        if (!identical(target_name, dataset$target_names)) {
          stopf("Target column in the dataset (%s) does not match the codomain's target (%s).", str_collapse(dataset$target_names), str_collapse(target_name))
        }
        dataset <- dataset$data()
      }

      dataset <- as.data.table(dataset)

      # Validate and process the codomain within the dataset; domain is validated by the parent constructor
      assert_data_table_param_set(dataset, codomain, require_uniqueness = FALSE, allow_untyped = TRUE, .param_set_name = "Codomain", .dt_name = "dataset")

      # Call parent constructor (rejects duplicate domain rows)
      super$initialize(
        pool = dataset,
        domain = domain,
        codomain = codomain,
        id = id,
        properties = "deterministic",
        check_values = check_values,
        label = "Objective from Pre-evaluated Dataset",
        man = "celecx::ObjectiveDataset"
      )
      # check this after parent checked everything else
      assert_names(colnames(dataset), permutation.of = c(domain$ids(), codomain$ids()))
    }
  ),

  private = list(
    .eval_pool = function(matched_pool) {
      # since our pool contains both domain and codomain columns and .eval_pool gets the whole pool,
      # we can just extract the codomain columns
      codomain_ids <- self$codomain$ids()
      matched_pool[, codomain_ids, with = FALSE]
    }
  )
)
