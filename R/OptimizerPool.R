#' @title Pool-Based Optimizer
#'
#' @name mlr_optimizers_pool
#'
#' @description
#' Optimizer that generates a pool of candidates and evaluates them all.
#' This is an extension of [bbotk::OptimizerBatchRandomSearch] that allows
#' for more flexible candidate generation.
#'
#' @section Parameters:
#' * `candidate_generator` (`function`)\cr
#'   Function to generate candidate points. Must have signature
#'   `function(search_space, n)` and return a `data.table` of n candidates.
#'   Default is [candidate_generator_lhs()].
#'
#' * `pool_size` (`integer(1)`)\cr
#'   Number of candidates to generate and evaluate. If `NULL`, calculates
#'   remaining evaluations from the terminator (requires [bbotk::TerminatorEvals]).
#'   Default is to use the remaining evaluations from the terminator.
#'
#' @examples
#' \dontrun{
#' optimizer <- opt("pool")
#' optimizer$param_set$set_values(
#'   candidate_generator = candidate_generator_lhs(),
#'   pool_size = 100L
#' )
#' }
#'
#' @export
OptimizerPool <- R6Class("OptimizerPool",
  inherit = bbotk::OptimizerBatch,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      # Set up param_set with our configuration parameters
      param_set <- ps(
        candidate_generator = p_uty(custom_check = check_function, tags = "required"),
        pool_size = p_int(lower = 1L)
      )

      param_set$set_values(
        candidate_generator = candidate_generator_lhs(),
        pool_size = NULL
      )

      super$initialize(
        id = "pool",
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl"),
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = c("paradox", "data.table"),
        label = "Pool-Based Optimizer",
        man = "celecx::mlr_optimizers_pool"
      )
    }
  ),

  private = list(

    .optimize = function(inst) {
      # Get parameters
      pv <- self$param_set$get_values()
      generator <- pv$candidate_generator
      pool_size <- pv$pool_size

      # Calculate pool size if not explicitly set
      if (is.null(pool_size)) {
        # Use remaining evaluations from terminator
        if (!inherits(inst$terminator, "TerminatorEvals")) {
          stop("Terminator must be a TerminatorEvals when no pool_size is set")
        }
        status <- inst$terminator$status(inst$archive)
        pool_size <- status["max_steps"] - status["current_steps"]
      }

      # Generate candidate pool
      search_space <- inst$search_space
      candidates <- generator(search_space, pool_size)

      # Evaluate all candidates
      # The instance will handle storing results in the archive
      inst$eval_batch(candidates)

      # Return invisibly (results are in archive)
      invisible(NULL)
    }
  )
)

#' @include aaa.R
optimizers[["pool"]] = OptimizerPool
