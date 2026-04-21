#' @title Random Search on Pool-Restricted or Discrete Objectives
#'
#' @name mlr_optimizers_pool_random
#'
#' @description
#' Optimizer that randomly samples configurations from a candidate pool when
#' the objective has the `"pool_restricted"` property (typically by inheriting from
#' [ObjectivePoolAbstract]), or from the full grid of a completely discrete
#' search space (where all parameters are [paradox::p_int], [paradox::p_fct],
#' or [paradox::p_lgl]).
#'
#' When the search space is continuous (not fully discrete) and the objective is
#' not pool-restricted, this optimizer falls back to uniform random search
#' (like [bbotk::OptimizerBatchRandomSearch]).
#'
#' @section Parameters:
#' \describe{
#' \item{`batch_size`}{`integer(1)`\cr
#'   Number of configurations per batch. `Inf` evaluates the entire remaining
#'   pool in a single batch (forbidden when `replace_samples = "within_batches"`
#'   or when the objective is not pool-restricted).
#'   Default is `1`.}
#' \item{`max_batches`}{`integer(1)`\cr
#'   Maximum number of batches to evaluate. `Inf` means no batch limit
#'   (termination is governed solely by the instance's terminator). Default is
#'   `Inf`.}
#' \item{`replace_samples`}{`character(1)`\cr
#'   Controls duplicate handling across evaluations:
#'   \itemize{
#'     \item `"never"`: configurations already present in the archive are
#'       excluded from sampling. The pool shrinks as evaluations accumulate.
#'     \item `"between_batches"`: each batch is drawn without replacement from
#'       the full pool, but previous batches do not affect future sampling.
#'     \item `"within_batches"`: samples inside each batch are drawn with
#'       replacement from the full pool, so duplicates are possible even
#'       within a single batch.
#'   }
#'   Default is `"never"`.}
#' }
#'
#' @export
OptimizerPoolRandom <- R6Class("OptimizerPoolRandom",
  inherit = OptimizerPoolAbstract,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param grid_expansion_limit (`integer(1)`)\cr
    #'   Upper limit on the number of rows to materialize for the full grid for
    #'   discrete search spaces in Objectives that are not pool-restricted.
    #'   An error is raised if the full grid would exceed this limit.
    #'   This is to protect against accidental usage of too much memory.
    #'   Default is `1e7L`.
    initialize = function(grid_expansion_limit = 1e7L) {
      param_set <- ps(
        batch_size = p_int(lower = 1L, special_vals = list(Inf), init = 1, tags = "required"),
        max_batches = p_int(lower = 1L, special_vals = list(Inf), init = Inf, tags = "required"),
        replace_samples = p_fct(
          levels = c("never", "between_batches", "within_batches"),
          init = "never",
          tags = "required"
        )
      )

      super$initialize(
        id = "pool_random",
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl"),
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = c("paradox", "data.table"),
        label = "Pool Random Search",
        man = "celecx::mlr_optimizers_pool_random",
        grid_expansion_limit = grid_expansion_limit
      )
    }
  ),

  private = list(

    .optimize_discrete = function(inst, candidates) {
      pv <- self$param_set$get_values()
      batch_size <- pv$batch_size
      max_batches <- pv$max_batches
      replace_samples <- pv$replace_samples

      if (batch_size == Inf && replace_samples == "within_batches") {
        stop("batch_size = Inf is not allowed when replace_samples = 'within_batches'")
      }

      search_space_ids <- inst$search_space$ids()

      if (replace_samples == "never" && inst$archive$n_evals != 0L) {
        available_idx <- candidates[!inst$archive$data, on = search_space_ids, which = TRUE]
        n_available <- length(available_idx)
      } else {
        n_available <- nrow(candidates)
        available_idx <- seq_len(n_available)
      }

      batches_remaining <- max_batches
      if (replace_samples == "within_batches") {
        n_take <- batch_size  # if we sample with replacement, we don't need to limit batch size to available rows
      } else {
        # batch_size may be `Inf`, in which case we take all available
        n_take <- min(batch_size, n_available)
      }
      # max_batches may be `Inf`, in which case we do Inf - 1 -> Inf
      # Note n_available does not decrease when replace_samples != "never"
      while (batches_remaining > 0L && n_available > 0L) {
        if (replace_samples == "never") {
          # only need to re-compute n_take in this branch
          n_take <- min(batch_size, n_available)
          # Fisher-Yates partial shuffle
          pick_pos <- sample.int(n_available, n_take)  # positions from available_idx to take
          remaining_size <- n_available - n_take  # make available_idx smaller by n_take (without moving memory)
          selected_idx <- available_idx[pick_pos]  # the positions within the data.table to take
          unused_pos <- (remaining_size + 1L):n_available
          # entries in now cut off available_idx that are shifted to the front
          unused_pos <- unused_pos[!unused_pos %in% pick_pos]
          # ... these replace the positions inside available_idx that were selected to be evaluated and are discarded
          available_idx[pick_pos[pick_pos <= remaining_size]] <- available_idx[unused_pos]
          n_available <- remaining_size
        } else {
          selected_idx <- available_idx[
            sample.int(
              n_available,
              n_take,
              replace = replace_samples == "within_batches"
            )
          ]
        }

        batches_remaining <- batches_remaining - 1L
        inst$eval_batch(candidates[selected_idx])
      }
    },

    # basically OptimizerBatchRandomSearch with an optional batch number limit
    .optimize_continuous = function(inst) {
      pv <- self$param_set$get_values()
      batch_size <- pv$batch_size
      max_batches <- pv$max_batches

      if (batch_size == Inf) {
        stop("batch_size = Inf is not allowed when the search space is not fully discrete / pool-restricted")
      }

      sampler <- SamplerUnif$new(inst$search_space)
      remaining_batches <- max_batches
      # max_batches may be `Inf`, in which case we do Inf - 1 -> Inf
      while (remaining_batches > 0L) {
        design <- sampler$sample(as.integer(batch_size))
        inst$eval_batch(design$data)
        remaining_batches <- remaining_batches - 1L
      }
    }
  )
)

#' @include OptimizerPoolAbstract.R aaa.R
optimizers[["pool_random"]] = OptimizerPoolRandom
