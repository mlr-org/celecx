#' @title Sampler-Based Search on Pool-Restricted or Discrete Objectives
#'
#' @name mlr_optimizers_pool_sample
#'
#' @description
#' Optimizer that samples configurations with a [SpaceSampler] from a candidate
#' pool when the objective has the `"pool_restricted"` property (typically by
#' inheriting from [ObjectivePoolAbstract]), or from the full grid of a
#' completely discrete search space (where all parameters are [paradox::p_int],
#' [paradox::p_fct], or [paradox::p_lgl]).
#'
#' When the search space is continuous (not fully discrete) and the objective is
#' not pool-restricted, this optimizer samples directly from the search space.
#'
#' @section Parameters:
#' \describe{
#' \item{`batch_size`}{`integer(1)`\cr
#'   Number of configurations per batch. `Inf` evaluates the entire remaining
#'   pool in a single batch when `replace_samples = "never"`, or the full pool
#'   in each batch when `replace_samples = "between_batches"`. `Inf` is
#'   forbidden when the objective is not pool-restricted and the search space is
#'   not fully discrete. Default is `1`.}
#' \item{`max_batches`}{`integer(1)`\cr
#'   Maximum number of batches to evaluate. `Inf` means no batch limit
#'   (termination is governed solely by the instance's terminator). Default is
#'   `Inf`.}
#' \item{`replace_samples`}{`character(1)`\cr
#'   Controls duplicate handling across evaluations:
#'   \itemize{
#'     \item `"never"`: configurations already present in the archive are
#'       excluded from sampling. The pool shrinks as evaluations accumulate.
#'     \item `"between_batches"`: each batch is sampled from the full pool, but
#'       previous batches do not affect future sampling.
#'   }
#'   Default is `"never"`.}
#' \item{`pass_known_pool`}{`logical(1)`\cr
#'   Whether evaluated configurations from the archive are passed to the
#'   [SpaceSampler]'s `sample()` method as `known_pool`. Default is `TRUE`.}
#' }
#'
#' The wrapped sampler's parameters are exposed with a `space_sampler.` prefix.
#'
#' @export
OptimizerPoolSample <- R6Class("OptimizerPoolSample",
  inherit = OptimizerPoolAbstract,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param space_sampler ([SpaceSampler])\cr
    #'   Sampler used to draw each batch. Default is [SpaceSamplerUniform].
    #' @param grid_expansion_limit (`integer(1)`)\cr
    #'   Upper limit on the number of rows to materialize for the full grid for
    #'   discrete search spaces in Objectives that are not pool-restricted.
    #'   An error is raised if the full grid would exceed this limit.
    #'   This is to protect against accidental usage of too much memory.
    #'   Default is `1e7L`.
    initialize = function(space_sampler = SpaceSamplerUniform$new(), grid_expansion_limit = 1e7L) {
      private$.space_sampler <- assert_r6(space_sampler, "SpaceSampler")
      private$.own_param_set <- ps(
        batch_size = p_int(lower = 1L, special_vals = list(Inf), init = 1, tags = "required"),
        max_batches = p_int(lower = 1L, special_vals = list(Inf), init = Inf, tags = "required"),
        replace_samples = p_fct(
          levels = c("never", "between_batches"),
          init = "never",
          tags = "required"
        ),
        pass_known_pool = p_lgl(init = TRUE, tags = "required")
      )

      super$initialize(
        id = "pool_sample",
        param_set = ps(),
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl"),
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = private$.space_sampler$packages,
        label = "Pool Sample Search",
        man = "celecx::mlr_optimizers_pool_sample",
        grid_expansion_limit = grid_expansion_limit
      )

      private$.param_set <- NULL
    }
  ),

  active = list(

    #' @field space_sampler ([SpaceSampler])\cr
    #'   Wrapped sampler.
    space_sampler = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.space_sampler)) {
        stop("space_sampler is read-only.")
      }
      private$.space_sampler
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #'   Combined parameter set of this optimizer and the wrapped sampler.
    param_set = function(rhs) {
      if (is.null(private$.param_set)) {
        private$.param_set <- ParamSetCollection$new(list(
          private$.own_param_set,
          space_sampler = private$.space_sampler$param_set
        ))
      }
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .space_sampler = NULL,
    .own_param_set = NULL,

    .optimize_discrete = function(inst, candidates) {
      private$.optimize_discrete_continuous(inst, candidates = candidates)
    },

    .optimize_continuous = function(inst) {
      private$.optimize_discrete_continuous(inst, NULL)
    },

    # candidates = NULL for continuous search space
    .optimize_discrete_continuous = function(inst, candidates = NULL) {
      pv <- private$.own_param_set$get_values()
      batch_size <- pv$batch_size
      max_batches <- pv$max_batches
      replace_samples <- pv$replace_samples
      pass_known_pool <- pv$pass_known_pool

      if (is.null(candidates) && batch_size == Inf) {
        stop("batch_size = Inf is not allowed when the search space is not fully discrete / pool-restricted")
      }

      if (private$.space_sampler$deterministic && replace_samples == "between_batches" && !pass_known_pool) {
        warningf(
          "SpaceSampler '%s' is deterministic, replace_samples = 'between_batches', and pass_known_pool = FALSE; every batch may contain the same sample",
          private$.space_sampler$id
        )
      }

      search_space_ids <- inst$search_space$ids()
      available_pool <- if (!is.null(candidates) && replace_samples == "never" && inst$archive$n_evals != 0L) {
        candidates[!inst$archive$data, on = search_space_ids]
      } else {
        candidates
      }

      batches_remaining <- max_batches
      n_available <- if (!is.null(candidates)) nrow(available_pool) else Inf
      while (batches_remaining > 0L && n_available > 0L) {
        known_pool <- if (pass_known_pool && inst$archive$n_evals != 0L) {
          inst$archive$data[, search_space_ids, with = FALSE]
        }
        n_take <- min(batch_size, n_available)

        selected <- private$.space_sampler$sample(
          n = n_take,
          search_space = inst$search_space,
          pool = available_pool,
          known_pool = known_pool
        )

        batches_remaining <- batches_remaining - 1L
        inst$eval_batch(selected)

        if (!is.null(candidates) && replace_samples == "never") {
          available_pool <- available_pool[!selected, on = search_space_ids]
          n_available <- nrow(available_pool)
        }
      }

    },

    deep_clone = function(name, value) {
      switch(name,
        .space_sampler = {
          private$.param_set <- NULL
          value$clone(deep = TRUE)
        },
        .own_param_set = {
          private$.param_set <- NULL
          value$clone(deep = TRUE)
        },
        .param_set = NULL,
        value
      )
    }
  )
)

#' @include OptimizerPoolAbstract.R SpaceSamplerUniform.R aaa.R
optimizers[["pool_sample"]] = OptimizerPoolSample
