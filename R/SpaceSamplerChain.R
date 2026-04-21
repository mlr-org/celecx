#' @title Chained Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include SpaceSampler.R
#' @include SpaceSamplerUniform.R
#'
#' @name mlr_space_samplers_chain
#'
#' @description
#' Wrapper sampler that applies several [SpaceSampler] objects one after the
#' other to thin down a pool.
#'
#' The `target_size_fn` hyperparameter is a function taking scalar `pool_size` and
#' scalar `n` and returning one target size per wrapped sampler. The returned
#' sequence must be non-increasing, each entry must be at most `pool_size`, and
#' the final entry must equal `n`.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSampler].
#'
#' @section Construction:
#' ```
#' clx_sps("chain")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerChain <- R6Class("SpaceSamplerChain",
  inherit = SpaceSampler,

  public = list(

    #' @description
    #' Creates a new chained space sampler.
    #'
    #' @param samplers (named `list()` of [SpaceSampler])\cr
    #'   Samplers to apply in sequence.
    initialize = function(samplers = list(uniform = SpaceSamplerUniform$new())) {
      assert_list(samplers, min.len = 1L, names = "unique")
      walk(samplers, assert_r6, "SpaceSampler")

      sampler_count <- length(samplers)
      private$.samplers <- samplers
      private$.chain_param_set <- ps(
        target_size_fn = p_uty(custom_check = check_function, init = crate(
          function(pool_size, n) {
            if (is.finite(pool_size)) {
              # finite pool: geometrically decreasing sequence
              sequence <- exp(seq(log(pool_size), log(n), length.out = sampler_count + 1)[-1])
              sequence <- round(sequence)
              sequence[sequence < n] <- n
              sequence[sequence > pool_size] <- pool_size
              sequence
            } else {
              # infinite pool: halve with every sampler
              n * 2^seq.int(sampler_count - 1, 0)
            }
          }, sampler_count), tags = "required"
        )
      )
      super$initialize(
        id = sprintf("chain_%s", paste(names(samplers), collapse = ".")),
        deterministic = every(samplers, "deterministic"),
        packages = unique(unlist(map(samplers, "packages"), use.names = FALSE)),
        param_set = c(
          alist(private$.chain_param_set),
          imap(samplers, function(sampler, sname) {
            # use imap so we keep the list names
            substitute(private$.samplers[[sname]]$param_set, list(sname = sname))
          })
        ),
        label = "Chained Space Sampler",
        man = "celecx::mlr_space_samplers_chain",
        additional_phash_input = ".samplers"
      )
    }
  ),

  active = list(

    #' @field samplers (named `list()` of [SpaceSampler])
    #'   Wrapped samplers.
    samplers = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.samplers)) {
        stop("samplers is read-only.")
      }
      private$.samplers
    }
  ),

  private = list(
    .samplers = NULL,
    .chain_param_set = NULL,

    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      pool_size <- if (is.null(pool)) Inf else nrow(pool)
      target_size_fn <- private$.chain_param_set$get_values()$target_size_fn
      sequence_values <- target_size_fn(pool_size = pool_size, n = n)
      sequence_values <- assert_integerish(
        sequence_values,
        len = length(private$.samplers),
        lower = n,
        upper = pool_size,
        any.missing = FALSE,
        tol = 0,
        coerce = TRUE
      )

      if (any(diff(sequence_values) > 0)) {
        stopf("SpaceSamplerChain requires 'target_size_fn' to return a non-increasing sequence")
      }
      if (tail(sequence_values, 1L) != n) {
        stopf("SpaceSamplerChain requires the final 'target_size_fn' entry to equal 'n'")
      }

      current_pool <- pool
      for (i in seq_along(private$.samplers)) {
        current_pool <- private$.samplers[[i]]$sample(
          n = sequence_values[[i]],
          search_space = search_space,
          pool = current_pool,
          known_pool = known_pool
        )
      }

      current_pool
    },

    deep_clone = function(name, value) {
      switch(name,
        .samplers = lapply(value, function(sampler) sampler$clone(deep = TRUE)),
        super$deep_clone(name, value)
      )
    }
  )
)

mlr_space_samplers$add("chain", SpaceSamplerChain)
