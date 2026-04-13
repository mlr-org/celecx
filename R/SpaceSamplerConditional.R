#' @title Conditional Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include SpaceSampler.R
#' @include SpaceSamplerUniform.R
#'
#' @name mlr_space_samplers_conditional
#'
#' @description
#' Wrapper sampler that delegates to one sampler for finite candidate pools and
#' another sampler for direct search-space sampling.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSampler].
#'
#' @section Construction:
#' ```
#' clx_sps("conditional")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerConditional <- R6Class("SpaceSamplerConditional",
  inherit = SpaceSampler,

  public = list(

    #' @description
    #' Creates a new conditional space sampler.
    #'
    #' @param on_discrete ([SpaceSampler])\cr
    #'   Sampler used when a `pool` is supplied.
    #' @param on_continuous ([SpaceSampler])\cr
    #'   Sampler used when `pool` is `NULL`.
    initialize = function(on_discrete = SpaceSamplerUniform$new(), on_continuous = SpaceSamplerUniform$new()) {
      private$.on_discrete <- assert_r6(on_discrete, "SpaceSampler")
      private$.on_continuous <- assert_r6(on_continuous, "SpaceSampler")

      super$initialize(
        id = sprintf("conditional_%s.%s", private$.on_discrete$id, private$.on_continuous$id),
        deterministic = private$.on_discrete$deterministic && private$.on_continuous$deterministic,
        packages = unique(c(private$.on_discrete$packages, private$.on_continuous$packages)),
        param_set = alist(
          on_discrete = private$.on_discrete$param_set,
          on_continuous = private$.on_continuous$param_set
        ),
        label = "Conditional Space Sampler",
        man = "celecx::mlr_space_samplers_conditional",
        additional_phash_input = c(".on_discrete", ".on_continuous")
      )
    }
  ),

  active = list(

    #' @field on_discrete ([SpaceSampler])
    #'   Sampler used when a `pool` is supplied.
    on_discrete = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.on_discrete)) {
        stop("on_discrete is read-only.")
      }
      private$.on_discrete
    },

    #' @field on_continuous ([SpaceSampler])
    #'   Sampler used when `pool` is `NULL`.
    on_continuous = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.on_continuous)) {
        stop("on_continuous is read-only.")
      }
      private$.on_continuous
    }
  ),

  private = list(
    .on_discrete = NULL,
    .on_continuous = NULL,

    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      sampler <- if (is.null(pool)) {
        private$.on_continuous
      } else {
        private$.on_discrete
      }
      sampler$sample(
        n = n,
        search_space = search_space,
        pool = pool,
        known_pool = known_pool
      )
    },

    deep_clone = function(name, value) {
      switch(name,
        .on_discrete = value$clone(deep = TRUE),
        .on_continuous = value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)

mlr_space_samplers$add("conditional", SpaceSamplerConditional)
