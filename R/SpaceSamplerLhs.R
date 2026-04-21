#' @title LHS Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include SpaceSampler.R
#'
#' @name mlr_space_samplers_lhs
#'
#' @description
#' Sampler that draws Latin hypercube designs from a search space.
#'
#' This sampler only samples from the search space directly and errors when a
#' `pool` is supplied.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSampler].
#'
#' @section Construction:
#' ```
#' clx_sps("lhs")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerLhs <- R6Class("SpaceSamplerLhs",
  inherit = SpaceSampler,

  public = list(

    #' @description
    #' Creates a new LHS space sampler.
    initialize = function() {
      super$initialize(
        id = "lhs",
        deterministic = FALSE,
        packages = "lhs",
        label = "LHS Space Sampler",
        man = "celecx::mlr_space_samplers_lhs"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (!is.null(pool)) {
        stopf("SpaceSampler '%s' does not support 'pool'", self$id)
      }

      generate_design_lhs(search_space, n = n)$data
    }
  )
)

mlr_space_samplers$add("lhs", SpaceSamplerLhs)
