#' @title Uniform Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include SpaceSampler.R
#'
#' @name mlr_space_samplers_uniform
#'
#' @description
#' Sampler that draws uniformly from the search space or uniformly without
#' replacement from a given pool.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSampler].
#'
#' @section Construction:
#' ```
#' clx_sps("uniform")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerUniform <- R6Class("SpaceSamplerUniform",
  inherit = SpaceSampler,

  public = list(

    #' @description
    #' Creates a new uniform space sampler.
    initialize = function() {
      super$initialize(
        id = "uniform",
        deterministic = TRUE,
        label = "Uniform Space Sampler",
        man = "celecx::mlr_space_samplers_uniform"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (is.null(pool)) {
        return(generate_design_random(search_space, n)$data)
      }

      pool[sample.int(nrow(pool), n, replace = FALSE)]
    }
  )
)

mlr_space_samplers$add("uniform", SpaceSamplerUniform)
