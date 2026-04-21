#' @title Sobol Space Sampler
#'
#' @include mlr_space_samplers.R
#' @include SpaceSampler.R
#'
#' @name mlr_space_samplers_sobol
#'
#' @description
#' Sampler that draws Sobol sequence designs from a search space.
#'
#' This sampler only samples from the search space directly and errors when a
#' `pool` is supplied.
#'
#' @format [R6::R6Class] object inheriting from [SpaceSampler].
#'
#' @section Construction:
#' ```
#' clx_sps("sobol")
#' ```
#'
#' @export
#' @family SpaceSampler
SpaceSamplerSobol <- R6Class("SpaceSamplerSobol",
  inherit = SpaceSampler,

  public = list(

    #' @description
    #' Creates a new Sobol space sampler.
    initialize = function() {
      super$initialize(
        id = "sobol",
        deterministic = FALSE,
        packages = "spacefillr",
        label = "Sobol Space Sampler",
        man = "celecx::mlr_space_samplers_sobol"
      )
    }
  ),

  private = list(
    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      if (!is.null(pool)) {
        stopf("SpaceSampler '%s' does not support 'pool'", self$id)
      }

      generate_design_sobol(search_space, n = n)$data
    }
  )
)

mlr_space_samplers$add("sobol", SpaceSamplerSobol)
