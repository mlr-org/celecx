#' @title Candidate Point Generators
#'
#' @description
#' Functions for generating candidate points from which the batch proposer
#' selects. Different generators trade off coverage, speed, and adaptivity.
#'
#' @details
#' All generator functions return a **generator function** with signature:
#' ```
#' function(search_space, n)
#' ```
#' The generator function returns a `data.table` of n candidate configurations.
#'
#' @section Available Generators:
#' - `candidate_generator_lhs()`: Latin Hypercube Sampling (space-filling)
#' - `candidate_generator_random()`: Pure random sampling
#' - `candidate_generator_sobol()`: Sobol quasi-random sequence
#' - `candidate_generator_grid()`: Regular grid (for small spaces)
#'
#' @name candidate_generators
NULL


#' @title LHS Candidate Generator
#'
#' @description
#' Generates candidates using Latin Hypercube Sampling, which provides
#' good space-filling properties.
#'
#' @return A candidate generator function.
#'
#' @details
#' LHS ensures that each variable's marginal distribution is uniformly
#' covered, even with a relatively small number of samples. Good default
#' choice for most problems.
#'
#' Uses `paradox::generate_design_lhs()` internally.
#'
#' @export
candidate_generator_lhs <- function() {
  function(search_space, n) {
    design <- generate_design_lhs(search_space, n = n)
    design$data
  }
}


#' @title Random Candidate Generator
#'
#' @description
#' Generates candidates using pure random sampling.
#'
#' @return A candidate generator function.
#'
#' @details
#' Simpler and faster than LHS, but may have poor coverage with small n.
#' Use when speed matters more than coverage, or when n is very large.
#'
#' Uses `paradox::generate_design_random()` internally.
#'
#' @export
candidate_generator_random <- function() {
  function(search_space, n) {
    design <- generate_design_random(search_space, n = n)
    design$data
  }
}


#' @title Sobol Candidate Generator
#'
#' @description
#' Generates candidates using Sobol quasi-random sequences, which have
#' even better space-filling properties than LHS for low discrepancy.
#'
#' @return A candidate generator function.
#'
#' @details
#' Sobol sequences are deterministic and provide excellent coverage.
#' However, they're designed for continuous spaces; categorical variables
#' are handled by discretizing.
#'
#' @export
candidate_generator_sobol <- function() {
  function(search_space, n) {
    design <- generate_design_sobol(search_space, n = n)
    design$data
  }
}


#' @title Grid Candidate Generator
#'
#' @description
#' Generates candidates on a regular grid. Only suitable for low-dimensional
#' problems or when specific resolution is needed.
#'
#' @param resolution (`integer(1)` | `named integer()`)\cr
#'   Number of points per dimension. Can be a single value applied to all
#'   dimensions, or a named vector with per-dimension values.
#' @param limit (`integer(1)`)\cr
#'   Upper limit on number of points to generate; an error is generated if the
#'   number of points that would be generated exceeds this limit. Default is 1e6.
#'
#' @return A candidate generator function.
#'
#' @details
#' Warning: Grid size grows exponentially with dimension. For d dimensions
#' with resolution r, produces r^d points.
#'
#' Emulates `paradox::generate_design_grid()`.
#'
#' @export
candidate_generator_grid <- function(resolution = 10L, limit = 1e7L) {
  limit <- assert_count(limit, coerce = TRUE)

  if (is.null(names(resolution))) {
    resolution <- assert_count(resolution, positive = TRUE, coerce = TRUE)  # must be scalar when unnamed
    param_resolutions <- NULL
  } else {
    param_resolutions <- assert_integerish(resolution, lower = 1L, any.missing = FALSE, min.len = 1L, coerce = TRUE)
    resolution <- NULL
  }

  function(search_space, n) {
    # Note: n is ignored; grid size is determined by resolution
    design <- generate_design_grid_celecx(
      search_space,
      resolution = resolution,
      param_resolutions = param_resolutions,
      upper_limit = limit
    )
    design$data
  }
}
