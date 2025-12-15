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
#' - `candidate_generator_local()`: Local sampling around best points
#' - `candidate_generator_mixed()`: Combination of global and local sampling
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
#'
#' @return A candidate generator function.
#'
#' @details
#' Warning: Grid size grows exponentially with dimension. For d dimensions
#' with resolution r, produces r^d points.
#'
#' Uses `paradox::generate_design_grid()` internally.
#'
#' @export
candidate_generator_grid <- function(resolution = 10L) {
  assert_integerish(resolution, lower = 2L, any.missing = FALSE, min.len = 1L)
  # Preserve names when converting to integer
  res_names <- names(resolution)
  resolution <- as.integer(resolution)
  names(resolution) <- res_names

  function(search_space, n) {
    # Note: n is ignored; grid size is determined by resolution
    if (is.null(names(resolution))) {
      design <- generate_design_grid(search_space, resolution = resolution[[1L]])
    } else {
      # Named vector: use param_resolutions
      design <- generate_design_grid(search_space, param_resolutions = resolution)
    }
    design$data
  }
}


#' @title Local Candidate Generator
#'
#' @description
#' Generates candidates locally around promising regions, identified from
#' the archive. Good for exploitation/refinement.
#'
#' @param n_centers (`integer(1)`)\cr
#'   Number of archive points to sample around. Points are selected by
#'   their y values (best first).
#' @param radius (`numeric(1)`)\cr
#'   Sampling radius around each center, as a fraction of the domain range.
#'   Default 0.1 (10% of range).
#'
#' @return A candidate generator function.
#'
#' @details
#' Algorithm:
#' 1. Select n_centers best points from archive
#' 2. For each center, generate n/n_centers local samples
#' 3. Local samples are uniform within a hypercube around the center
#'
#' Note: This generator needs access to the archive, so the signature is
#' extended to `function(search_space, n, archive)`.
#'
#' @export
candidate_generator_local <- function(n_centers = 5L, radius = 0.1) {
  assert_int(n_centers, lower = 1L)
  assert_number(radius, lower = 0, upper = 0.5)

  function(search_space, n, archive = NULL) {
    # If no archive, fall back to LHS
    # Otherwise:
    # 1. Get top n_centers points from archive
    # 2. For each center, sample locally
    # 3. Clip to search_space bounds
    # 4. Return combined candidates
    stop("Not implemented")
  }
}


#' @title Mixed Candidate Generator
#'
#' @description
#' Combines global and local sampling for a balance of exploration and
#' exploitation.
#'
#' @param global_fraction (`numeric(1)`)\cr
#'   Fraction of candidates from global sampling (LHS). Default 0.5.
#' @param global_generator (`function`)\cr
#'   Generator for global samples. Default is LHS.
#' @param local_generator (`function`)\cr
#'   Generator for local samples. Default is local generator.
#'
#' @return A candidate generator function.
#'
#' @details
#' Splits the requested n candidates between global and local generators.
#' Useful for maintaining exploration while focusing on promising regions.
#'
#' @export
candidate_generator_mixed <- function(global_fraction = 0.5,
    global_generator = NULL, local_generator = NULL) {
  assert_number(global_fraction, lower = 0, upper = 1)

  # Default generators
  global_gen <- global_generator %??% candidate_generator_lhs()
  local_gen <- local_generator %??% candidate_generator_local()

  function(search_space, n, archive = NULL) {
    # Split n between global and local
    # n_global <- ceiling(n * global_fraction)
    # n_local <- n - n_global
    # Generate from both and rbind
    stop("Not implemented")
  }
}


# =============================================================================
# Helper Functions
# =============================================================================

#' @title Sample in Local Region
#'
#' @description
#' Generates random points in a hypercube around a center point.
#'
#' @param center (`data.table` or `list`) Center point.
#' @param n (`integer(1)`) Number of points to generate.
#' @param radius (`numeric(1)`) Radius as fraction of domain range.
#' @param search_space ([paradox::ParamSet]) The search space.
#'
#' @return `data.table` of n points.
#'
#' @keywords internal
sample_local <- function(center, n, radius, search_space) {
  # For each dimension:
  #   - Compute local range: [center - radius*range, center + radius*range]
  #   - Clip to search_space bounds
  #   - Sample uniformly in local range
  # Handle categorical variables: sample with higher probability for center value
  stop("Not implemented")
}
