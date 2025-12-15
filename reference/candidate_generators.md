# Candidate Point Generators

Functions for generating candidate points from which the batch proposer
selects. Different generators trade off coverage, speed, and adaptivity.

## Details

All generator functions return a **generator function** with signature:

    function(search_space, n)

The generator function returns a `data.table` of n candidate
configurations.

## Available Generators

- [`candidate_generator_lhs()`](https://celecx.mlr-org.com/reference/candidate_generator_lhs.md):
  Latin Hypercube Sampling (space-filling)

- [`candidate_generator_random()`](https://celecx.mlr-org.com/reference/candidate_generator_random.md):
  Pure random sampling

- [`candidate_generator_sobol()`](https://celecx.mlr-org.com/reference/candidate_generator_sobol.md):
  Sobol quasi-random sequence

- [`candidate_generator_grid()`](https://celecx.mlr-org.com/reference/candidate_generator_grid.md):
  Regular grid (for small spaces)

- [`candidate_generator_local()`](https://celecx.mlr-org.com/reference/candidate_generator_local.md):
  Local sampling around best points

- [`candidate_generator_mixed()`](https://celecx.mlr-org.com/reference/candidate_generator_mixed.md):
  Combination of global and local sampling
