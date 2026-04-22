# Chained Space Sampler

Wrapper sampler that applies several
[SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md)
objects one after the other to thin down a pool.

The `target_size_fn` hyperparameter is a function taking scalar
`pool_size` and scalar `n` and returning one target size per wrapped
sampler. The returned sequence must be non-increasing, each entry must
be at most `pool_size`, and the final entry must equal `n`.

Creates a new chained space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md).

## Arguments

- samplers:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))  
  Samplers to apply in sequence.

## Fields

- `samplers`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))
  Wrapped samplers.

## Construction

    clx_sps("chain")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md),
[`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
