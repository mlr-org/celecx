# Chained Space Sampler

Wrapper sampler that applies several
[SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md)
objects one after the other to thin down a pool.

The `target_size_fn` hyperparameter is a function taking scalar
`pool_size` and scalar `n` and returning one target size per wrapped
sampler. The returned sequence must be non-increasing, each entry must
be at most `pool_size`, and the final entry must equal `n`.

Creates a new chained space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md).

## Arguments

- samplers:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))  
  Samplers to apply in sequence.

## Fields

- `samplers`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))
  Wrapped samplers.

## Construction

    clx_sps("chain")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://celecx.mlr-org.com/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md),
[`mlr_space_samplers_conditional`](https://celecx.mlr-org.com/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://celecx.mlr-org.com/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://celecx.mlr-org.com/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://celecx.mlr-org.com/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md)
