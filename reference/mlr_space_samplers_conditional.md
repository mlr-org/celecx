# Conditional Space Sampler

Wrapper sampler that delegates to one sampler for finite candidate pools
and another sampler for direct search-space sampling.

Creates a new conditional space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md).

## Arguments

- on_discrete:

  ([SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))  
  Sampler used when a `pool` is supplied.

- on_continuous:

  ([SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))  
  Sampler used when `pool` is `NULL`.

## Fields

- `on_discrete`:

  ([SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))
  Sampler used when a `pool` is supplied.

- `on_continuous`:

  ([SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))
  Sampler used when `pool` is `NULL`.

## Construction

    clx_sps("conditional")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://celecx.mlr-org.com/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://celecx.mlr-org.com/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_gsx`](https://celecx.mlr-org.com/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://celecx.mlr-org.com/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://celecx.mlr-org.com/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md)
