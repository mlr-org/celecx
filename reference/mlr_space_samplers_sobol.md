# Sobol Space Sampler

Sampler that draws Sobol sequence designs from a search space.

This sampler only samples from the search space directly and errors when
a `pool` is supplied.

Creates a new Sobol space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md).

## Construction

    clx_sps("sobol")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://celecx.mlr-org.com/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://celecx.mlr-org.com/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://celecx.mlr-org.com/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://celecx.mlr-org.com/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://celecx.mlr-org.com/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_uniform`](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md)
