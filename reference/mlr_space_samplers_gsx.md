# GSx Space Sampler

Greedy-sampling-in-input-space sampler.

Without `known_pool`, the first selected point is the pool point
minimizing the sum of squared distances to all other pool points.
Subsequent points are chosen by farthest-first traversal, maximizing the
minimum squared distance to the already-selected or known points.

Creates a new GSx space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSamplerDistance](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md).

## Arguments

- distance:

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md))  
  Distance used for the GSx selection rule.

## Construction

    clx_sps("gsx")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://celecx.mlr-org.com/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://celecx.mlr-org.com/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://celecx.mlr-org.com/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://celecx.mlr-org.com/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://celecx.mlr-org.com/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md)
