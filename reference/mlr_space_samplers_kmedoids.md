# K-Medoids Space Sampler

K-medoids-style pool sampler based on an arbitrary
[ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md).

Medoids are selected from the candidate pool using a greedy build step
followed by swap-based local improvement. The optimization target is the
sum of distances to the nearest selected medoid.

If `known_pool` is given, those points are treated as fixed medoids and
the selected pool points are optimized to complement them.

Creates a new k-medoids space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSamplerDistance](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md).

## Arguments

- distance:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md))  
  Distance used for the medoid objective.

## Construction

    clx_sps("kmedoids")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
