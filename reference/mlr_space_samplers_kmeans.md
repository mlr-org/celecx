# K-Means Space Sampler

K-means-based pool sampler.

The wrapped distance must expose a numeric reference embedding or
numeric reference columns after `set_reference_points()`.

Creates a new K-means space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSamplerDistance](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md).

## Arguments

- distance:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md))  
  Distance used to construct the fitted geometry.

## Construction

    clx_sps("kmeans")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
