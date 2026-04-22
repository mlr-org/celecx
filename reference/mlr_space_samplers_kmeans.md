# K-Means Space Sampler

K-means-based pool sampler.

The wrapped distance must expose a numeric reference embedding or
numeric reference columns after `set_reference_points()`.

Creates a new K-means space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSamplerDistance](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md).

## Arguments

- distance:

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md))  
  Distance used to construct the fitted geometry.

## Construction

    clx_sps("kmeans")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://celecx.mlr-org.com/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://celecx.mlr-org.com/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://celecx.mlr-org.com/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://celecx.mlr-org.com/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmedoids`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://celecx.mlr-org.com/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://celecx.mlr-org.com/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md)
