# Relational K-Means Space Sampler

K-means-style pool sampler driven directly by the pairwise distance
matrix.

This sampler clusters the fitted pool with the relational k-means
objective. For Euclidean distances it is equivalent to ordinary k-means
in an unknown embedding. For non-Euclidean distances it remains a
heuristic.

If `known_pool` is given, those points influence the implicit centroids
but are not eligible for selection.

Creates a new relational k-means space sampler.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[SpaceSamplerDistance](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md).

## Arguments

- distance:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md))  
  Distance used to construct the pairwise dissimilarity matrix.

## Construction

    clx_sps("relational_kmeans")

## See also

Other SpaceSampler:
[`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
