# Distance-Based Space Sampler

Abstract base class for pool samplers that are driven by an
[ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md).

The wrapped distance object is stored directly and its
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
is forwarded as the sampler parameter set.

Creates a new distance-based space sampler.

## Arguments

- id:

  (`character(1)`)  
  Identifier of the sampler.

- distance:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md))  
  Distance object used by the sampler.

## Fields

- `distance`:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md))
  Wrapped distance object.

## See also

Other SpaceSampler:
[`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md),
[`mlr_space_samplers`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
