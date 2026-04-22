# Distance-Based Space Sampler

Abstract base class for pool samplers that are driven by an
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md).

The wrapped distance object is stored directly and its
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
is forwarded as the sampler parameter set.

Creates a new distance-based space sampler.

## Arguments

- id:

  (`character(1)`)  
  Identifier of the sampler.

- distance:

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md))  
  Distance object used by the sampler.

## Fields

- `distance`:

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md))
  Wrapped distance object.

## See also

Other SpaceSampler:
[`SpaceSampler`](https://celecx.mlr-org.com/reference/SpaceSampler.md),
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md),
[`mlr_space_samplers_chain`](https://celecx.mlr-org.com/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://celecx.mlr-org.com/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://celecx.mlr-org.com/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://celecx.mlr-org.com/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://celecx.mlr-org.com/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://celecx.mlr-org.com/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://celecx.mlr-org.com/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md)
