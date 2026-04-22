# Space Sampler Base Class

Abstract base class for objects that sample points from a search space.

A `SpaceSampler` can sample directly from a
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
or thin down an existing candidate pool. Some samplers may additionally
make use of a `known_pool` of already-known points. The intended
contract is that `pool` and `known_pool` are disjoint, but this is not
checked.

If `pool` is given and `n >= nrow(pool)`, the full `pool` is returned
directly.

Creates a new space sampler.

Sample `n` points from the search space or thin down a pool.

## Arguments

- id:

  (`character(1)`)  
  Identifier of the sampler.

- deterministic:

  (`logical(1)`)  
  Whether this sampler is marked as deterministic.

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Optional package names required by the sampler.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Configuration parameter set.

- label:

  (`character(1)`)  
  Label for the object.

- man:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page.

- n:

  (`integer(1)`)  
  Number of points to sample.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space describing the point columns.

- pool:

  (`NULL` \| `data.table`)  
  Optional candidate pool to sample from.

- known_pool:

  (`NULL` \| `data.table`)  
  Optional already-known points that may influence the sampler.

## Value

`data.table`.

## Fields

- `label`:

  (`character(1)`) Label for this object.

- `man`:

  (`character(1)`) String in the format `[pkg]::[topic]` pointing to a
  manual page.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Required
  packages.

- `deterministic`:

  (`logical(1)`) Whether the sampler is marked as deterministic.

## Dictionary

This class can be retrieved from
[mlr_space_samplers](https://celecx.mlr-org.com/reference/mlr_space_samplers.md)
via [`clx_sps()`](https://celecx.mlr-org.com/reference/clx_sps.md) and
[`clx_spss()`](https://celecx.mlr-org.com/reference/clx_spss.md).

## See also

Other SpaceSampler:
[`SpaceSamplerDistance`](https://celecx.mlr-org.com/reference/SpaceSamplerDistance.md),
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
