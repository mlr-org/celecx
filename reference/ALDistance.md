# Active Learning Distance Base Class

Abstract base class for active-learning distances.

An `ALDistance` is fitted on a candidate pool or, when the `xdt` passed
to `$fit_pool()` is `NULL`, on the search space itself. Concrete
distance classes support search-space fitting where the search-space
bounds define the scaling or encoding; classes that need empirical pool
information throw an error for `NULL` pools. Reference points are set
separately before distances are computed.

Creates a new active-learning distance.

Fit the distance on a finite candidate pool or on the search space.

Set the reference points used by `distances()`.

Compute distances from query points to reference points indexed by `i`.

Clear all fitted state.

## Arguments

- id:

  (`character(1)`)  
  Identifier of the distance.

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Optional package names required by the distance.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Configuration parameter set.

- label:

  (`character(1)`)  
  Label for the object.

- man:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space describing the candidate-pool columns.

- xdt:

  (`NULL` \| `data.table`)  
  Configurations in search-space coordinates. For `fit_pool()` only,
  `NULL` fits the distance to the search space itself where supported.

- i:

  (`NULL` \| `integerish()`)  
  Optional subset of reference-point indices. `NULL` uses all reference
  points.

## Value

`self`.

`self`.

Numeric matrix with one row per query point and one column per selected
reference point.

`self`.

## Fields

- `label`:

  (`character(1)`) Label for this object.

- `man`:

  (`character(1)`) String in the format `[pkg]::[topic]` pointing to a
  manual page.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Required
  packages.

- `is_fitted`:

  (`logical(1)`) Whether the distance was fitted on a pool or search
  space.

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`) Search space used during fitting.

- `n_pool`:

  (`integer(1)` \| `NULL`) Number of pool points. `NULL` if the distance
  is not fitted or was fitted to the search space without a finite pool.

- `n_reference_points`:

  (`integer(1)` \| `NULL`) Number of reference points set for distance
  computation.

- `state`:

  (`any`) Fitted state returned by the subclass.

## Dictionary

This class can be retrieved from
[mlr_al_distances](https://celecx.mlr-org.com/reference/mlr_al_distances.md)
via [`clx_ald()`](https://celecx.mlr-org.com/reference/clx_ald.md) and
[`clx_alds()`](https://celecx.mlr-org.com/reference/clx_alds.md).

## See also

Other ALDistance:
[`ALDistanceGeometry`](https://celecx.mlr-org.com/reference/ALDistanceGeometry.md),
[`mlr_al_distances`](https://celecx.mlr-org.com/reference/mlr_al_distances.md),
[`mlr_al_distances_affine`](https://celecx.mlr-org.com/reference/mlr_al_distances_affine.md),
[`mlr_al_distances_gower`](https://celecx.mlr-org.com/reference/mlr_al_distances_gower.md),
[`mlr_al_distances_standardize`](https://celecx.mlr-org.com/reference/mlr_al_distances_standardize.md)
