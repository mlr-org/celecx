# Distance-Aware Acquisition Function Base Class

Abstract base class for distance-aware active-learning acquisition
functions.

The acquisition constants include a detached copy of the wrapped
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md)
parameter set under the `al_distance` prefix. During update and
evaluation, those values are copied into the distance before reference
points are set or distances are computed.

`$fit_pool()` fits the distance on the candidate pool used for scaling
and pool-derived acquisition state, or on the search space when `xdt` is
`NULL`. `$update()` then sets the distance reference points to the
current archive rows in archive order.

Creates a new distance-aware acquisition function.

Fits the wrapped distance and any subclass pool state on a finite
candidate pool or on the search space.

Sets the distance reference points to the current surrogate archive.

## Arguments

- id:

  (`character(1)`)  
  Acquisition function id.

- constants:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Changeable constants or parameters of the acquisition function.

- surrogate:

  (`NULL` \|
  [mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))  
  Surrogate whose predictions are used in the acquisition function.

- al_distance:

  (`NULL` \|
  [ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md))  
  Distance object used by the acquisition function.

- requires_predict_type_se:

  (`logical(1)`)  
  Whether the surrogate must use predict type `"se"`.

- surrogate_class:

  (`character(1)`)  
  Allowed class of the surrogate. Default `"Surrogate"`.

- direction:

  (`"same"` \| `"minimize"` \| `"maximize"`)  
  Optimization direction of the acquisition function.

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required packages.

- label:

  (`character(1)`)  
  Label for the object.

- man:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page.

- xdt:

  (`NULL` \| `data.table`)  
  Candidate configurations in search-space coordinates. `NULL` fits to
  the search space where supported.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space describing the candidate-pool columns.

## Value

`self`.

## Fields

- `al_distance`:

  (`NULL` \|
  [ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md))
  Distance object used by the acquisition function.

## See also

Other Acquisition Function:
[`AcqFunctionDistGSx`](https://celecx.mlr-org.com/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIDEAL`](https://celecx.mlr-org.com/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionDistIGS`](https://celecx.mlr-org.com/reference/AcqFunctionDistIGS.md),
[`AcqFunctionGSy`](https://celecx.mlr-org.com/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://celecx.mlr-org.com/reference/SurrogateNull.md)
