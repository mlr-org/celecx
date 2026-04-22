# Gower Active Learning Distance

Distance for mixed-type active-learning search spaces.

Numeric and integer columns contribute absolute differences, optionally
rescaled according to the `scale` hyperparameter. Logical and factor
columns contribute `0` for equal values and `1` for different values.
The final distance is the average across all observed per-column
contributions.

Numeric columns with zero scale are ignored. Missing values are excluded
from the pairwise average for the affected column.

Creates a new Gower active-learning distance.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md).

## Construction

    clx_ald("gower")

The configuration hyperparameter `scale` controls numeric scaling:

- `"minmax_auto"` divides by the observed pool range when a pool is
  supplied, and by the finite search-space range when fitted with
  `xdt = NULL`.

- `"minmax_space"` divides by the search-space range.

- `"minmax_empirical"` divides by the observed pool range.

- `"standardize"` divides by the observed pool standard deviation.

- `"off"` leaves numeric differences unscaled.

## See also

Other ALDistance:
[`ALDistance`](https://celecx.mlr-org.com/reference/ALDistance.md),
[`ALDistanceGeometry`](https://celecx.mlr-org.com/reference/ALDistanceGeometry.md),
[`mlr_al_distances`](https://celecx.mlr-org.com/reference/mlr_al_distances.md),
[`mlr_al_distances_affine`](https://celecx.mlr-org.com/reference/mlr_al_distances_affine.md),
[`mlr_al_distances_standardize`](https://celecx.mlr-org.com/reference/mlr_al_distances_standardize.md)
