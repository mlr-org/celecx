# Affine Active Learning Distance

Distance for IDEAL-style active learning methods.

All search-space columns must be numeric or integer. Each geometry
column is affine-scaled to the interval \\\[-1, 1\]\\. With a finite
pool, scaling uses the candidate-pool minima and maxima. With
`xdt = NULL`, scaling uses the finite search-space bounds. Zero-range
columns and missing values are mapped to 0 in the geometry.

Creates an affine active-learning distance.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[ALDistanceGeometry](https://mlr-org.github.io/celecx/reference/ALDistanceGeometry.md).

## Construction

    clx_ald("affine")

## See also

Other ALDistance:
[`ALDistance`](https://mlr-org.github.io/celecx/reference/ALDistance.md),
[`ALDistanceGeometry`](https://mlr-org.github.io/celecx/reference/ALDistanceGeometry.md),
[`mlr_al_distances`](https://mlr-org.github.io/celecx/reference/mlr_al_distances.md),
[`mlr_al_distances_gower`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_gower.md),
[`mlr_al_distances_standardize`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_standardize.md)
