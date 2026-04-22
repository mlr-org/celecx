# Standardized Active Learning Distance

Distance for the greedy-sampling family of active learning methods.

Numeric and integer columns stay numeric, logical and factor columns are
one-hot encoded, and every geometry column is standardized to mean 0 and
standard deviation 1. With a finite pool, center and scale are estimated
from the candidate pool. With `xdt = NULL`, numeric centers and scales
are derived from finite search-space bounds under a uniform
distribution, and dummy-column centers and scales are derived from
uniform categorical levels.

Missing or dependency-inactive values are mapped to 0 in the
standardized geometry.

Creates a standardized active-learning distance.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[ALDistanceGeometry](https://mlr-org.github.io/celecx/reference/ALDistanceGeometry.md).

## Construction

    clx_ald("standardize")

## See also

Other ALDistance:
[`ALDistance`](https://mlr-org.github.io/celecx/reference/ALDistance.md),
[`ALDistanceGeometry`](https://mlr-org.github.io/celecx/reference/ALDistanceGeometry.md),
[`mlr_al_distances`](https://mlr-org.github.io/celecx/reference/mlr_al_distances.md),
[`mlr_al_distances_affine`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_affine.md),
[`mlr_al_distances_gower`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_gower.md)
