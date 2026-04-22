# Geometry-Based Active Learning Distance

Abstract base class for active-learning distances based on Euclidean
distances in a geometry space.

Subclasses implement how the pool or search space determines
geometry-space scaling and how reference and query points are
transformed into that same space.

Subclasses must implement the following methods:

- `private$.fit_geometry(xdt, search_space)`: Fit the geometry from the
  pool and search space, or from the search space only when `xdt` is
  `NULL`. The result must contain an integer `dimension` giving the
  number of geometry columns.

- `private$.transform(xdt, state)`: Transform query points into the
  geometry space. The result must be a matrix with one row per query
  point and `state$dimension` columns.

## Fields

- `dimension`:

  (`integer(1)` \| `NULL`) Dimension of the fitted geometry space.

- `reference_embedding`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html) \| `NULL`) Current
  reference points transformed into the fitted geometry space. `NULL` if
  the distance has not been fitted or reference points have not been
  set.

## See also

Other ALDistance:
[`ALDistance`](https://celecx.mlr-org.com/reference/ALDistance.md),
[`mlr_al_distances`](https://celecx.mlr-org.com/reference/mlr_al_distances.md),
[`mlr_al_distances_affine`](https://celecx.mlr-org.com/reference/mlr_al_distances_affine.md),
[`mlr_al_distances_gower`](https://celecx.mlr-org.com/reference/mlr_al_distances_gower.md),
[`mlr_al_distances_standardize`](https://celecx.mlr-org.com/reference/mlr_al_distances_standardize.md)
