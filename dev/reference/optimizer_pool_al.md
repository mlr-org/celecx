# Convenience Constructor for Pool-Based Active Learning Optimizers

Creates an
[OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)
with appropriate components for a specific active learning method.

## Usage

``` r
optimizer_pool_al(
  method = c("gsx", "gsy", "igs", "qbc", "random", "ideal"),
  learner = NULL,
  delta = 1,
  n_init = NULL,
  init_method = NULL,
  k_qbc = 5L,
  batch_size = 1L,
  n_candidates = NULL,
  distance = NULL
)
```

## Arguments

- method:

  (`character(1)`)  
  One of `"gsx"`, `"gsy"`, `"igs"`, `"qbc"`, `"random"`, `"ideal"`.

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  \| `NULL`)  
  Regression learner. Required for `"gsy"`, `"igs"`, `"qbc"`, `"ideal"`.

- delta:

  (`numeric(1)`)  
  Exploration weight for IDEAL (default 1).

- n_init:

  (`integer(1)` \| `NULL`)  
  Number of initial samples. `NULL` uses
  [OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)'s
  default initialization policy.

- init_method:

  (`character(1)` \| `NULL`)  
  Optional initialization override: `"gsx"`, `"random"`, or `"kmeans"`.
  `NULL` keeps the method-specific default (`"kmeans"` for IDEAL,
  `"gsx"` for GSx/GSy/iGS, `"random"` for random/QBC).

- k_qbc:

  (`integer(1)`)  
  Number of QBC committee members (default 5).

- batch_size:

  (`integer(1)`)  
  Points per iteration (default 1).

- n_candidates:

  (`NULL` \| `integer(1)`)  
  Optional number of candidate points to subsample uniformly before
  scoring. `NULL` keeps exhaustive pool scoring and does not enable
  continuous-space use.

- distance:

  (`character(1)` \|
  [ALDistance](https://mlr-org.github.io/celecx/dev/reference/ALDistance.md)
  \| `NULL`)  
  Distance used by every distance-based component (the GSx / iGS / IDEAL
  acquisition functions and the `"gsx"` / `"kmeans"` initializations).
  `NULL` (default) keeps the papers' method-specific scalings
  (standardization; per-dimension affine for IDEAL), which support
  numeric search spaces only. For mixed-type pools pass `"gower"` (a
  [mlr_al_distances](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances.md)
  key) or an
  [ALDistance](https://mlr-org.github.io/celecx/dev/reference/ALDistance.md)
  object. A `"kmeans"` initialization combined with a non-geometry
  distance (such as Gower) uses the medoid-based
  [SpaceSamplerKMedoids](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_kmedoids.md)
  instead, its mixed-type analogue. Note that on mixed-type pools the
  surrogate `learner` must support `character` features (e.g.
  `lrn("regr.ranger")`), since archives store
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  factor parameters as character columns.

## Value

A configured
[OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md).
