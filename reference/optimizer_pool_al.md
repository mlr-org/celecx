# Convenience Constructor for Pool-Based Active Learning Optimizers

Creates an
[OptimizerAL](https://celecx.mlr-org.com/reference/OptimizerAL.md) with
appropriate components for a specific active learning method.

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
  pool_size = NULL
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
  [OptimizerAL](https://celecx.mlr-org.com/reference/OptimizerAL.md)'s
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

- pool_size:

  (`NULL` \| `integer(1)`)  
  Optional number of candidate points to subsample uniformly before
  scoring. `NULL` keeps exhaustive pool scoring and does not enable
  continuous-space use.

## Value

A configured
[OptimizerAL](https://celecx.mlr-org.com/reference/OptimizerAL.md).
