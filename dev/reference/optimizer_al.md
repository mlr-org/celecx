# Active Learning Optimizer Factory

Convenience constructor that wires together an
[OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)
for uncertainty-based active learning with optional multipoint proposal
heuristics.

## Usage

``` r
optimizer_al(
  learner,
  se_method = c("auto", "bootstrap", "quantile"),
  n_bootstrap = 30L,
  batch_size = 1L,
  multipoint_method = c("greedy", "local_penalization", "diversity", "constant_liar"),
  candidate_sampler = NULL,
  n_candidates = 100L,
  n_init = NULL
)
```

## Arguments

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Base regression learner used as the surrogate.

- se_method:

  (`character(1)`)  
  How to obtain standard errors:

  - `"auto"`: use native `"se"` if supported by `learner`, otherwise
    `"bootstrap"`.

  - `"bootstrap"`: wrap via
    [LearnerRegrBootstrapSE](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.bootstrap_se.md).

  - `"quantile"`: wrap via
    [LearnerRegrQuantileSE](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.quantile_se.md)
    (requires `"quantiles"` support).

- n_bootstrap:

  (`integer(1)`)  
  Number of bootstrap replicates for `"bootstrap"`. Ignored otherwise.

- batch_size:

  (`integer(1)`)  
  Number of points proposed per active-learning iteration.

- multipoint_method:

  (`character(1)`)  
  Batch selection strategy:

  - `"greedy"`: top-k by acquisition score

  - `"local_penalization"`: sequential local-penalization heuristic

  - `"diversity"`: sequential score/diversity trade-off

  - `"constant_liar"`: sequential pseudo-label batching

- candidate_sampler:

  (`NULL` \|
  [SpaceSampler](https://mlr-org.github.io/celecx/dev/reference/SpaceSampler.md))  
  Sampler that draws the scored candidates from a continuous search
  space. `NULL` uses
  [SpaceSamplerUniform](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_uniform.md).

- n_candidates:

  (`integer(1)`)  
  Number of candidate points scored per proposal round.

- n_init:

  (`NULL` \| `integer(1)`)  
  Number of initial evaluations. `NULL` uses
  [OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)'s
  default initialization policy (`4 * d` for fresh runs, none when the
  archive is already populated).

## Value

Configured
[OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md).

## Details

This helper builds an active-learning optimizer around:

- an uncertainty acquisition function (`"sd"`)

- a surrogate (registered under id `"model"`) that can provide standard
  errors (either native `"se"`,
  [LearnerRegrBootstrapSE](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.bootstrap_se.md),
  or
  [LearnerRegrQuantileSE](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.quantile_se.md))

- proposer-based batch construction via
  [ALProposerScore](https://mlr-org.github.io/celecx/dev/reference/ALProposerScore.md),
  [ALProposerSequentialScore](https://mlr-org.github.io/celecx/dev/reference/ALProposerSequentialScore.md),
  or
  [ALProposerPseudoLabel](https://mlr-org.github.io/celecx/dev/reference/ALProposerPseudoLabel.md)

`n_candidates` controls the size of the candidate pool scored in each
proposal round. For continuous search spaces, candidates are drawn from
the search space by `candidate_sampler`; for finite pools, candidates
are subsampled uniformly from the remaining pool.
