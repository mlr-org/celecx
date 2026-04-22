# Active Learning Optimizer Factory

Convenience constructor that wires together an
[OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md)
for uncertainty-based active learning with optional multipoint proposal
heuristics.

## Usage

``` r
optimizer_active_learning(
  learner,
  se_method = c("auto", "bootstrap", "quantile"),
  n_bootstrap = 30L,
  batch_size = 1L,
  multipoint_method = c("greedy", "local_penalization", "diversity", "constant_liar"),
  acq_optimizer = opt("random_search"),
  acq_evals = 100L
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
    [LearnerRegrBootstrapSE](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.bootstrap_se.md).

  - `"quantile"`: wrap via
    [LearnerRegrQuantileSE](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.quantile_se.md)
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

- acq_optimizer:

  ([bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
  \|
  [mlr3mbo::AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html))  
  Optimizer used to choose the candidate-generation strategy for
  acquisition scoring. The current implementation translates common
  optimizers to a
  [SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md)
  and ignores optimizer-specific search logic.

- acq_evals:

  (`integer(1)`)  
  Number of candidate points scored per proposal round.

## Value

Configured
[OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md).

## Details

This helper builds an active-learning optimizer around:

- an uncertainty acquisition function (`"sd"`)

- a surrogate that can provide standard errors (either native `"se"`,
  [LearnerRegrBootstrapSE](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.bootstrap_se.md),
  or
  [LearnerRegrQuantileSE](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.quantile_se.md))

- proposer-based batch construction via
  [ALProposerScore](https://mlr-org.github.io/celecx/reference/ALProposerScore.md),
  [ALProposerSequentialScore](https://mlr-org.github.io/celecx/reference/ALProposerSequentialScore.md),
  or
  [ALProposerPseudoLabel](https://mlr-org.github.io/celecx/reference/ALProposerPseudoLabel.md)

`acq_evals` controls the size of the candidate pool scored in each
proposal round. For continuous search spaces, candidates are sampled
from the search space using a coarse translation of `acq_optimizer` to a
[SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md).
For finite pools, the same sampler is applied to the remaining pool.
