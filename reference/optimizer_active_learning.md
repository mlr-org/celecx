# Active Learning Optimizer Factory

Convenience constructor that wires together an
[mlr3mbo::OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.html)
for active learning with an uncertainty-based acquisition function (SD)
and optional multipoint (batch) proposal via
[BatchProposer](https://celecx.mlr-org.com/reference/BatchProposer.md)
batch strategies.

## Usage

``` r
optimizer_active_learning(
  learner,
  se_method = c("auto", "bootstrap", "quantile"),
  se_method_n_bootstrap = 30L,
  batch_size = 1L,
  multipoint_method = c("greedy", "local_penalization", "diversity", "constant_liar"),
  aqf_optimizer = opt("pool"),
  aqf_evals = 100L
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
    [LearnerRegrBootstrapSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.bootstrap_se.md).

  - `"quantile"`: wrap via
    [LearnerRegrQuantileSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.quantile_se.md)
    (requires `"quantiles"` support).

- se_method_n_bootstrap:

  (`integer(1)`)  
  Number of bootstrap replicates for `"bootstrap"`. Ignored otherwise.

- batch_size:

  (`integer(1)`)  
  Number of points proposed per MBO iteration.

- multipoint_method:

  (`character(1)`)  
  Batch selection strategy:

  - `"greedy"`: top-k by acquisition score (equivalent to
    `n_candidates = batch_size`)

  - `"local_penalization"`: local penalization based diversity

  - `"diversity"`: score/diversity trade-off

  - `"constant_liar"`: uses
    [mlr3mbo::bayesopt_mpcl](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_mpcl.html)
    (constant liar loop)

- aqf_optimizer:

  ([bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
  \|
  [mlr3mbo::AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html))  
  Acquisition optimization backend. If an
  [bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
  is provided, it is wrapped into a
  [BatchProposer](https://celecx.mlr-org.com/reference/BatchProposer.md).
  Default is `opt("pool")`.

- aqf_evals:

  (`integer(1)`)  
  Evaluation budget for acquisition optimization
  (`TerminatorEvals$n_evals`).

## Value

Configured
[mlr3mbo::OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.html).

## Details

This helper focuses on the common active learning setup in this project:

- uses an SD acquisition function
  ([mlr3mbo::AcqFunctionSD](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.html))

- uses a surrogate that can provide standard errors (either native
  `"se"`,
  [LearnerRegrBootstrapSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.bootstrap_se.md),
  or
  [LearnerRegrQuantileSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.quantile_se.md))

- disables assigning a "best" point via
  [ResultAssignerNull](https://celecx.mlr-org.com/reference/ResultAssignerNull.md)
  (required for codomains tagged with `"learn"`)
