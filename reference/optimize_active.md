# Run Active Learning

Convenience function that constructs an active learning
[mlr3mbo::OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.html)
via
[`optimizer_active_learning()`](https://celecx.mlr-org.com/reference/optimizer_active_learning.md),
runs it on a bbotk instance, and (optionally) logs metrics via
[CallbackMetricsTracker](https://celecx.mlr-org.com/reference/celecx.metrics_tracker.md).

## Usage

``` r
optimize_active(
  objective,
  search_space = NULL,
  term_evals = NULL,
  terminator = NULL,
  metrics_tracker = NULL,
  ...
)
```

## Arguments

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  Objective to evaluate. Typically has a single codomain target tagged
  `"learn"`.

- search_space:

  (`NULL` \|
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Optional restricted search space. If `NULL`, the search space is
  derived from `objective$domain` (same logic as bbotk's
  `OptimInstanceBatch`).

- term_evals:

  (`NULL` \| `integer(1)`)  
  Convenience evaluation budget used only if `terminator` is `NULL`.

- terminator:

  (`NULL` \|
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Terminator for the outer active learning loop. If `NULL`, a
  `trm("evals", n_evals = term_evals)` is constructed.

- metrics_tracker:

  (`NULL` \|
  [MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md))  
  Optional metrics tracker. If provided, a
  [CallbackMetricsTracker](https://celecx.mlr-org.com/reference/celecx.metrics_tracker.md)
  is attached to the instance.

- ...:

  Passed to
  [`optimizer_active_learning()`](https://celecx.mlr-org.com/reference/optimizer_active_learning.md).

## Value

[`list()`](https://rdrr.io/r/base/list.html) with:

- `instance`:
  [SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md)

- `optimizer`: configured
  [mlr3mbo::OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.html)

- `metrics_tracker`: the passed tracker (or `NULL`)
