# Run Active Learning

Convenience function that constructs an active learning
[OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md)
via
[`optimizer_active_learning()`](https://mlr-org.github.io/celecx/reference/optimizer_active_learning.md),
runs it on a bbotk instance, and (optionally) logs metrics via
[CallbackMetricsTracker](https://mlr-org.github.io/celecx/reference/celecx.metrics_tracker.md).

## Usage

``` r
optimize_active(
  objective,
  search_space = NULL,
  n_evals = NULL,
  terminator = NULL,
  metrics_tracker = NULL,
  forecast_tracker = NULL,
  forecast_terminator = NULL,
  callbacks = NULL,
  optimizer = NULL,
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

- n_evals:

  (`NULL` \| `integer(1)`)  
  Convenience evaluation budget used only if `terminator` is `NULL`.

- terminator:

  (`NULL` \|
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Terminator for the outer active learning loop. If `NULL`, a
  `trm("evals", n_evals = n_evals)` is constructed.

- metrics_tracker:

  (`NULL` \|
  [MetricsTracker](https://mlr-org.github.io/celecx/reference/MetricsTracker.md))  
  Optional metrics tracker. If provided, a
  [CallbackMetricsTracker](https://mlr-org.github.io/celecx/reference/celecx.metrics_tracker.md)
  is attached to the instance.

- forecast_tracker:

  (`NULL` \| `ForecastTracker`)  
  Optional forecast tracker. If provided, `CallbackForecastTracker` is
  attached after
  [CallbackMetricsTracker](https://mlr-org.github.io/celecx/reference/celecx.metrics_tracker.md).
  Requires `metrics_tracker`.

- forecast_terminator:

  (`NULL` \|
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Optional forecast-based terminator. If supplied, it is combined with
  the base terminator via `trm("combo", ..., any = TRUE)`.

- callbacks:

  (`NULL` \| [`list()`](https://rdrr.io/r/base/list.html) of
  [bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html))  
  Additional user callbacks. These are appended after internal
  callbacks.

- optimizer:

  (`NULL` \|
  [bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html))  
  Explicit optimizer to use. If `NULL`, constructs one via
  [`optimizer_active_learning()`](https://mlr-org.github.io/celecx/reference/optimizer_active_learning.md).
  Supply an optimizer from
  [`optimizer_pool_al()`](https://mlr-org.github.io/celecx/reference/optimizer_pool_al.md)
  to use paper-style active learning methods.

- ...:

  Passed to
  [`optimizer_active_learning()`](https://mlr-org.github.io/celecx/reference/optimizer_active_learning.md)
  when `optimizer` is `NULL`.

## Value

[`list()`](https://rdrr.io/r/base/list.html) with:

- `instance`:
  [SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md)

- `optimizer`: configured optimizer

- `metrics_tracker`: the passed tracker (or `NULL`)
