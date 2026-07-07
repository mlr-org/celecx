# Run Active Learning

Convenience function that constructs an active learning
[OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)
via
[`optimizer_al()`](https://mlr-org.github.io/celecx/dev/reference/optimizer_al.md)
and runs it on a
[SearchInstance](https://mlr-org.github.io/celecx/dev/reference/SearchInstance.md).

## Usage

``` r
optimize_active(
  objective,
  search_space = NULL,
  n_evals = NULL,
  terminator = NULL,
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

- callbacks:

  (`NULL` \| [`list()`](https://rdrr.io/r/base/list.html) of
  [bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html))  
  Callbacks attached to the instance, e.g. a
  [CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md)
  for per-batch surrogate tracking.

- optimizer:

  (`NULL` \|
  [bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html))  
  Explicit optimizer to use. If `NULL`, constructs one via
  [`optimizer_al()`](https://mlr-org.github.io/celecx/dev/reference/optimizer_al.md).
  Supply an optimizer from
  [`optimizer_pool_al()`](https://mlr-org.github.io/celecx/dev/reference/optimizer_pool_al.md)
  to use paper-style active learning methods.

- ...:

  Passed to
  [`optimizer_al()`](https://mlr-org.github.io/celecx/dev/reference/optimizer_al.md)
  when `optimizer` is `NULL`.

## Value

[`list()`](https://rdrr.io/r/base/list.html) with:

- `instance`:
  [SearchInstance](https://mlr-org.github.io/celecx/dev/reference/SearchInstance.md)

- `optimizer`: configured optimizer
