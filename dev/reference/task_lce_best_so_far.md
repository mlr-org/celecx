# Best-So-Far Optimization Trace as an LCE Task

Builds a
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
whose target is the best objective value observed up to (and including)
each archive batch – the progress curve of an optimization run. This is
the optimization-mode counterpart of the surrogate-performance tasks
built by
[CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md)
and
[`replay_surrogate_performance()`](https://mlr-org.github.io/celecx/dev/reference/replay_surrogate_performance.md):
the same LCE learners, measures, and resamplings apply, forecasting
future best-so-far values instead of future model quality.

The task has one row per archive evaluation, carrying the archive's
feature and target columns in the `archive_x` / `archive_y` roles. The
target column `best_so_far` is constant within a batch (batches are
evaluated as a whole, so mid-batch improvements only become visible at
the batch's end). The optimization direction is taken from the archive
codomain's single minimize/maximize-tagged target and travels with the
task via its stored codomain; direction-dependent operations (e.g. the
`"target_reached"` predict type,
[`lce_batches_to_target()`](https://mlr-org.github.io/celecx/dev/reference/lce_batches_to_target.md))
work without a task measure.

## Usage

``` r
task_lce_best_so_far(
  archive,
  link = "identity",
  id = "best_so_far",
  label = NA_character_
)
```

## Arguments

- archive:

  ([bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html))  
  Archive of a completed single-target optimization run. Its codomain
  target must be tagged `"minimize"` or `"maximize"` (a `"learn"`-tagged
  target has no best value).

- link:

  (`character(1)`)  
  Name of the predictive
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  for the resulting task. `"identity"` by default; note that objective
  values are in general not sign-constrained, so non-identity links only
  make sense for suitably bounded objectives.

- id:

  (`character(1)`)  
  Task id. Defaults to `"best_so_far"`.

- label:

  (`character(1)`)  
  Optional task label.

## Value

[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
