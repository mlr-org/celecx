# Surrogate Performance Callback

Evaluates a named
[OptimizerAL](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)
surrogate on a held-out regression task after every evaluated batch.

The callback stores one row per batch in `$data`. Rows include the
archive batch number, cumulative number of evaluations, timestamp,
surrogate id, and one column per configured regression measure.

Creates a new CallbackSurrogatePerformance.

Clears the logged performance history and the stored archive reference.

Build a
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
from the recorded archive and surrogate performance.

The resulting task has one row per archive evaluation. Its columns are
the archive search-space columns (role `archive_x`), the archive
codomain target columns (role `archive_y`), the `batch_nr` column (role
`feature`), and the selected per-batch surrogate-performance column
(role `target`). The archive's search space and codomain, the selected
regression measure, and (for pool-based runs) the candidate pool are
carried along for extrapolators that replay the active-learning loop.

## Arguments

- surrogate_id:

  (`character(1)`)  
  Surrogate registry id in `optimizer$surrogates`.

- task:

  ([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html))  
  Held-out regression task.

- measures:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Regression measures. Named lists use their names as output column
  names; unnamed measures use their measure ids.

- measure:

  (`character(1)` \| `NULL`)  
  Name of the measure column to use as task target. Refers to the
  `$state$measures` names. When `NULL` (default) and the callback was
  configured with exactly one measure, that measure is used.

- id:

  (`character(1)`)  
  Task id. Defaults to `"surrogate_performance"`.

- link:

  (`character(1)`)  
  Name of the predictive
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  for the resulting
  [TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
  `"identity"` by default.

- label:

  (`character(1)`)  
  Optional task label.

## Value

[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).

## Fields

- `data`:

  (`data.table`)  
  Logged performance history.

- `latest`:

  (`data.table` \| `NULL`)  
  Most recently logged row.

## Examples

``` r
if (FALSE) { # \dontrun{
perf <- clbk("celecx.surrogate_performance",
  surrogate_id = "model",
  task = test_task,
  measures = list(r2 = msr("regr.rsq"), mae = msr("regr.mae"))
)
} # }
```
