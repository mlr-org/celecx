# Build an LCE Task from an Archive and a Performance Table

Builds a
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md) by
joining an optimization archive with a per-batch performance table: one
task row per archive evaluation, with the selected performance column as
target. This is the assembly used by
[CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md)`$task()`
and
[`replay_surrogate_performance()`](https://mlr-org.github.io/celecx/dev/reference/replay_surrogate_performance.md);
it is exported for benchmark harnesses that record archives and curves
separately and rebuild tasks later.

## Usage

``` r
task_lce_from_perf(
  archive,
  perf,
  measure,
  measure_obj = NULL,
  pool = NULL,
  link = "identity",
  id,
  label = NA_character_
)
```

## Arguments

- archive:

  ([bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html))  
  Archive of the originating run; supplies the `archive_x` / `archive_y`
  columns, `batch_nr`, and the search space / codomain provenance.

- perf:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Per-batch performance table with a `batch_nr` column plus one column
  per measure. Archive batches without a matching `perf` row are
  dropped.

- measure:

  (`character(1)`)  
  Name of the `perf` column to use as task target.

- measure_obj:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) \|
  `NULL`)  
  The regression measure that produced the target column; carried into
  the task so replay extrapolators can score on the same scale.

- pool:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| `NULL`)  
  Candidate pool of the originating run (search-space columns only), for
  pool-restricted replay.

- link:

  (`character(1)`)  
  Name of the predictive
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  for the resulting task.

- id:

  (`character(1)`)  
  Task id.

- label:

  (`character(1)`)  
  Optional task label.

## Value

[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
