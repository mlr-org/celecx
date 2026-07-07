# Learning Curve Extrapolation Task

Task class for predicting the surrogate-model quality trajectory of an
active learning run. Each row corresponds to one archive evaluation and
carries the batch in which it was evaluated, the untransformed archive
feature values, the archive target values, and the surrogate's
performance for that batch.

The task has a single feature, `batch_nr`, which is the only column
passed to a
[LearnerLCE](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md)
at predict time. The archive feature and target columns are carried in
the dedicated column roles `archive_x` and `archive_y` so that more
sophisticated extrapolators may inspect them during training without the
columns being required for prediction on new data.

Optionally, the task also carries the
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
search space and the
[bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html) of
the originating optimization run. These cannot be recovered from the
archive columns alone (which only retain storage types), but they are
required by extrapolators that replay the active-learning loop, since
they define parameter bounds, transformations, dependencies, and the
codomain optimization directions.

Two further pieces of run information can be carried for replay
extrapolators: the regression
[mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) that
produced the performance `target` column (so a simulation can score on
the same scale) and, for finite-pool active learning, the candidate
`pool` (so a simulation can propose from the same candidate set). The
originating surrogate is *not* stored here, as it is a property of the
optimizer rather than of the data.

`TaskLCE` objects are typically constructed by tracking an
[`OptimizerAL`](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)
optimization run with a
[`CallbackSurrogatePerformance`](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md)
callback.

Creates a new LCE task.

True surrogate performance for the given rows (defaults to all active
rows).

Returns the archive feature data (columns with role `archive_x`).

Returns the archive target data (columns with role `archive_y`).

## Arguments

- id:

  (`character(1)`)  
  Task id.

- backend:

  ([mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
  \| [`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Data backend. Coerced via
  [`mlr3::as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.html).

- target:

  (`character(1)`)  
  Name of the numeric column holding the surrogate performance.

- batch_nr:

  (`character(1)`)  
  Name of the integer column holding the batch number. This column
  becomes the task's single feature.

- archive_x:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of the archive feature columns (untransformed `x` values). At
  least one column is required at construction.

- archive_y:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of the archive target columns (objective `y` values). At least
  one column is required at construction.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`)  
  Optional search space of the originating optimization run. Its
  parameter ids must match `archive_x`. Cloned and stored for replay
  extrapolators.

- codomain:

  ([bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html)
  \| `NULL`)  
  Optional codomain of the originating optimization run. Its target ids
  must match `archive_y`. Cloned and stored for replay extrapolators.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) \|
  `NULL`)  
  Optional regression measure that produced the performance `target`
  column. Cloned and stored so replay extrapolators can score on the
  same scale.

- pool:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| `NULL`)  
  Finite candidate pool of the originating run, for pool-based active
  learning. Its columns must be exactly `archive_x` (the search-space
  parameter ids). `NULL` for continuous search spaces.

- link:

  (`character(1)`)  
  Name of the predictive
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  on whose scale the learners model the performance curve and the
  distributional measures interpret `se`. A property of the target
  metric; `"identity"` by default. See
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  (and
  [lce_link_from_range](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  to derive one from a measure's range).

- label:

  (`character(1)`)  
  Optional label.

- extra_args:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Extra constructor arguments preserved for cloning.

- rows:

  ([`integer()`](https://rdrr.io/r/base/integer.html))

## Value

[`numeric()`](https://rdrr.io/r/base/numeric.html).

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

## Fields

- `batch_nr`:

  (`character(1)`)  
  Name of the batch_nr column.

- `archive_x`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of the archive feature columns.

- `archive_y`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of the archive target columns.

- `batch_nrs`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Batch number for every active row, in the order of `task$row_ids`.

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`)  
  Search space of the originating optimization run.

- `codomain`:

  ([bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html)
  \| `NULL`)  
  Codomain of the originating optimization run.

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) \|
  `NULL`)  
  Regression measure that produced the performance `target` column.

- `pool`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| `NULL`)  
  Finite candidate pool of the originating run.

- `link`:

  (`character(1)`)  
  Name of the predictive link scale (see
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)).
