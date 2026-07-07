# Offline Surrogate-Performance Replay

Reconstructs the surrogate-performance learning curve of a finished
active-learning run, offline, from its
[bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html).
For every batch the surrogate is refit on the archive data available up
to that batch and scored on a held-out regression task, yielding one
performance value per batch and measure. The result is assembled into a
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md),
the same shape produced online by
[CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md).

This is the offline twin of
[CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md):
the callback scores the surrogate the optimizer maintains during the
run, whereas this function replays the run from a stored archive,
refitting `learner` itself.

## Usage

``` r
replay_surrogate_performance(
  archive,
  learner,
  task,
  measures = list(msr("regr.rsq"), msr("regr.mae")),
  measure = NULL,
  pool = NULL,
  link = "identity",
  id = "surrogate_performance",
  label = NA_character_
)
```

## Arguments

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))  
  Archive of a finished single-target run.

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  \|
  [mlr3mbo::SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.html))  
  Surrogate model refit per batch. A plain regression learner is wrapped
  in a
  [mlr3mbo::SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.html)
  so the fit / predict / output-transform path matches a live run; a
  surrogate is used as given (cloned). Either way the object is cloned,
  so the caller's model is left untrained.

- task:

  ([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html))  
  Held-out regression task to score on. Must contain the archive's
  search-space columns as features.

- measures:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Regression measures. Named entries use their names as
  performance-column names; unnamed entries use the measure id.

- measure:

  (`character(1)` \| `NULL`)  
  Name of the measure to use as the
  [TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
  target. When `NULL` (default) and a single measure is given, that
  measure is used.

- pool:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| `NULL`)  
  Finite candidate pool of the originating run, carried into the
  resulting
  [TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
  for pool-based replay. Its columns must be the archive's search-space
  ids. `NULL` (default) for continuous runs.

- link:

  (`character(1)`)  
  Name of the predictive
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  for the resulting
  [TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
  `"identity"` by default; pass e.g. `"log"` for a non-negative loss
  metric (see
  [lce_link_from_range](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)).

- id:

  (`character(1)`)  
  Task id.

- label:

  (`character(1)`)  
  Optional task label.

## Value

[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
