# Offline Surrogate-Performance Table

The scoring core of
[`replay_surrogate_performance()`](https://mlr-org.github.io/celecx/reference/replay_surrogate_performance.md):
refits `learner` on every archive batch prefix, scores it on the
held-out `task`, and returns the per-batch performance table with one
row per batch and one column per measure – the same shape
[CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/reference/celecx.surrogate_performance.md)
records online (columns `batch_nr`, `n_evals`, then the measure
columns). Use this when the full multi-measure table is wanted rather
than a single-target
[TaskLCE](https://mlr-org.github.io/celecx/reference/TaskLCE.md).

## Usage

``` r
replay_surrogate_perf_table(
  archive,
  learner,
  task,
  measures = list(msr("regr.rsq"), msr("regr.mae"))
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
  Surrogate model refit per batch; see
  [`replay_surrogate_performance()`](https://mlr-org.github.io/celecx/reference/replay_surrogate_performance.md).

- task:

  ([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html))  
  Held-out regression task to score on.

- measures:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Regression measures; named entries use their names as column names.

## Value

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `batch_nr`, `n_evals`, and one numeric column per measure.
