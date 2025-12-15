# Regret Metric

Returns the gap between the current best and the known optimum. Assumes
minimization; for maximization, negate both values.

## Usage

``` r
metric_regret(archive, surrogate = NULL, optimum, target = NULL, ...)
```

## Arguments

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))
  The archive.

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))
  Ignored.

- optimum:

  (`numeric(1)`) The known optimal value.

- target:

  (`character(1)`) Target column name.

- ...:

  Ignored.

## Value

`numeric(1)` The regret (best_y - optimum).
