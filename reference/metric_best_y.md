# Best Y Metric

Returns the best (minimum) observed y value in the archive.

## Usage

``` r
metric_best_y(archive, surrogate = NULL, target = NULL, ...)
```

## Arguments

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))
  The archive.

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))
  Ignored.

- target:

  (`character(1)`) Target column name. If NULL, uses first codomain
  target.

- ...:

  Ignored.

## Value

`numeric(1)` The minimum y value.
