# Simple Regret Metric

Alias for
[metric_regret](https://mlr-org.github.io/celecx/reference/metric_regret.md).

## Usage

``` r
metric_simple_regret(archive, surrogate = NULL, optimum, target = NULL, ...)
```

## Arguments

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))  
  The archive.

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))  
  Ignored.

- optimum:

  (`numeric(1)`)  
  The known optimal value.

- target:

  (`character(1)` \| `NULL`)  
  Target column name.

- ...:

  Ignored.
