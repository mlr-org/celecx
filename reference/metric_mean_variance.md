# Mean Variance Metric

Returns the average prediction variance of the surrogate over an
evaluation grid. Measures overall model uncertainty.

## Usage

``` r
metric_mean_variance(
  archive,
  surrogate = NULL,
  grid = NULL,
  search_space = NULL,
  ...
)
```

## Arguments

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))
  The archive (may be used for normalization).

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))
  The fitted surrogate model.

- grid:

  (`data.table`) Points at which to evaluate variance. If NULL,
  generates a default grid from the search space.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  Used to generate grid if not provided.

- ...:

  Ignored.

## Value

`numeric(1)` The mean of se^2 over the grid.
