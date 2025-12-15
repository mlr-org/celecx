# Integrated Variance Metric

Approximates the integral of prediction variance over the domain. This
is the continuous version of mean variance, accounting for domain
volume.

## Usage

``` r
metric_integrated_variance(
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

`numeric(1)` The integrated variance.

## Details

For a uniform grid, this equals mean_variance \* domain_volume. More
sophisticated integration (e.g., Monte Carlo) may be added.
