# Model R-squared Metric

Returns the R-squared (coefficient of determination) of the surrogate's
predictions on test data.

## Usage

``` r
metric_model_r2(archive, surrogate = NULL, test_data, target, ...)
```

## Arguments

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))
  The archive (ignored for this metric).

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))
  The fitted surrogate model.

- test_data:

  (`data.table`) Test data with features and target column.

- target:

  (`character(1)`) Target column name in test_data.

- ...:

  Ignored.

## Value

`numeric(1)` The R-squared value.
