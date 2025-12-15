# Model RMSE Metric

Returns the root mean squared error of the surrogate's predictions on
test data.

## Usage

``` r
metric_model_rmse(archive, surrogate = NULL, test_data, target, ...)
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

`numeric(1)` The RMSE.
