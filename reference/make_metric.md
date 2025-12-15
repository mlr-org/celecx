# Make Metric Function

Creates a metric function with fixed parameters, useful for configuring
metrics that require specific settings.

## Usage

``` r
make_metric(metric_fun, ...)
```

## Arguments

- metric_fun:

  (`function`) Base metric function.

- ...:

  Fixed arguments to pass to the metric function.

## Value

A new metric function with the fixed arguments bound.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create RMSE metric with fixed test data
my_rmse <- make_metric(metric_model_rmse, test_data = my_test_dt, target = "y")

# Use in tracker
tracker <- MetricsTracker$new(metrics = list(rmse = my_rmse))
} # }
```
