# Forecast Tracker Callback

Updates a `ForecastTracker` after each evaluated batch.

Creates a new CallbackForecastTracker.

## Arguments

- metrics_tracker:

  ([MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md))  
  Tracker containing the metrics history.

- forecast_tracker:

  (`ForecastTracker`)  
  Tracker to be updated.

## Details

This callback expects that a
[MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md)
callback has already logged the current batch in the same callback
chain.

## Fields

- `metrics_tracker`:

  ([MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md))

- `forecast_tracker`:

  (`ForecastTracker`)

## Examples

``` r
if (FALSE) { # \dontrun{
metrics_tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))
forecast_tracker <- ForecastTracker$new(
  extrapolator = CurveExtrapolatorParametric$new(),
  metric_name = "best_y"
)
callback <- clbk("celecx.forecast_tracker",
  metrics_tracker = metrics_tracker,
  forecast_tracker = forecast_tracker
)
} # }
```
