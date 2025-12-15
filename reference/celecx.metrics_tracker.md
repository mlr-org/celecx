# Metrics Tracker Callback

Logs metrics after each batch evaluation using a
[MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md).

This callback is meant to be passed as `callbacks` to a bbotk
[bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
/
[bbotk::OptimInstanceAsync](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html)
(or via the `callbacks` argument of
[`oi()`](https://bbotk.mlr-org.com/reference/oi.html) /
[`bb_optimize()`](https://bbotk.mlr-org.com/reference/bb_optimize.html)).

Creates a new CallbackMetricsTracker.

## Arguments

- metrics_tracker:

  ([MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md))  
  Tracker used for logging.

## Fields

- `metrics_tracker`:

  ([MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md))  
  Tracker that collects per-batch metrics.

## Examples

``` r
if (FALSE) { # \dontrun{
tracker <- MetricsTracker$new()
callback <- clbk("celecx.metrics_tracker", metrics_tracker = tracker)
} # }
```
