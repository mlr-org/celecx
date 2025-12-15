# Create Metrics Tracker

Convenience constructor for
[MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md)
with commonly used metrics.

## Usage

``` r
metrics_tracker(goal = c("optimize", "learn", "both"), metrics = list(), ...)
```

## Arguments

- goal:

  (`character(1)`)  
  Either `"optimize"`, `"learn"`, or `"both"`. Determines which default
  metrics to include.

- metrics:

  (`list`)  
  Additional custom metrics to include.

- ...:

  Extra data passed to metric functions (e.g., test_data, optimum).

## Value

A
[MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
# For optimization
tracker <- metrics_tracker("optimize", optimum = 0.0)

# For active learning with test set
tracker <- metrics_tracker("learn", test_data = test_dt)

# For both goals
tracker <- metrics_tracker("both", test_data = test_dt, optimum = 0.0)
} # }
```
