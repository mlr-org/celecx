# Metrics Tracker

Tracks per-batch metrics during a search, providing the data needed for
progress monitoring, learning curve analysis, and forecasting.

Creates a new MetricsTracker.

Log metrics for a completed batch.

Add a new metric to track (for future batches).

Remove a metric from tracking.

Update extra_data used by metric functions.

Clear all logged history.

Get metrics for a specific batch.

Extract a single metric's history as a vector.

Printer.

## Arguments

- metrics:

  (`list`)  
  Named list of metric functions. Names become column names in the
  history table. Each function should have signature
  `function(archive, surrogate = NULL, ...)` and return a scalar.

- extra_data:

  (`list`)  
  Additional data passed to metric functions via `...`. For example,
  `list(test_data = test_dt, optimum = 0.5)`.

- fun:

  (`function`)  
  Metric function.

- backfill:

  (`logical(1)`)  
  If TRUE and there's existing history, attempt to compute the metric
  for past batches. Requires archive snapshots which may not be
  available.

- keep_data:

  (`logical(1)`)  
  If TRUE, keep the column in the data table. Default FALSE removes it.

- batch_nr:

  (`integer(1)`)  
  The batch number to retrieve.

- name:

  (`character(1)`)  
  Name of the metric.

- ...:

  (ignored).

- metric_fun:

  (`function`) The metric function.

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))
  Current archive.

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))
  Current surrogate (may be NULL).

- extra_args:

  (`list`) Extra arguments for the function.

## Value

Invisibly returns the new row as a `data.table`.

A single-row `data.table`, or NULL if batch not found.

A numeric vector of metric values.

Scalar metric value. Initialize the empty history table.

## Details

The tracker stores a history table with one row per batch. Built-in
columns:

- `batch_nr`: Batch number (1-indexed)

- `n_evals`: Cumulative number of evaluations

- `timestamp`: When the batch was logged

Additional columns are computed by user-defined metric functions. These
functions receive the current state (archive, surrogate, extra data) and
return a scalar value.

## Fields

- `data`:

  (`data.table`)  
  The history table with all logged metrics.

- `metrics`:

  (`list`)  
  The registered metric functions.

- `extra_data`:

  (`list`)  
  Additional data passed to metric functions.

- `n_batches`:

  (`integer(1)`)  
  Number of batches logged.

- `metric_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of all tracked metrics (excluding built-in columns).

- `latest`:

  (`data.table`)  
  The most recently logged row, or NULL if no history. Compute a single
  metric value.

## Metric Functions

Metric functions should have signature:

    function(archive, surrogate = NULL, ...)

They should return a single numeric value. The `...` can include
additional context like test data, known optimum, etc.

See `search_metrics.R` for built-in metrics:

- [`metric_best_y()`](https://celecx.mlr-org.com/reference/metric_best_y.md):
  Best observed y value (optimization)

- [`metric_regret()`](https://celecx.mlr-org.com/reference/metric_regret.md):
  Gap to known optimum (optimization)

- [`metric_model_rmse()`](https://celecx.mlr-org.com/reference/metric_model_rmse.md):
  Surrogate RMSE on test data (learning)

- [`metric_mean_variance()`](https://celecx.mlr-org.com/reference/metric_mean_variance.md):
  Average prediction variance (learning)

## Usage with Forecasters

The `$data` field returns the complete history as a `data.table`, which
can be directly fed to learning curve extrapolation models. The
`n_evals` column serves as the x-axis (sample size) for most forecasting
methods.
