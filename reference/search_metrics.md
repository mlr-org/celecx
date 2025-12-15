# Search Metrics

Built-in metric functions for tracking search progress. These functions
are used by
[MetricsTracker](https://celecx.mlr-org.com/reference/MetricsTracker.md)
to compute per-batch summaries.

## Details

All metric functions follow the signature:

    function(archive, surrogate = NULL, ...)

They return a single numeric value. The `...` can receive additional
context like test data, known optimum, evaluation grid, etc.

## Optimization Metrics

- `metric_best_y`: Best observed value (for minimization)

- `metric_worst_y`: Worst observed value

- `metric_regret`: Gap between current best and known optimum

- `metric_simple_regret`: Same as regret (alias)

## Learning Metrics

- `metric_model_rmse`: Surrogate RMSE on test data

- `metric_model_mae`: Surrogate MAE on test data

- `metric_model_r2`: Surrogate R-squared on test data

- `metric_mean_variance`: Average prediction variance on a grid

- `metric_max_variance`: Maximum prediction variance on a grid

- `metric_integrated_variance`: Integral of variance over domain

## Progress Metrics

- `metric_n_evals`: Total evaluations (built-in to tracker)

- `metric_improvement_rate`: Improvement per evaluation
