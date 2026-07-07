# Per-Batch LCE Loss Measures

Common per-batch loss measures for
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md) /
[LearnerLCE](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md)
evaluations. Each measure aggregates predictions and truth to one value
per batch (via `lce_per_batch`) and then computes a standard regression
loss on those per-batch pairs.

- `lce.mse`: mean squared error.

- `lce.rmse`: root mean squared error (the square root of the per-batch
  MSE).

- `lce.mae`: mean absolute error.

All measures support observation weights from a `weights_measure` task
column; the weight applied to a batch is the sum of weights of its
archive rows.
