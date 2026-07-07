# Expanding-Window LCE Cross-Validation

Time-series style cross-validation for
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
Splits are made by whole batches (column with role `feature`, i.e. the
`batch_nr` column of the task). Each fold trains on all batches from the
very first up to a moving training end and tests on the next `horizon`
batches; the training window therefore always starts at the beginning
and expands by `step_size` batches between folds.

Creates a new expanding-window LCE resampling.

## Fields

- `iters`:

  (`integer(1)`)  
  Number of resampling iterations. Only meaningful after
  `$instantiate()`.

## Parameters

- `horizon` :: `integer(1)`  
  Number of consecutive batches used as test set in every fold.
  Initialized to `1`.

- `step_size` :: `integer(1)`  
  Number of batches between the train-end indices of consecutive folds.
  Initialized to `1`.

- `min_train_batches` :: `integer(1)`  
  Number of batches in the training set of the first fold. Required,
  with no default: results are sensitive to it and there is no good
  universal value. Must be at least the wrapped learner's minimum
  training requirement, which is larger for some learners (e.g.
  [LearnerLCESplineMonotone](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.spline_monotone.md)
  needs five batches,
  [LearnerLCEConformal](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.conformal.md)
  needs `n_calibration_batches + 1`).

- `folds` :: `integer(1)` \| `NULL`  
  Maximum number of folds. When unset, all feasible folds are generated.
