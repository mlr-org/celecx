# Bootstrap Ensemble Learner with SE Prediction

Wraps any regression learner and trains a bootstrap ensemble.
Predictions return mean and SE across bootstrap samples.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Base learner to bootstrap.

## Details

This learner creates a bootstrap ensemble by:

1.  Taking `n_bootstrap` bootstrap samples (sampling with replacement)

2.  Training the base learner on each sample, storing the trained state

3.  During prediction, restoring each state and querying its model

4.  Computing mean and SD of predictions across the ensemble

The standard deviation across bootstrap predictions serves as the
standard error estimate.

The bootstrap tasks are rebuilt from the task's feature/target data, so
task properties beyond that (observation weights, strata, groups) are
not forwarded to the ensemble members and the corresponding base-learner
properties are not advertised by the wrapper.

The wrapped base learner (`$wrapped`) remains untrained after training
the wrapper. Use `$base_learner()` to get a trained clone of the base
learner.

## Parameters

The base learner's parameters are exposed with the `base.` prefix (e.g.
`base.maxdepth`).

Own parameters:

- `n_bootstrap` :: `integer(1)`  
  Number of bootstrap samples. Initialized to `30`.

## Fields

- `$wrapped` ::
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)  
  Read-only access to the wrapped base learner.

## Examples

``` r
if (FALSE) { # \dontrun{
# Wrap ranger with bootstrap SE
learner <- lrn("regr.bootstrap_se", learner = lrn("regr.ranger"))
learner$param_set$set_values(n_bootstrap = 10)

# Train on a task
task <- tsk("mtcars")
learner$train(task)

# Predict with SE
pred <- learner$predict(task)
pred$se  # Standard errors
} # }
```
