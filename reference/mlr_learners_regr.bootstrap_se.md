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

2.  Training the base learner on each sample and storing the trained
    state

3.  During prediction, restoring each state and computing predictions

4.  Computing mean and SD of predictions across the ensemble

The standard deviation across bootstrap predictions serves as the
standard error estimate.

The wrapped base learner (`$wrapped`) remains untrained after training
the wrapper. Use `$base_learner()` to get a trained clone of the base
learner.

## Fields

- `wrapped`:

  (`LearnerRegr`)  
  Read-only access to the wrapped base learner.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The combined parameter set.

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
