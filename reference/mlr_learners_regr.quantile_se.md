# Quantile Regression Learner with SE Prediction

Wraps a quantile regression learner and converts quantile predictions to
SE. Assumes approximate normality to map inter-quantile range to
standard error.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Base quantile learner. Must support `predict_type = "quantiles"`.

## Details

This learner:

1.  Trains a base learner that supports quantile predictions

2.  predicts lower and upper quantiles

3.  SE prediction is the inter-quantile range multiplied by a given
    factor

## Fields

- `wrapped`:

  (`LearnerRegr`)\cr Read-only access to the wrapped base learner.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))\cr
  The combined parameter set.

## Parameters

- `quantile_response` :: `numeric(1)`  
  Quantile response to use for the prediction. Initialized to 0.5
  (median).

- `quantile_lower` :: `numeric(1)`  
  Lower quantile for SE estimation. Initialized to 0.1 (10th
  percentile).

- `quantile_upper` :: `numeric(1)`  
  Upper quantile for SE estimation. Initialized to 0.9 (90th
  percentile).

- `se_factor` :: `numeric(1)`  
  Factor to multiply the inter-quantile range to get the SE. Initialized
  to 0.5.

The initial setup forwards the wrapped learner's response prediction and
calculates the SE as half the range from predicted 0.1 to 0.9 quantiles.

## Fields

- `$wrapped` ::
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)\cr
  Read-only access to the wrapped base learner.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires a quantile regression learner (e.g., from mlr3extralearners)
# base_learner <- lrn("regr.ranger")  # Assuming it supports quantiles
# learner <- lrn("regr.quantile_se", learner = base_learner)
# learner$param_set$set_values(quantile_lower = 0.1, quantile_upper = 0.9)

# Train and predict
# task <- tsk("mtcars")
# learner$train(task)
# pred <- learner$predict(task)
# pred$se  # Standard errors derived from quantiles
} # }
```
