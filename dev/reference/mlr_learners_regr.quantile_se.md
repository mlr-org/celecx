# Quantile Regression Learner with SE Prediction

Wraps a quantile regression learner and converts quantile predictions to
SE.

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

3.  SE prediction is the inter-quantile range multiplied by a factor

## Parameters

The base learner's parameters are exposed with the `base.` prefix.

Own parameters:

- `quantile_response` :: `numeric(1)`  
  Quantile response to use for the prediction. Initialized to 0.5
  (median).

- `quantile_lower` :: `numeric(1)`  
  Lower quantile for SE estimation. Initialized to 0.1 (10th
  percentile).

- `quantile_upper` :: `numeric(1)`  
  Upper quantile for SE estimation. Initialized to 0.9 (90th
  percentile).

- `se_factor` :: `numeric(1)` \| `NULL`  
  Factor to multiply the inter-quantile range to get the SE. The default
  `NULL` uses the normal-consistent factor
  `1 / (qnorm(quantile_upper) - qnorm(quantile_lower))`, so that under
  an approximately Gaussian predictive the SE estimates the predictive
  SD.

## Fields

- `$wrapped` ::
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)  
  Read-only access to the wrapped base learner.
