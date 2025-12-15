# Unmarshal Model for LearnerRegrBootstrapSE State

Unmarshals the model state of
[LearnerRegrBootstrapSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.bootstrap_se.md),
restoring it after deserialization.

## Usage

``` r
# S3 method for class 'learner_regr_bootstrap_se_state_marshaled'
unmarshal_model(model, inplace = FALSE, ...)
```

## Arguments

- model:

  (`learner_regr_bootstrap_se_state_marshaled`)  
  The marshaled model to unmarshal.

- inplace:

  (`logical(1)`)  
  Whether to unmarshal in place.

- ...:

  (any)  
  Additional arguments passed to
  [`mlr3::unmarshal_model()`](https://mlr3.mlr-org.com/reference/marshaling.html).

## Value

The unmarshaled model state.
