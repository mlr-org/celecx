# Marshal Model for LearnerRegrQuantileSE State

Marshals the model state of
[LearnerRegrQuantileSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.quantile_se.md),
preparing it for serialization.

## Usage

``` r
# S3 method for class 'learner_regr_quantile_se_state'
marshal_model(model, inplace = FALSE, ...)
```

## Arguments

- model:

  (`learner_regr_quantile_se_state`)  
  The model to marshal.

- inplace:

  (`logical(1)`)  
  Whether to marshal in place.

- ...:

  (any)  
  Additional arguments passed to
  [`mlr3::marshal_model()`](https://mlr3.mlr-org.com/reference/marshaling.html).

## Value

Either the original model (if no marshaling was needed) or a marshaled
version with class `learner_regr_quantile_se_state_marshaled`.
