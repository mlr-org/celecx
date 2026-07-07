# Marshal Model for LearnerRegrQuantileSE State

Marshals the model state of
[LearnerRegrQuantileSE](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.quantile_se.md),
preparing it for serialization. This marshals the model inside the
stored base learner state (via
[mlr3::mlr3](https://mlr3.mlr-org.com/reference/mlr3-package.html)'s
`marshal_model.learner_state`).

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
