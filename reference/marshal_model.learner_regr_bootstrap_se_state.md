# Marshal Model for LearnerRegrBootstrapSE State

Marshals the model state of
[LearnerRegrBootstrapSE](https://celecx.mlr-org.com/reference/mlr_learners_regr.bootstrap_se.md),
preparing it for serialization. This marshals each bootstrap state
individually.

## Usage

``` r
# S3 method for class 'learner_regr_bootstrap_se_state'
marshal_model(model, inplace = FALSE, ...)
```

## Arguments

- model:

  (`learner_regr_bootstrap_se_state`)  
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
version with class `learner_regr_bootstrap_se_state_marshaled`.
