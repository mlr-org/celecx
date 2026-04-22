# Validate Learner Against Domain

Validates that a fitted learner is compatible with a given domain.
Checks that the learner is fitted, that domain parameters match trained
features, and validates type compatibility including factor level
constraints.

## Usage

``` r
assert_learner_domain(learner, domain, learner_name = NULL)
```

## Arguments

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  A fitted regression learner.

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the input space.

## Value

`TRUE` invisibly if validation succeeds.

## Details

Type compatibility:

- `ParamDbl` -\> numeric feature in training task

- `ParamInt` -\> integer or numeric feature in training task

- `ParamFct` -\> factor feature in training task, and domain levels must
  be a subset of the trained factor levels

- `ParamLgl` -\> logical feature in training task
