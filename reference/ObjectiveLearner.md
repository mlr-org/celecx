# Objective Function Based on a Fitted Learner

An
[bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html)
subclass where evaluation uses a fitted regression learner to predict
outcomes. This is useful for surrogate-based optimization where a model
has been trained on observed data and we want to optimize over predicted
outcomes.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Evaluates multiple input values on the objective function.

## Arguments

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  A fitted regression learner. Must have been trained via `$train()`
  before creating the objective.

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the input space. All parameter IDs must
  correspond to features the learner was trained on.

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the output space. Must contain at least one
  target tagged with `"minimize"`, `"maximize"`, or `"learn"`.

- id:

  (`character(1)`)  
  Identifier for the objective.

- check_values:

  (`logical(1)`)  
  Whether to check validity of input configurations against the domain.

- xdt:

  (`data.table`)  
  A data.table with one configuration per row.

## Value

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
containing codomain columns.

## Details

The learner must be trained before creating the objective. The domain
parameters must correspond to features the learner was trained on. For
factor features, domain levels must be a subset of the levels present in
the training data (the learner cannot generalize to unseen factor
levels).

Type compatibility:

- `ParamDbl` corresponds to numeric training features

- `ParamInt` corresponds to integer or numeric training features

- `ParamFct` corresponds to factor training features (domain levels must
  be subset)

- `ParamLgl` corresponds to logical training features

## Fields

- `learner`:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Read-only access to the internal learner.

- `train_task`:

  ([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html))  
  Returns the task the learner was trained on (without data backend).
