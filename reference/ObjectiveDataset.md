# Objective Based on Pre-evaluated Dataset

An
[bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html)
subclass where evaluation happens by table lookup in a pre-evaluated
dataset. This is useful for "replaying" optimization on historical data
or for testing optimization algorithms on known datasets.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- dataset:

  (`TaskRegr` \| `data.frame` \| `data.table`)  
  Dataset containing pre-evaluated configurations. Must contain columns
  matching all domain parameters and codomain targets.

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the input space. All parameter IDs must
  correspond to columns in the dataset (excluding codomain columns).

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the output space. Must contain at least one
  target tagged with `"minimize"`, `"maximize"`, or `"learn"`. Target
  IDs must correspond to columns in the dataset.

- id:

  (`character(1)`)  
  Identifier for the objective.

- check_values:

  (`logical(1)`)  
  Whether to check validity of input configurations against the domain.

## Details

The dataset must contain columns for all parameters in the domain and
all targets in the codomain. Evaluation fails with an informative error
if a requested configuration is not present in the dataset.
