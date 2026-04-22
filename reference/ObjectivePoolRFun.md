# Objective Function Based on a Candidate Pool and R Function

An
[ObjectivePoolAbstract](https://mlr-org.github.io/celecx/reference/ObjectivePoolAbstract.md)
where matched pool rows are evaluated by a user-supplied R function.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- pool:

  (`data.frame` \| `data.table`)  
  Candidate pool containing one row per admissible configuration. Must
  contain all domain columns. Extra columns are allowed and are passed
  to `fun`.

- fun:

  (`function`)  
  R function that evaluates matched pool rows. It must accept at least
  one argument, the matched pool rows, and return the codomain values as
  a `data.table`, `data.frame`, or `list` that can be converted to a
  `data.table` using `rbindlist`.

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the input space.

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the output space.

- id:

  (`character(1)`)  
  Identifier for the objective.

- properties:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Objective properties such as `"deterministic"` or `"noisy"`.

- constants:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Constant parameters that are not subject to tuning.

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Packages required to evaluate `fun`.

- check_values:

  (`logical(1)`)  
  Whether to check validity of input configurations against the domain.

## Details

The evaluation function receives the matched pool rows, not just the
domain columns. Extra pool columns are therefore available for candidate
identifiers or other metadata needed during evaluation.

Duplicate domain configurations in the pool are rejected.

## Fields

- `fun`:

  (`function`)  
  Evaluation function used on the matched pool rows.
