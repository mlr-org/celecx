# Objective Function Wrapping Another Objective on a Candidate Pool

An
[ObjectivePoolAbstract](https://celecx.mlr-org.com/reference/ObjectivePoolAbstract.md)
that restricts evaluation to a candidate pool and delegates evaluation
to another
[bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html).

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- pool:

  (`data.frame` \| `data.table`)  
  Candidate pool containing one row per admissible configuration. Must
  contain all wrapped-objective domain columns.

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  Objective to wrap. It is cloned on construction.

- id:

  (`character(1)`)  
  Identifier for the objective.

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`)  
  Parameter set describing the input space. If `NULL`, the wrapped
  objective's domain is used.

- check_values:

  (`logical(1)`)  
  Whether to check validity of input configurations against the domain.
  This does not affect the wrapped objective's `check_values`. When
  `domain` is `NULL`, the wrapped objective's `check_values` is set to
  `FALSE`. Otherwise, it is unchanged; it is up to the user to decide
  whether it is desirable for the wrapped objective to check values.

## Details

The wrapped objective is cloned on construction. Its codomain,
properties, and package requirements define the corresponding fields of
the wrapper.

The wrapped objective's domain may differ from the wrapper's domain; in
this case, the missing values are reconstructed from the pool columns
and superfluous columns are dropped. The pool values must be unique with
respect to the wrapper's domain.

## Fields

- `objective`:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  Wrapped objective.
