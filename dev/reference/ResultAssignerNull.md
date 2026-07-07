# Null Result Assigner

Result assigner that intentionally does nothing.

This is useful for search runs where no single "best" point exists, e.g.
active learning tasks with codomain targets tagged `"learn"`. In these
cases, calling `archive$best()` is undefined and will error, so using
the default result assigner from mlr3mbo is not appropriate.

Creates a new ResultAssignerNull.

Assigns the result to the instance.

This implementation does nothing (leaves `instance$result` untouched).

## Arguments

- instance:

  ([bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html))  
  The instance the result would be assigned to.

## Fields

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required packages (none).
