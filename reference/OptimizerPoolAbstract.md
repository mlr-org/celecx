# Abstract Base Class for Pool-Aware Optimizers

Extends
[OptimizerSearchAbstract](https://celecx.mlr-org.com/reference/OptimizerSearchAbstract.md)
with infrastructure for optimizing over pool-restricted objectives
([ObjectivePoolAbstract](https://celecx.mlr-org.com/reference/ObjectivePoolAbstract.md))
and fully discrete search spaces.

Subclasses implement the actual optimization logic by overriding the
private methods `.optimize_discrete()` and `.optimize_continuous()`.
This base class handles pool detection, candidate filtering, and
discrete-grid expansion.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- id:

  (`character(1)`)  
  Identifier for the optimizer.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

- param_classes:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported parameter classes.

- properties:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Optimizer properties.

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required packages.

- label:

  (`character(1)`)  
  Human-readable label.

- man:

  (`character(1)`)  
  Manual page reference.

- grid_expansion_limit:

  (`integer(1)`)  
  Upper limit on the number of rows to materialize for the full grid for
  discrete search spaces in Objectives that are not pool-restricted. An
  error is raised if the full grid would exceed this limit. Default is
  `1e7L`.

## Fields

- `grid_expansion_limit`:

  (`integer(1)`)  
  Upper limit on the number of rows to materialize for the full grid for
  discrete search spaces in Objectives that are not pool-restricted.

## Extending

Concrete subclasses must implement two private methods:

- `.optimize_discrete(inst, candidates)`:

  Called when the search space is fully discrete or the objective is
  pool-restricted. `candidates` is a `data.table` of admissible
  configurations.

- `.optimize_continuous(inst)`:

  Called when the search space contains continuous parameters and the
  objective is not pool-restricted.
