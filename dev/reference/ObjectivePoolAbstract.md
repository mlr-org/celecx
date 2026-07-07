# Abstract Base Class for Pool-backed Objectives

Common base class for objectives that restrict admissible evaluations to
a finite candidate pool.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Evaluates multiple input values on the objective function.

Evaluates multiple input values on the objective function.

## Arguments

- pool:

  (`data.frame` \| `data.table`)  
  Candidate pool containing one row per admissible configuration. Must
  contain all domain columns.

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
  Packages required to evaluate the objective.

- check_values:

  (`logical(1)`)  
  Whether to check validity of input configurations against the domain.

- label:

  (`character(1)`)  
  Human-readable label.

- man:

  (`character(1)`)  
  Help topic in `pkg::topic` format.

- xdt:

  (`data.table`)  
  A data.table with one configuration per row.

- xss:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  A list of lists that contains multiple x values, e.g.
  `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.

## Value

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
containing codomain columns.

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
containing codomain columns.

## Details

Subclasses define how matched pool rows are turned into codomain values.
This base class handles pool validation, exact matching of queried
configurations back to the pool, and inspection of queried versus
remaining pool rows through the attached archive.

Objectives inheriting from this class have the property
"pool_restricted", indicating that a `$pool` field is available.

## Fields

- `pool`:

  (`data.table`)  
  Read-only access to the candidate pool.

- `pool_size`:

  (`integer(1)`)  
  Number of candidates in the pool.
