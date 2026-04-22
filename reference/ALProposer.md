# Active Learning Proposer

Base class for objects that propose the next active-learning batch.

Proposers receive an
[ALContext](https://celecx.mlr-org.com/reference/ALContext.md) from
[OptimizerAL](https://celecx.mlr-org.com/reference/OptimizerAL.md). They
should use the context's accessor methods instead of touching surrogate
and acquisition registries directly, because those methods implement
lazy update and clone-on-write rules.

Inheriting classes should implement the `private$.propose()` method. It
gets the context and the number of points to propose, should return a
`data.table` of the proposed points.

Creates a new proposer.

Proposes up to `n` points for the next evaluation batch.

Calculates the scalar score to be maximized by the proposer.

Returns the indices of the top `n` candidates based on the utility
score.

## Arguments

- id:

  (`character(1)`)  
  Identifier.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| [`list()`](https://rdrr.io/r/base/list.html))  
  Configuration parameter set or dynamic parameter-set source list.

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required packages.

- label:

  (`character(1)`)  
  Label.

- man:

  (`character(1)`)  
  Help page reference.

- additional_phash_input:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional private/public fields used for persistent hashing.

- context:

  ([ALContext](https://celecx.mlr-org.com/reference/ALContext.md))  
  Active-learning proposal context.

- n:

  (`integer(1)`)  
  Maximum number of points to propose.

## Value

`data.table`.

## Fields

- `label`:

  (`character(1)`) Label.

- `man`:

  (`character(1)`) Help page reference.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Required
  packages.
