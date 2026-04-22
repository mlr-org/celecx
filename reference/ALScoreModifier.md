# Active Learning Score Modifier

Score modifiers adjust acquisition utilities while a batch is built
sequentially. Utilities use the convention that larger values are
better.

Score modifiers are proposer-local: they may consider points already
selected for the current batch, but they do not change the real archive,
surrogate, or acquisition function state.

Creates a new score modifier.

Modifies acquisition utilities.

## Arguments

- id:

  (`character(1)`)  
  Identifier.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Configuration parameters.

- label:

  (`character(1)`)  
  Label.

- man:

  (`character(1)`)  
  Help page reference.

- candidates:

  (`data.table`)  
  Candidate points.

- utility:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Acquisition utility, larger is better.

- selected:

  (`data.table`)  
  Points already selected for the current batch or portfolio round. The
  point of the score modifier is typically to penalize points close to
  points proposed within the same batch (but to be indifferent of points
  that were already evaluated – it is expected that their value was
  taken into account by the acquisition function).

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space.

- context:

  ([ALContext](https://mlr-org.github.io/celecx/reference/ALContext.md))  
  Current active-learning context.

## Value

[`numeric()`](https://rdrr.io/r/base/numeric.html).

## Fields

- `label`:

  (`character(1)`) Label.

- `man`:

  (`character(1)`) Help page reference.
