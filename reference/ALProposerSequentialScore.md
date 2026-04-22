# Sequential Score-Based Active Learning Proposer

Scores a candidate set once and then builds a batch sequentially by
applying a proposer-local
[ALScoreModifier](https://mlr-org.github.io/celecx/reference/ALScoreModifier.md).

Creates a sequential score-based proposer.

## Arguments

- acq_id:

  (`character(1)`)  
  Acquisition-function registry id.

- surrogate_id:

  (`character(1)`)  
  Surrogate registry id.

- score_modifier:

  ([ALScoreModifier](https://mlr-org.github.io/celecx/reference/ALScoreModifier.md))  
  Score modifier used between sequential selections.

- candidate_sampler:

  (`NULL` \|
  [SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))  
  Optional sampler used to create the candidate set to score.

- n_candidates:

  (`integer(1)` \| `Inf`)  
  Number of candidates requested from `candidate_sampler`.

- acq_fit_scope:

  (`character(1)`)  
  Acquisition-function fit scope.

## Fields

- `score_modifier`:

  ([ALScoreModifier](https://mlr-org.github.io/celecx/reference/ALScoreModifier.md))
  Score modifier.
