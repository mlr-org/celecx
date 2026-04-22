# Score-Based Active Learning Proposer

Scores a finite candidate set once with an acquisition function and
returns the top candidates.

Creates a score-based proposer.

## Arguments

- acq_id:

  (`character(1)`)  
  Acquisition-function registry id.

- surrogate_id:

  (`character(1)`)  
  Surrogate registry id.

- candidate_sampler:

  (`NULL` \|
  [SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))  
  Optional sampler used to create the candidate set to score. `NULL`
  means exhaustive scoring of the remaining finite pool.

- n_candidates:

  (`integer(1)` \| `Inf`)  
  Number of candidates requested from `candidate_sampler`. Ignored when
  `candidate_sampler` is `NULL`.

- acq_fit_scope:

  (`character(1)`)  
  `"global"` fits acquisition functions on the run's full finite pool;
  `"candidate"` fits on the current candidate set; `"search_space"` fits
  on the search space bounds.
