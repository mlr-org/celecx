# Active Learning Proposal Context

Ephemeral context passed from
[OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md)
to
[ALProposer](https://mlr-org.github.io/celecx/reference/ALProposer.md)
objects for a single outer proposal round.

The context keeps shallow references to the optimizer's canonical
surrogate and acquisition-function registries. Access should go through
`$get_surrogate()` and `$get_acq()`, because these methods implement
lazy model updates, lazy acquisition-function fitting, and
proposer-local pending-point filtering.

Creates a new active-learning context.

Adds points selected for the current proposal batch.

Lazily updates and returns a canonical surrogate.

Lazily wires, fits, updates, and returns an acquisition function.

Creates a fresh acquisition-function working object.

Points that have been evaluated or are pending evaluation.

## Arguments

- instance:

  ([bbotk::EvalInstance](https://bbotk.mlr-org.com/reference/EvalInstance.html))  
  Search or optimization instance.

- surrogates:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))  
  Canonical run-local surrogate objects.

- acq_functions:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html))  
  Unwired acquisition-function prototypes.

- run_state:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Run-local mutable state shared across proposal rounds.

- allow_repeat_evaluations:

  (`logical(1)`)  
  Whether to allow repeat evaluations of the same point.

- xdt:

  (`data.table`)  
  Points selected in the current proposal round.

- id:

  (`character(1)`)  
  Surrogate registry id.

- surrogate_id:

  (`character(1)`)  
  Surrogate registry id.

- clone:

  (`logical(1)`)  
  Whether to return a fresh working clone.

- acq_id:

  (`character(1)`)  
  Acquisition-function registry id.

- surrogate:

  ([mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))  
  Surrogate to wire into the acquisition function.

- fit_scope:

  (`character(1)`)  
  Either `"global"`, `"candidate"`, or `"search_space"`. `"global"`
  falls back to `"search_space"` when the run has no finite pool.

- pool:

  (`NULL` \| `data.table`)  
  Candidate pool for candidate-scope fitting.

## Value

[mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html).

[mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html).

[mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html).

## Fields

- `instance`:

  ([bbotk::EvalInstance](https://bbotk.mlr-org.com/reference/EvalInstance.html))  
  Search or optimization instance.

- `pool`:

  (`NULL` \| `data.table`) Full finite candidate pool.

- `unevaluated_xdt`:

  (`NULL` \| `data.table`) Remaining finite candidate pool that were not
  evaluated yet (but may have been proposed already; use
  `proposable_xdt` to get candidates that can still be proposed). NULL
  for continuous contexts without `pool`.

  'unevaluated' candidates are the union of 'proposable' and 'pending'
  candidates.

- `unevaluated_indices`:

  (`NULL` \| `integer`) Indices within `pool` of the remaining finite
  candidate pool that were not evaluated yet (but may have been proposed
  already; use `proposable_indices` to get indices of candidates that
  can still be proposed). NULL for continuous contexts without `pool`.

  'unevaluated' indices are the union of 'proposable' and 'pending'
  indices.

- `evaluated_xdt`:

  (`data.table`) Points that have been evaluated.

- `evaluated_indices`:

  (`integer` \| `NULL`) Indices within `pool` of points that have been
  evaluated. NULL for continuous contexts without `pool`.

- `pending_xdt`:

  (`data.table`) Points already selected during the current proposal
  round. These are a subset of 'unevaluated' points.

- `pending_indices`:

  (`integer` \| `NULL`) Indices within `pool` of points already selected
  during the current proposal round. NULL for continuous contexts
  without `pool`. These are a subset of 'unevaluated' indices.

- `proposable_xdt`:

  (`NULL` \| `data.table`) Candidates that can still be proposed. NULL
  for continuous contexts without `pool`. These are all 'unevaluated'
  points that are not 'pending' when `allow_repeat_evaluations` is
  FALSE; otherwise, these are all points that are not pending.

- `proposable_indices`:

  (`integer` \| `NULL`) Indices within `pool` of candidates that can
  still be proposed. NULL for continuous contexts without `pool`. These
  are all 'unevaluated' indices that are not 'pending' when
  `allow_repeat_evaluations` is FALSE; otherwise, these are all indices
  that are not pending.

- `evaluated_and_pending_indices`:

  (`integer` \| `NULL`) Indices within `pool` of points that have been
  evaluated or are pending evaluation. NULL for continuous contexts
  without `pool`.

- `allow_repeat_evaluations`:

  (`logical(1)`) Whether to allow repeat evaluations of the same point.
