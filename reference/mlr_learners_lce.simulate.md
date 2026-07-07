# Forward-Simulation LCE Learner

A [LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md)
that predicts the future learning curve by *simulating the
active-learning loop forward* on a surrogate fit over the archive.

At train time it fits an "oracle" regression learner on the archive
`(x -> y)` mapping; the oracle serves as a ground-truth proxy for the
objective. At predict time it re-runs the same
[OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md)
configuration forward, starting from the training archive, using the
oracle to generate outcomes for newly proposed points, and scores the
tracked surrogate after each simulated batch on a held-out evaluation
set whose targets are the oracle's own predictions. The per-batch scores
form the predicted curve.

The optimizer configuration and the oracle learner are properties of the
method and are supplied at construction. The regression measure and (for
pool-based runs) the candidate pool are read from the
[TaskLCE](https://mlr-org.github.io/celecx/reference/TaskLCE.md), so the
same learner can be benchmarked across tasks built from different runs.

Creates a new forward-simulation LCE learner.

## Arguments

- optimizer:

  ([OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md))  
  Active-learning optimizer configuration to run forward. Cloned on
  construction.

- oracle_learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Ground-truth proxy fit on the archive `(x -> y)` mapping. Cloned on
  construction. There is deliberately no default: the choice of oracle
  model is consequential and must be made explicitly.

- surrogate_id:

  (`character(1)`)  
  Id of the surrogate in `optimizer$surrogates` whose performance is
  tracked. Defaults to `"model"` (the
  [optimizer_al](https://mlr-org.github.io/celecx/reference/optimizer_al.md)
  default).

- eval_sampler:

  ([SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))  
  Sampler for the fabricated evaluation set on continuous search spaces.
  Defaults to
  [SpaceSamplerSobol](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md).

## Fields

- `optimizer`:

  ([OptimizerAL](https://mlr-org.github.io/celecx/reference/OptimizerAL.md))  
  Active-learning optimizer configuration run forward by the learner.

- `oracle_learner`:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Ground-truth proxy learner.

- `surrogate_id`:

  (`character(1)`)  
  Tracked surrogate id.

- `eval_sampler`:

  ([SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))  
  Evaluation-set sampler for continuous runs.

## Prediction

The simulation produces a value at each requested `batch_nr`.
Consecutive requested `batch_nr`s define the simulation's per-step batch
size: a request from `b_prev` to `b_next` is realized as one optimizer
proposal batch of `(b_next - b_prev) * evals_per_batch` evaluations,
where `evals_per_batch` is the originating run's per-batch evaluation
count, inferred at train time as the median number of archive rows per
non-initial batch (the initial design is usually larger). This
reproduces the recorded batch sizes when the future batches are
requested at the recorded spacing.

## Prediction types

Each restart is an independent forward simulation, i.e. one joint sample
path of the *realised* trajectory over the requested batches. The
`response` is the across-restart mean on the
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale, back-transformed (the predictive median under the link). The
total predictive `se` is the across-restart standard deviation on the
link scale (`0` with a single restart); the epistemic `se_epistemic` is
that divided by `sqrt(n_restarts)` (the standard error of the expected
curve). The `samples` predict type returns the restart paths themselves
(one column per restart), `quantiles` their per-row sample quantiles,
and `target_reached` the fraction of restart paths that have reached the
target. Richer-than-`response` predict types therefore need
`n_restarts > 1` (with a stochastic proposer).

## Parameters

- `n_eval_points` :: `integer(1)`  
  Size of the fabricated held-out evaluation set (continuous runs).
  Initialized to `100`. Ignored for pool-based runs (the pool is used).

- `n_restarts` :: `integer(1)`  
  Number of independent forward simulations, which is also the number of
  predictive sample paths. Initialized to `1` (then `se` is `0`, since
  no spread is measured). Only meaningful with a stochastic proposer.

- `extrapolation` :: `character(1)`  
  How to extend beyond the simulated horizon (only reached via
  `max_batch_cap`): `"hold"` repeats the last value, `"linear"`
  continues the slope of the last two simulated points. Initialized to
  `"hold"`.

- `max_batch_cap` :: `integer(1)` \| `NULL`  
  Maximum number of future points to simulate; further requested points
  use `extrapolation`. `NULL` (default) simulates every requested future
  point.

- `seed` :: `integer(1)` \| `NULL`  
  RNG seed. Governs both the train-time evaluation-set sampling and the
  predict-time simulation, so a fixed seed makes the whole prediction
  reproducible. `NULL` (default) uses the current RNG state.

## Known limitations

- The predicted quantity is the surrogate scored against the oracle's
  predictions, which can differ in level and plateau from the original
  surrogate-vs-test-set target (inherent to a self-contained forecast).

- Search-space transformations are ignored: the oracle is fit on, and
  proposals are made on, the untransformed scale.

- `se` reflects only proposer/sampler stochasticity across restarts; a
  deterministic configuration yields `se = 0`.
