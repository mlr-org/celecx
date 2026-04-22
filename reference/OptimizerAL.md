# Proposer-Based Active Learning Optimizer

Active-learning optimizer whose outer loop is fixed and whose proposal
logic is delegated to an
[ALProposer](https://mlr-org.github.io/celecx/reference/ALProposer.md).

The optimizer owns canonical run-local surrogates and unwired
acquisition function prototypes. Proposers refer to those objects by
registry id; the ephemeral
[ALContext](https://mlr-org.github.io/celecx/reference/ALContext.md)
wires and updates them lazily.

Creates a new active-learning optimizer.

## Arguments

- proposer:

  ([ALProposer](https://mlr-org.github.io/celecx/reference/ALProposer.md))  
  Proposer used after initialization.

- surrogates:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))  
  Canonical surrogate registry. Defaults to an archive-only surrogate
  under id `"archive"` for model-free acquisition functions.

- acq_functions:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html))  
  Acquisition-function prototype registry. Registered acquisition
  functions are treated as unwired prototypes; any pre-set surrogate is
  removed with a warning.

- init_sampler:

  (`NULL` \|
  [SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))  
  Sampler for the initial evaluations. `NULL` is only valid when
  `n_init = 0`.

- result_assigner:

  (`NULL` \|
  [mlr3mbo::ResultAssigner](https://mlr3mbo.mlr-org.com/reference/ResultAssigner.html))  
  Result assigner used for
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  objects. Defaults to
  [ResultAssignerNull](https://mlr-org.github.io/celecx/reference/ResultAssignerNull.md).

- grid_expansion_limit:

  (`integer(1)`)  
  Upper limit for fully-discrete grid expansion inherited from
  [OptimizerPoolAbstract](https://mlr-org.github.io/celecx/reference/OptimizerPoolAbstract.md).

## Fields

- `proposer`:

  ([ALProposer](https://mlr-org.github.io/celecx/reference/ALProposer.md))
  Proposer used after initialization.

- `init_sampler`:

  (`NULL` \|
  [SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md))
  Sampler for initial evaluations.

- `surrogates`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))
  Canonical surrogate registry.

- `acq_functions`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html))
  Unwired acquisition-function prototype registry.

- `result_assigner`:

  (`NULL` \|
  [mlr3mbo::ResultAssigner](https://mlr3mbo.mlr-org.com/reference/ResultAssigner.html))
  Result assigner.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  Combined parameter set of the optimizer and directly owned components.

## Parameters

- `batch_size`:

  `integer(1)`  
  Number of configurations evaluated per active-learning proposal round.

- `replace_samples`:

  `character(1)`  
  Whether finite-pool points can be proposed again after a batch.
  `"never"` exhausts the pool without replacement; `"between_batches"`
  allows repeat evaluations in later batches, while still preventing
  repeats within the current proposal batch.

- `n_init`:

  `integer(1)`  
  Number of initial evaluations requested before the proposer is used.
  If unset, fresh runs use `4 * d` initial evaluations, where `d` is the
  search-space dimension, while runs with an already populated archive
  do not request additional initial evaluations.

The optimizer parameter set also exposes the parameter sets of directly
owned components: `init_sampler`, `proposer`, `surrogates`, and
acquisition-function `constants`.
