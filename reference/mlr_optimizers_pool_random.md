# Random Search on Pool-Restricted or Discrete Objectives

Optimizer that randomly samples configurations from a candidate pool
when the objective has the `"pool_restricted"` property (typically by
inheriting from
[ObjectivePoolAbstract](https://mlr-org.github.io/celecx/reference/ObjectivePoolAbstract.md)),
or from the full grid of a completely discrete search space (where all
parameters are
[paradox::p_int](https://paradox.mlr-org.com/reference/Domain.html),
[paradox::p_fct](https://paradox.mlr-org.com/reference/Domain.html), or
[paradox::p_lgl](https://paradox.mlr-org.com/reference/Domain.html)).

When the search space is continuous (not fully discrete) and the
objective is not pool-restricted, this optimizer falls back to uniform
random search (like
[bbotk::OptimizerBatchRandomSearch](https://bbotk.mlr-org.com/reference/mlr_optimizers_random_search.html)).

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- grid_expansion_limit:

  (`integer(1)`)  
  Upper limit on the number of rows to materialize for the full grid for
  discrete search spaces in Objectives that are not pool-restricted. An
  error is raised if the full grid would exceed this limit. This is to
  protect against accidental usage of too much memory. Default is
  `1e7L`.

## Parameters

- `batch_size`:

  `integer(1)`  
  Number of configurations per batch. `Inf` evaluates the entire
  remaining pool in a single batch (forbidden when
  `replace_samples = "within_batches"` or when the objective is not
  pool-restricted). Default is `1`.

- `max_batches`:

  `integer(1)`  
  Maximum number of batches to evaluate. `Inf` means no batch limit
  (termination is governed solely by the instance's terminator). Default
  is `Inf`.

- `replace_samples`:

  `character(1)`  
  Controls duplicate handling across evaluations:

  - `"never"`: configurations already present in the archive are
    excluded from sampling. The pool shrinks as evaluations accumulate.

  - `"between_batches"`: each batch is drawn without replacement from
    the full pool, but previous batches do not affect future sampling.

  - `"within_batches"`: samples inside each batch are drawn with
    replacement from the full pool, so duplicates are possible even
    within a single batch.

  Default is `"never"`.
