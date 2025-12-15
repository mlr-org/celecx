# Batch Proposer

Extension of
[mlr3mbo::AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html)
that applies a batch selection strategy to diversify the returned
candidate set.

Creates a new BatchProposer.

Optimize the acquisition function and apply a batch selection strategy.

Print method.

## Arguments

- optimizer:

  ([bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html))  
  Optimizer used for acquisition optimization.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Terminator used for acquisition optimization.

- acq_function:

  (`NULL` \|
  [mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html))  
  Acquisition function. Can be set later via `$acq_function`.

- batch_strategy:

  (`function`)  
  Batch selection strategy. Must have signature
  `function(candidates, scores, batch_size, surrogate, archive, search_space)`
  and return integer indices into `candidates`.

- pool_factor:

  (`integer(1)`)  
  Pool size multiplier. Pool size is `n_candidates * pool_factor`.
  Default is 10.

- callbacks:

  (`NULL` \| [`list()`](https://rdrr.io/r/base/list.html))  
  Callbacks. Passed to parent
  [mlr3mbo::AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html).

- ...:

  (ignored).

## Value

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
with 1 row per candidate.

## Details

This class implements a wrapper around the parent
[mlr3mbo::AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html):
it temporarily requests a larger number of candidates, then down-selects
to the desired batch size using `batch_strategy`.

Batch strategies operate on a pool that is already ranked by the
acquisition optimizer and follow the convention: **lower is better**
(the sign is adjusted automatically based on the acquisition codomain).

## Fields

- `pool_factor`:

  (`integer(1)`)  
  Multiplier for pool size. The internal pool size is
  `n_candidates * pool_factor`.

- `batch_strategy`:

  (`function`)  
  Batch selection strategy.

## Parameters

Inherits all parameters from
[mlr3mbo::AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html),
in particular:

- `n_candidates`: number of points to propose (batch size)

- `warmstart`, `warmstart_size`, `skip_already_evaluated`,
  `catch_errors`, `logging_level`
