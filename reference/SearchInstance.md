# Search Instance

Container for a search problem that extends bbotk's
[bbotk::EvalInstance](https://bbotk.mlr-org.com/reference/EvalInstance.html)
to support both optimization and active learning. Holds the objective,
search space, archive, and terminator, and provides the evaluation loop
mechanics.

Unlike
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html),
this class:

- Supports codomains with "learn" tags (in addition to
  minimize/maximize)

- Does not compute `objective_multiplicator`

- Does not assign a "result" (best point)

Creates a new SearchInstance.

Evaluates a batch of points and adds results to the archive.

Resets the instance for a fresh search.

Printer.

## Arguments

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  The objective to evaluate. Can be any bbotk Objective subclass
  including our ObjectiveDataset and ObjectiveLearner.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Optional restricted search space. If NULL:

  - If the domain contains no TuneTokens, uses the whole domain

  - If the domain contains TuneTokens, derives search space from them
    Cannot be supplied if the domain already contains TuneTokens.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  When to stop the search. Uses bbotk terminators.

- archive:

  ([bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html))  
  Optional pre-existing archive. If NULL, creates a new one.

- check_values:

  (`logical(1)`)  
  Whether to validate points against search_space before evaluation.

- callbacks:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))  
  Optional callbacks to hook into the evaluation loop.

- xdt:

  (`data.table`)  
  Points to evaluate, one row per configuration.

- ...:

  (ignored).

## Value

Invisibly returns the ydt (codomain values).

## Details

The search instance serves as the "problem specification" for custom
search loops. It owns the archive and handles evaluation mechanics.

If you want to run a full MBO-style loop, prefer using bbotk's
[bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
/
[bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html)
together with
[mlr3mbo::OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.html).
For codomains containing `"learn"` targets, use
[ResultAssignerNull](https://celecx.mlr-org.com/reference/ResultAssignerNull.md)
to disable assigning a "best" result.

Since bbotk's Codomain now natively supports "learn" tags, codomains are
passed directly to ArchiveBatch without conversion. Note that calling
`archive$best()` or `archive$nds_selection()` will error if the codomain
contains only "learn" targets, which is the correct behavior.

## Callbacks

Callbacks can be registered to hook into the evaluation loop. The
following stages are supported:

- `on_optimizer_before_eval`: Called before evaluating a batch

- `on_optimizer_after_eval`: Called after evaluating a batch

## Termination

Before each batch evaluation, the terminator is checked. If terminated,
a `search_terminated_error` condition is raised. This can be caught with
`tryCatch(..., search_terminated_error = function(e) ...)`.
