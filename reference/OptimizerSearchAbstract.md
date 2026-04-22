# Abstract Base Class for Search-Compatible Optimizers

Extends
[bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
with support for
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md)
in addition to
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).
Optimizers that should work with both
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
and
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md)
should inherit from this class instead of
[bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html).

Runs the optimizer on either an
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
or a
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md).

## Arguments

- inst:

  ([bbotk::EvalInstance](https://bbotk.mlr-org.com/reference/EvalInstance.html))  
  Either an
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  (delegated to
  [bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html))
  or a
  [SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md)
  (handled by a custom loop).

## Value

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
for
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html),
`NULL` invisibly otherwise.

## Details

The default `.assign_result()` in
[bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
assumes an
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
and errors on a
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md).
This class overrides `.assign_result()` to handle both: it delegates to
[`bbotk::assign_result_default()`](https://bbotk.mlr-org.com/reference/assign_result_default.html)
for
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
and is a no-op for
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md)
(which has no result slot).

For
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md),
`$optimize()` runs a custom batch-style loop that mirrors
[`bbotk::optimize_batch_default()`](https://bbotk.mlr-org.com/reference/optimize_batch_default.html)
but accepts the broader
[bbotk::EvalInstance](https://bbotk.mlr-org.com/reference/EvalInstance.html)
base class and catches `terminated_error` (the condition class raised by
[`search_terminated_error()`](https://mlr-org.github.io/celecx/reference/search_terminated_error.md)).
