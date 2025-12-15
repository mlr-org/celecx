# Context for Search Instance

Context object that allows callbacks to access and modify data during
search operations on a
[SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md).

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- inst:

  ([SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md)).

- optimizer:

  ([bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
  \| `NULL`).

## Details

Similar to bbotk's `ContextBatch`, but for
[SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md)
instead of
[bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html).

## Fields

- `instance`:

  ([SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md)).

- `optimizer`:

  ([bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
  \| `NULL`).

- `xdt`:

  ([data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  The points of the latest batch in `instance$eval_batch()`.
