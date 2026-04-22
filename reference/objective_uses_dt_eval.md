# Detect Objectives with Native data.table Evaluation Semantics

Returns whether an objective explicitly advertises that its `eval_dt()`
implementation preserves the missing-value semantics expected by
[SearchInstance](https://mlr-org.github.io/celecx/reference/SearchInstance.md).

## Usage

``` r
objective_uses_dt_eval(objective)
```

## Arguments

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  Objective to inspect.

## Value

`logical(1)`.

## Details

This is an explicit S3 capability instead of a method-body heuristic.
Merely overriding `eval_dt()` is not sufficient: subclasses can wrap or
forward to
[bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html)'s
default `eval_dt()`, which still converts rows with `transpose_list()`
and therefore treats dependency-inactive parameters as literal `NA`
values instead of dropping them. Native data-table objectives opt in
with an S3 method, and subclasses inherit that capability automatically.
