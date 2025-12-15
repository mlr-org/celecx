# Choose Search Space

Derives the search space from the objective domain and optional
user-supplied search space. Follows the same logic as bbotk's
OptimInstanceBatch.

## Usage

``` r
choose_search_space(objective, search_space)
```

## Arguments

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  The objective with a domain ParamSet.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`)  
  Optional user-supplied search space.

## Value

A
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
to use as the search space.
