# Assert Pool Objective Search-Space Compatibility

Validates search-space features that pool-backed objectives cannot
support.

## Usage

``` r
assert_pool_objective_search_space(objective, search_space)
```

## Arguments

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  Objective to evaluate.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space used to propose points.

## Value

The validated `search_space`, invisibly.
