# Get Optimization Target IDs

Returns IDs of parameters tagged for optimization (minimize or
maximize), excluding "learn" targets.

## Usage

``` r
codomain_optimize_ids(codomain)
```

## Arguments

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The codomain to inspect.

## Value

Character vector of optimization target IDs.
