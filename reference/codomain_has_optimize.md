# Check if Codomain Has Optimization Targets

Tests whether a codomain contains any minimize/maximize tagged
parameters.

## Usage

``` r
codomain_has_optimize(codomain)
```

## Arguments

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The codomain to check.

## Value

`logical(1)` TRUE if any parameter has minimize or maximize tag.
