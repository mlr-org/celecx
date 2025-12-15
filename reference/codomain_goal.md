# Determine Goal from Codomain

Infers the search goal based on codomain tags.

## Usage

``` r
codomain_goal(codomain)
```

## Arguments

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The codomain to inspect.

## Value

`character(1)`: "optimize", "learn", or "both".
