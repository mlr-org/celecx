# Topologically sort parameter IDs by dependency order

"Parent" parameters that other parameters depend on are sorted first.

## Usage

``` r
param_set_topo_ids(param_set)
```

## Arguments

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set to sort.

## Value

[`character()`](https://rdrr.io/r/base/character.html) of parameter IDs
in topological order.
