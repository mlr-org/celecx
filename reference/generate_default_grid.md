# Generate Default Grid

Generates a default evaluation grid from a search space using Latin
Hypercube Sampling.

## Usage

``` r
generate_default_grid(search_space, n = 100L)
```

## Arguments

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  The search space to sample from.

- n:

  (`integer(1)`) Number of grid points. Default 100.

## Value

`data.table` with one row per grid point.
