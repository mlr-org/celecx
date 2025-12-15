# Compute Domain Volume

Computes the volume of the search space (product of ranges for numeric
parameters, product of level counts for categorical).

## Usage

``` r
compute_domain_volume(search_space)
```

## Arguments

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  The search space.

## Value

`numeric(1)` The domain volume.

## Details

For mixed numeric/categorical spaces, this returns the product of:

- (upper - lower) for each numeric parameter

- Number of levels for each categorical parameter
