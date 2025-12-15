# Sample in Local Region

Generates random points in a hypercube around a center point.

## Usage

``` r
sample_local(center, n, radius, search_space)
```

## Arguments

- center:

  (`data.table` or `list`) Center point.

- n:

  (`integer(1)`) Number of points to generate.

- radius:

  (`numeric(1)`) Radius as fraction of domain range.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  The search space.

## Value

`data.table` of n points.
