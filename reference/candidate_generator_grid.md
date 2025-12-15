# Grid Candidate Generator

Generates candidates on a regular grid. Only suitable for
low-dimensional problems or when specific resolution is needed.

## Usage

``` r
candidate_generator_grid(resolution = 10L)
```

## Arguments

- resolution:

  (`integer(1)` \| `named integer()`)  
  Number of points per dimension. Can be a single value applied to all
  dimensions, or a named vector with per-dimension values.

## Value

A candidate generator function.

## Details

Warning: Grid size grows exponentially with dimension. For d dimensions
with resolution r, produces r^d points.

Uses
[`paradox::generate_design_grid()`](https://paradox.mlr-org.com/reference/generate_design_grid.html)
internally.
