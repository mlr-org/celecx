# Generate a Dependency-Aware Grid Design

Generate the same configurations as
[`paradox::generate_design_grid()`](https://paradox.mlr-org.com/reference/generate_design_grid.html),
while avoiding avoidable Cartesian blow-up from duplicated per-parameter
grid values and from parameters that are inactive because of
dependencies.

## Usage

``` r
generate_design_grid_celecx(
  param_set,
  resolution = NULL,
  param_resolutions = NULL,
  upper_limit = NULL
)
```

## Arguments

- param_set:

  ([`paradox::ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)).

- resolution:

  (`integer(1)`)  
  Global resolution for all numerical parameters.

- param_resolutions:

  (named [`integer()`](https://rdrr.io/r/base/integer.html))  
  Resolution per
  [`paradox::Domain`](https://paradox.mlr-org.com/reference/Domain.html),
  named by parameter ID.

- upper_limit:

  (`integer(1)` \| `NULL`)  
  Optional upper bound on the number of rows to materialize. An error is
  raised before building the full grid if the design would exceed this
  limit.

## Value

[`paradox::Design`](https://paradox.mlr-org.com/reference/Design.html).
