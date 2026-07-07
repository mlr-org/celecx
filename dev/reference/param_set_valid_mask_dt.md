# Get `logical(nrow(dt))` mask of valid configurations in a data.table

Returns a `logical(nrow(dt))` indicating, for each row, whether the
configuration is valid in the `ParamSet`.

## Usage

``` r
param_set_valid_mask_dt(dt, param_set)
```

## Arguments

- dt:

  (`data.table`)  
  `data.table` to validate.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  `ParamSet` describing the space to validate against.
