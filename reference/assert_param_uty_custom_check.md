# Assert `ParamUty` Custom Checks for a data.table Column

Applies a `ParamUty` parameter's `custom_check` to each non-special
entry of a column and raises an informative error for the first failing
row.

## Usage

``` r
assert_param_uty_custom_check(
  col,
  param_id,
  custom_check,
  special_vals,
  .dt_name
)
```

## Arguments

- col:

  Column values to validate.

- param_id:

  (`character(1)`)  
  Parameter identifier used in error messages.

- custom_check:

  (`function`)  
  Custom validation function stored on the `ParamUty`.

- special_vals:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Special values that bypass the custom check.

- .dt_name:

  (`character(1)`)  
  Name of the `data.table` for error messages.

## Value

`TRUE` invisibly if all checked values pass.
