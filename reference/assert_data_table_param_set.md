# Validate `data.table` Against `ParamSet`

Validates that a `data.table` is compatible with a given `ParamSet`.
Checks column presence, types, bounds, and uniqueness within the domain
and does type coercion if necessary.

## Usage

``` r
assert_data_table_param_set(
  dt,
  param_set,
  require_uniqueness = TRUE,
  min_rows = 1L,
  presence = "all",
  allow_untyped = FALSE,
  allow_extra = TRUE,
  .param_set_name = "param_set",
  .dt_name = "dt"
)
```

## Arguments

- dt:

  (`data.table`)  
  `data.table` to validate.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  `ParamSet` describing the space to validate against.

- require_uniqueness:

  (`logical(1)`)  
  Whether to require uniqueness within the space. Default is `TRUE`.

- min_rows:

  (`integer(1)`)  
  Minimum number of rows required in `dt`. Default is `1L`.

- presence:

  (`character(1)`)  
  Whether all parameters must be present as columns in `dt`. Use `"all"`
  (default) to require all parameters, or `"subset"` to allow missing
  parameter columns and validate them as typed `NA` values.

- allow_untyped:

  (`logical(1)`)  
  Whether to allow `p_uty` in the `ParamSet`. Default is `FALSE`.
  `p_uty` columns are not checked for type compatibility. Their
  `custom_check` is applied when present.

- allow_extra:

  (`logical(1)`)  
  Whether columns outside the `ParamSet` are allowed. Default is `TRUE`.

- .param_set_name:

  (`character(1)`)  
  Name of the `ParamSet` for error messages. Default is `"param_set"`.

- .dt_name:

  (`character(1)`)  
  Name of the `data.table` for error messages. Default is `"dt"`.

## Value

The validated and possibly coerced `data.table` (modified by reference).

## Details

Extra columns are allowed and are passed through unchanged when
`allow_extra = TRUE`. Missing parameter columns are rejected unless
`presence = "subset"`, in which case they are validated as typed `NA`
values on an internal copy. When `require_uniqueness` is `TRUE`, tables
with rows that are duplicated within the space of the `ParamSet` are
rejected, even if they differ by other columns.
