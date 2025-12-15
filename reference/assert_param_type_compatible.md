# Assert Parameter Type Compatibility

Validates that a parameter class is compatible with an actual data type.

## Usage

``` r
assert_param_type_compatible(
  param_id,
  param_class,
  actual_type,
  context = "data"
)
```

## Arguments

- param_id:

  (`character(1)`)  
  Parameter identifier for error messages.

- param_class:

  (`character(1)`)  
  ParamSet class (e.g., "ParamDbl", "ParamInt", "ParamFct", "ParamLgl").

- actual_type:

  (`character(1)`)  
  Actual data type (e.g., "numeric", "integer", "factor", "logical").

- context:

  (`character(1)`)  
  Context for error messages (e.g., "data column", "training feature").

## Value

`TRUE` invisibly if compatible, otherwise throws an error.
