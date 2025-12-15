# Validate Dataset Against Domain and Codomain

Validates that a dataset is compatible with a given domain and codomain.
Checks column presence, types, bounds, and factor levels.

## Usage

``` r
assert_dataset_domain_codomain(dt, domain, codomain)
```

## Arguments

- dt:

  (`data.table`)  
  The dataset to validate.

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the input space.

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the output space.

## Value

The validated and possibly coerced `data.table` (modified by reference).

## Details

Type expectations:

- `ParamDbl` -\> numeric column

- `ParamInt` -\> integerish column (coerced to integer)

- `ParamFct` -\> factor or character column (character converted to
  factor with domain levels)

- `ParamLgl` -\> logical column
