# Syntactic Sugar Active Learning Distance Construction

This function complements
[mlr_al_distances](https://celecx.mlr-org.com/reference/mlr_al_distances.md)
with a construction helper in the spirit of `mlr_sugar`.

## Usage

``` r
clx_ald(.key, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to
  [mlr_al_distances](https://celecx.mlr-org.com/reference/mlr_al_distances.md).

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to the parameter set, or to
  public fields. See
  [`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for details.

## Value

[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md).

## Examples

``` r
clx_ald("standardize")
#> <ALDistanceStandardize:standardize> 
```
