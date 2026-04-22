# Syntactic Sugar Active Learning Distances Construction

Retrieve multiple active-learning distances from
[mlr_al_distances](https://mlr-org.github.io/celecx/reference/mlr_al_distances.md).

## Usage

``` r
clx_alds(.keys, ...)
```

## Arguments

- .keys:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Keys passed to
  [mlr_al_distances](https://mlr-org.github.io/celecx/reference/mlr_al_distances.md).

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to the parameter set, or to
  public fields. See
  [`mlr3misc::dictionary_sugar_mget()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for details.

## Value

Named `list` of
[ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md)
objects.

## Examples

``` r
clx_alds(c("standardize", "affine"))
#> $standardize
#> <ALDistanceStandardize:standardize> 
#> 
#> $affine
#> <ALDistanceAffine:affine> 
#> 
```
