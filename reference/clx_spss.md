# Syntactic Sugar Space Samplers Construction

Retrieve multiple space samplers from
[mlr_space_samplers](https://celecx.mlr-org.com/reference/mlr_space_samplers.md).

## Usage

``` r
clx_spss(.keys, ...)
```

## Arguments

- .keys:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Keys passed to
  [mlr_space_samplers](https://celecx.mlr-org.com/reference/mlr_space_samplers.md).

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to the parameter set, or to
  public fields. See
  [`mlr3misc::dictionary_sugar_mget()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for details.

## Value

Named `list` of
[SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md)
objects.

## Examples

``` r
clx_spss(c("uniform", "gsx"))
#> $uniform
#> <SpaceSamplerUniform:uniform> 
#> 
#> $gsx.standardize
#> <SpaceSamplerGSx:gsx.standardize> 
#> 
```
