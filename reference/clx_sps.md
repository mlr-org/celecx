# Syntactic Sugar Space Sampler Construction

This function complements
[mlr_space_samplers](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md)
with a construction helper in the spirit of `mlr_sugar`.

## Usage

``` r
clx_sps(.key, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to
  [mlr_space_samplers](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md).

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to the parameter set, or to
  public fields. See
  [`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for details.

## Value

[SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md).

## Examples

``` r
clx_sps("uniform")
#> <SpaceSamplerUniform:uniform> 
```
