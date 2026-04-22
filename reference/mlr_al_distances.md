# Dictionary of Active Learning Distances

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md). Each
distance has an associated help page, see `mlr_al_distances_[id]`.

For a more convenient way to retrieve and construct a distance, see
[`clx_ald()`](https://celecx.mlr-org.com/reference/clx_ald.md) and
[`clx_alds()`](https://celecx.mlr-org.com/reference/clx_alds.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar functions:
[`clx_ald()`](https://celecx.mlr-org.com/reference/clx_ald.md),
[`clx_alds()`](https://celecx.mlr-org.com/reference/clx_alds.md)

Other Dictionary:
[`mlr_space_samplers`](https://celecx.mlr-org.com/reference/mlr_space_samplers.md)

Other ALDistance:
[`ALDistance`](https://celecx.mlr-org.com/reference/ALDistance.md),
[`ALDistanceGeometry`](https://celecx.mlr-org.com/reference/ALDistanceGeometry.md),
[`mlr_al_distances_affine`](https://celecx.mlr-org.com/reference/mlr_al_distances_affine.md),
[`mlr_al_distances_gower`](https://celecx.mlr-org.com/reference/mlr_al_distances_gower.md),
[`mlr_al_distances_standardize`](https://celecx.mlr-org.com/reference/mlr_al_distances_standardize.md)

## Examples

``` r
as.data.table(mlr_al_distances)
#> Key: <key>
#>            key                    label                                  man
#>         <char>                   <char>                               <char>
#> 1:      affine       Affine AL Distance      celecx::mlr_al_distances_affine
#> 2:       gower        Gower AL Distance       celecx::mlr_al_distances_gower
#> 3: standardize Standardized AL Distance celecx::mlr_al_distances_standardize
clx_ald("standardize")
#> <ALDistanceStandardize:standardize> 
```
