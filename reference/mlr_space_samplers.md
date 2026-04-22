# Dictionary of Space Samplers

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[SpaceSampler](https://mlr-org.github.io/celecx/reference/SpaceSampler.md).
Each sampler has an associated help page, see `mlr_space_samplers_[id]`.

For a more convenient way to retrieve and construct a sampler, see
[`clx_sps()`](https://mlr-org.github.io/celecx/reference/clx_sps.md) and
[`clx_spss()`](https://mlr-org.github.io/celecx/reference/clx_spss.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar functions:
[`clx_sps()`](https://mlr-org.github.io/celecx/reference/clx_sps.md),
[`clx_spss()`](https://mlr-org.github.io/celecx/reference/clx_spss.md)

Other Dictionary:
[`mlr_al_distances`](https://mlr-org.github.io/celecx/reference/mlr_al_distances.md)

Other SpaceSampler:
[`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md),
[`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md),
[`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md),
[`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md),
[`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md),
[`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md),
[`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md),
[`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md),
[`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md),
[`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md),
[`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)

## Examples

``` r
as.data.table(mlr_space_samplers)
#> Key: <key>
#>                  key                            label
#>               <char>                           <char>
#> 1:             chain            Chained Space Sampler
#> 2:       conditional        Conditional Space Sampler
#> 3:               gsx                GSx Space Sampler
#> 4:            kmeans            K-Means Space Sampler
#> 5:          kmedoids          K-Medoids Space Sampler
#> 6:               lhs                LHS Space Sampler
#> 7: relational_kmeans Relational K-Means Space Sampler
#> 8:             sobol              Sobol Space Sampler
#> 9:           uniform            Uniform Space Sampler
#>                                             man deterministic
#>                                          <char>        <lgcl>
#> 1:             celecx::mlr_space_samplers_chain          TRUE
#> 2:       celecx::mlr_space_samplers_conditional          TRUE
#> 3:               celecx::mlr_space_samplers_gsx         FALSE
#> 4:            celecx::mlr_space_samplers_kmeans         FALSE
#> 5:          celecx::mlr_space_samplers_kmedoids          TRUE
#> 6:               celecx::mlr_space_samplers_lhs         FALSE
#> 7: celecx::mlr_space_samplers_relational_kmeans         FALSE
#> 8:             celecx::mlr_space_samplers_sobol         FALSE
#> 9:           celecx::mlr_space_samplers_uniform          TRUE
clx_sps("uniform")
#> <SpaceSamplerUniform:uniform> 
```
