# Distance-Aware IDEAL Acquisition Function

Inverse-distance-weighted active-learning acquisition function using an
[ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md).

The basic score is an IDW-weighted squared residual plus the IDEAL
exploration term. With `omega > 0`, `$fit_pool()` precomputes the IDEAL
pool-density multiplier on the fitted candidate pool. When fitted with
`xdt = NULL`, the population-case density multiplier is set to `1`.

Creates a new distance-aware IDEAL acquisition function.

Clears precomputed density values.

## Arguments

- surrogate:

  (`NULL` \|
  [mlr3mbo::SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.html))  
  Surrogate used for mean predictions.

- al_distance:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md)
  \| `NULL`)  
  Distance used for the IDW geometry.

- delta:

  (`numeric(1)`)  
  Exploration weight.

- omega:

  (`numeric(1)`)  
  Density multiplier weight.

- tolerance_equality:

  (`numeric(1)`)  
  Squared-distance tolerance for the exact-match branch.

## Value

`self`.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://mlr-org.github.io/celecx/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIGS`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIGS.md),
[`AcqFunctionGSy`](https://mlr-org.github.io/celecx/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://mlr-org.github.io/celecx/reference/SurrogateNull.md)
