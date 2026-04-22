# Distance-Aware IDEAL Acquisition Function

Inverse-distance-weighted active-learning acquisition function using an
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md).

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

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md) \|
  `NULL`)  
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
[`AcqFunctionDist`](https://celecx.mlr-org.com/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://celecx.mlr-org.com/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIGS`](https://celecx.mlr-org.com/reference/AcqFunctionDistIGS.md),
[`AcqFunctionGSy`](https://celecx.mlr-org.com/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://celecx.mlr-org.com/reference/SurrogateNull.md)
