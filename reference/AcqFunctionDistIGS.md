# Distance-Aware iGS Acquisition Function

Improved greedy-sampling acquisition function using an
[ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md).

The score is the minimum, over already evaluated reference points, of
the product of input-space distance and output-space distance.

Creates a new distance-aware iGS acquisition function.

## Arguments

- surrogate:

  (`NULL` \|
  [mlr3mbo::SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.html))  
  Surrogate used for mean predictions.

- al_distance:

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md)
  \| `NULL`)  
  Distance used for input-space scores.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://mlr-org.github.io/celecx/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIDEAL`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionGSy`](https://mlr-org.github.io/celecx/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://mlr-org.github.io/celecx/reference/SurrogateNull.md)
