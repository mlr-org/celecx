# Distance-Aware iGS Acquisition Function

Improved greedy-sampling acquisition function using an
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md).

The score is the minimum, over already evaluated reference points, of
the product of input-space distance and output-space distance.

Creates a new distance-aware iGS acquisition function.

## Arguments

- surrogate:

  (`NULL` \|
  [mlr3mbo::SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.html))  
  Surrogate used for mean predictions.

- al_distance:

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md) \|
  `NULL`)  
  Distance used for input-space scores.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://celecx.mlr-org.com/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://celecx.mlr-org.com/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIDEAL`](https://celecx.mlr-org.com/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionGSy`](https://celecx.mlr-org.com/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://celecx.mlr-org.com/reference/SurrogateNull.md)
