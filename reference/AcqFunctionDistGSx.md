# Distance-Aware GSx Acquisition Function

Greedy-sampling-in-input-space acquisition function using an
[ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md).

The score is the distance to the nearest already evaluated reference
point.

Creates a new distance-aware GSx acquisition function.

## Arguments

- surrogate:

  (`NULL` \|
  [mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html))  
  Optional surrogate. Only used to derive the acquisition domain and
  archive reference points.

- al_distance:

  ([ALDistance](https://celecx.mlr-org.com/reference/ALDistance.md) \|
  `NULL`)  
  Distance used for input-space scores.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://celecx.mlr-org.com/reference/AcqFunctionDist.md),
[`AcqFunctionDistIDEAL`](https://celecx.mlr-org.com/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionDistIGS`](https://celecx.mlr-org.com/reference/AcqFunctionDistIGS.md),
[`AcqFunctionGSy`](https://celecx.mlr-org.com/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://celecx.mlr-org.com/reference/SurrogateNull.md)
