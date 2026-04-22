# Distance-Aware GSx Acquisition Function

Greedy-sampling-in-input-space acquisition function using an
[ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md).

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

  ([ALDistance](https://mlr-org.github.io/celecx/reference/ALDistance.md)
  \| `NULL`)  
  Distance used for input-space scores.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://mlr-org.github.io/celecx/reference/AcqFunctionDist.md),
[`AcqFunctionDistIDEAL`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionDistIGS`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIGS.md),
[`AcqFunctionGSy`](https://mlr-org.github.io/celecx/reference/AcqFunctionGSy.md),
[`SurrogateNull`](https://mlr-org.github.io/celecx/reference/SurrogateNull.md)
