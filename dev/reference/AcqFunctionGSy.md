# GSy Acquisition Function

Greedy-sampling-in-output-space acquisition function.

The score is the minimum absolute distance between a candidate
prediction and an already observed target value.

Creates a new GSy acquisition function.

## Arguments

- surrogate:

  (`NULL` \|
  [mlr3mbo::SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.html))  
  Surrogate used for mean predictions.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIDEAL`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionDistIGS`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDistIGS.md),
[`SurrogateNull`](https://mlr-org.github.io/celecx/dev/reference/SurrogateNull.md)
