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
[`AcqFunctionDist`](https://celecx.mlr-org.com/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://celecx.mlr-org.com/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIDEAL`](https://celecx.mlr-org.com/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionDistIGS`](https://celecx.mlr-org.com/reference/AcqFunctionDistIGS.md),
[`SurrogateNull`](https://celecx.mlr-org.com/reference/SurrogateNull.md)
