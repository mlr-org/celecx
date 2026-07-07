# Learning Curve Extrapolation Measure

Abstract base class for performance measures evaluating the
surrogate-quality forecast produced by a
[LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md).
Concrete measures compare the predicted per-batch performance to the
recorded performance.

Measures defined via this base class always aggregate predictions and
truths to one value per batch (the within-batch mean) before computing
the per-batch loss. Multiple archive rows belong to the same batch and
share the same surrogate-quality value, so the aggregation collapses
them without affecting the loss; if a future learner predicts
differently within a batch, the mean is the natural reduction.

Creates a new LCE measure.

## Arguments

- id:

  (`character(1)`)  
  Measure id.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)).

- range:

  (`numeric(2)`)  
  Theoretical range of values.

- minimize:

  (`logical(1)`).

- average:

  (`character(1)`).

- aggregator:

  (`function()` or `NULL`).

- properties:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- predict_type:

  (`character(1)`)  
  Any lce predict type (see
  [PredictionLCE](https://mlr-org.github.io/celecx/reference/PredictionLCE.md)),
  e.g. `"response"`, `"se"`, `"quantiles"`.

- predict_sets:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- task_properties:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- label:

  (`character(1)`).

- man:

  (`character(1)`).
