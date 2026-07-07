# Learning Curve Extrapolation Learner

Abstract base class for learners that extrapolate the surrogate-model
quality trajectory of an active-learning run. Subclasses fit a model on
a [TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
and produce point and (optionally) distributional predictions of the
surrogate's performance at unseen `batch_nr` values.

The supported predict types are:

- `"response"`: the point forecast (predictive median on the natural
  scale).

- `"se"`: additionally the total predictive standard error `se` and the
  epistemic standard error of the mean `se_epistemic`, both on the
  task's
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  scale (see
  [PredictionLCE](https://mlr-org.github.io/celecx/dev/reference/PredictionLCE.md)).

- `"quantiles"`: a matrix of predictive quantiles at the probabilities
  given by the `quantile_probs` parameter.

- `"samples"`: a matrix of predictive draws (joint sample paths).

- `"target_reached"`: a matrix of reach probabilities at the targets
  given by the `reach_target` parameter; needs the task's optimization
  direction (from the task `measure`, or from a directed codomain for
  best-so-far tasks).

Subclasses that model the curve as Gaussian on the link scale assemble
their predictions with `lce_distr_predict()`; subclasses that produce
explicit predictive draws use `lce_samples_predict()`.

Creates a new LCE learner.

## Arguments

- id:

  (`character(1)`)  
  Learner id.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)).

- predict_types:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  One or more of `"response"`, `"se"`, `"quantiles"`, `"samples"`,
  `"target_reached"`.

- feature_types:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported feature types. Defaults to `"integer"` (the type of
  `batch_nr`).

- properties:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- packages:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- label:

  (`character(1)`).

- man:

  (`character(1)`).

## Parameters

Depending on the supported predict types, the following parameters are
added automatically:

- `quantile_probs` ::
  [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Probabilities for the `"quantiles"` predict type. Defaults to
  `c(0.05, 0.25, 0.5, 0.75, 0.95)`.

- `reach_target` :: [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Target value(s) for the `"target_reached"` predict type. No default.
