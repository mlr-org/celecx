# Split-Conformal LCE Learner Wrapper

Wraps an arbitrary base
[LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md)
with a split-conformal procedure that calibrates an absolute-residual
prediction band on a hold-out suffix of the training batches.

Training proceeds as:

1.  The last `n_calibration_batches` batches of the training task are
    set aside as the calibration set. The base learner is fit on the
    remaining prefix (the "proper" training set).

2.  The base learner predicts on the calibration set and per-batch
    absolute residuals \\\|y_b - \hat y_b\|\\ are collected.

3.  The conformal half-width \\q\\ is set to the \\\lceil
    (n\_{cal}+1)(1-\alpha)\rceil / n\_{cal}\\ sample quantile of the
    calibration residuals (the standard finite-sample correction).

At predict time the base learner trained on the proper-train subset is
used for the point prediction. Calibration residuals are measured on the
task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale. The `se` column, when requested, is the constant per-row value
\\q / z\_{1-\alpha/2}\\ (with \\z\\ the standard normal quantile and
\\q\\ the link-scale conformal half-width). Multiplying back by
\\z\_{1-\alpha/2}\\ recovers the \\(1-\alpha)\\ conformal half-width on
the link scale, so the downstream interpretation `g(response) ± z * se`
produces an interval with the chosen conformal coverage. Since the
split-conformal band is a total predictive band that does not separate
epistemic from aleatoric uncertainty, `se_epistemic` is reported equal
to `se`.

The `target_reached` predict type reports the realised-reach probability
(whether the *observed* `y_b` reaches the target), reading the same
total band as a Gaussian on the link scale. This matches the other
learners' `target_reached` and
[mlr_measures_lce_distributional](https://mlr-org.github.io/celecx/reference/mlr_measures_lce_distributional.md)'s
`lce.reach_brier`.

Two consequences of the band not separating epistemic from aleatoric
uncertainty: a distributional measure scoring at a `level` other than
`1 - alpha` rescales `se` and only attains its nominal coverage when
`level == 1 - alpha`; and
[lce_batches_to_target](https://mlr-org.github.io/celecx/reference/lce_batches_to_target.md)
with `crossing = "expected"` (which reads `se_epistemic`) conflates the
two and so behaves like an observed-crossing forecast for this learner.

Because the LCE batches are naturally ordered, the calibration split is
deterministic: the most recent batches go to calibration, matching the
"use the freshest history to calibrate the forecaster" intuition.

Creates a new conformal-wrapped LCE learner.

## Arguments

- learner:

  ([LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md))  
  Base LCE learner to wrap.

## Parameters

Combined parameter set merges the wrapper's own parameters with those of
the base learner via
[paradox::ParamSetCollection](https://paradox.mlr-org.com/reference/ParamSetCollection.html);
the base learner's parameters carry the `base.` prefix (e.g.
`base.rate_lower`).

Own parameters:

- `n_calibration_batches` :: `integer(1)`  
  Number of trailing batches used as the calibration set. Initialized to
  `3`. The proper-train set must contain at least one batch.

- `alpha` :: `numeric(1)`  
  Miscoverage level. Initialized to `0.1` (90% conformal coverage).
