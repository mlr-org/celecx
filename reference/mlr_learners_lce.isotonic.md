# Isotonic LCE Learner

Fits a monotone, piecewise-constant learning curve via isotonic
regression ([`stats::isoreg()`](https://rdrr.io/r/stats/isoreg.html)) to
the per-batch surrogate performance. With `direction = "auto"` (the
default), the direction is inferred from the task's measure:
utility-style targets (R², accuracy, ...) increase and loss-style
targets (MAE, RMSE, ...) decrease. Set `direction = "increasing"` or
`"decreasing"` to override this; the implementation negates the target
before fitting for the decreasing case.

Predictions outside the observed batch range are extrapolated as
constants of the closest endpoint fit, matching the monotone-shape
constraint. Within the observed range either piecewise-constant (step
function) or piecewise-linear interpolation is used, controlled by
`interpolation`. The fit is performed on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale.

When `predict_type = "se"` the epistemic `se_epistemic` is a constant
link-scale residual standard deviation divided by `sqrt(n_batches)`, and
the total predictive `se` adds that residual standard deviation back as
the aleatoric spread. The residual standard deviation uses
`n_batches - 1` degrees of freedom (it does not discount the degrees of
freedom the monotone fit itself consumes) and is constant across
batches, so the uncertainty is a coarse estimate that ignores how the
fit degrades away from the observed batches.

Creates a new instance of this learner.

## Parameters

- `direction` :: `character(1)`  
  `"auto"` (default), `"increasing"`, or `"decreasing"`.

- `interpolation` :: `character(1)`  
  `"linear"` (default) or `"constant"` (step). Controls within-range
  interpolation; out-of-range extrapolation is always constant.
