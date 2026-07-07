# Monotone Spline LCE Learner

Fits a shape-constrained smoothing spline (monotone increasing or
monotone decreasing) to the per-batch surrogate performance via
[`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html) with a monotone
P-spline basis (`"mpi"` / `"mpd"`). With `direction = "auto"` (the
default), the direction is inferred from the task's measure:
utility-style targets increase and loss-style targets decrease. This is
the smoother, lower-bias analogue of
[LearnerLCEIsotonic](https://mlr-org.github.io/celecx/reference/mlr_learners_lce.isotonic.md).

Extrapolation beyond the training-batch range linearly extends the last
fitted spline segment, so very long-horizon forecasts can drift; use
[LearnerLCEParametricExponential](https://mlr-org.github.io/celecx/reference/mlr_learners_lce.parametric_exponential.md)
or one of the parametric families when a hard asymptote is needed. The
spline is fit on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale.

At least five distinct batches are required: `scam`'s monotone basis
needs a basis dimension `k` of at least 4, and `k` is clamped to at most
`n_batches - 1` to keep the fit identifiable.

When `predict_type = "se"` the learner returns the spline's pointwise
standard errors (as reported by
[`scam::predict.scam()`](https://rdrr.io/pkg/scam/man/predict.scam.html))
as the epistemic `se_epistemic` and adds the spline's residual variance
back to form the total predictive `se`, both on the link scale.

Creates a new instance of this learner.

## Parameters

- `direction` :: `character(1)`  
  `"auto"` (default), `"increasing"`, or `"decreasing"`. Selects the
  monotone P-spline basis (`mpi` / `mpd`) after resolution.

- `k` :: `integer(1)` \| `NULL`  
  Basis dimension passed to `scam::s()`. Defaults to `NULL`, which uses
  `min(10, n_batches - 1)`. Floored at `4` (the smallest `k` the
  monotone basis supports) and clamped to at most `n_batches - 1` at
  training time to keep the fit identifiable.

- `bs` :: `character(1)`  
  Spline basis. Restricted to `"mpi"` (monotone increasing) and `"mpd"`
  (monotone decreasing) and is set automatically from `direction`.
