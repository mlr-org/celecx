# GAM LCE Learner

Fits an unconstrained smoothing spline to the per-batch performance via
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) with a single
smooth `s(batch_nr, k = k, bs = bs)`, estimated by REML. This is the
classic "GAM extrapolation with confidence bands" workflow for
learning-curve extrapolation. Unlike
[LearnerLCESplineMonotone](https://mlr-org.github.io/celecx/reference/mlr_learners_lce.spline_monotone.md)
it imposes no monotonicity, so it can also track non-monotone curves –
at the price of less stable extrapolation.

The curve is fit on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale. Extrapolation beyond the training-batch range extends the spline
basis (linearly beyond the boundary knots for the `"cr"` basis), so
long-horizon forecasts can drift; the pointwise standard errors grow
accordingly.

When `predict_type = "se"` the learner returns the spline's pointwise
standard errors (as reported by
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html))
as the epistemic `se_epistemic` and adds the residual variance back to
form the total predictive `se`, both on the link scale.

At least four distinct batches are required; `k` is clamped to at most
`n_batches - 1` to keep the fit identifiable.

Creates a new instance of this learner.

## Parameters

- `k` :: `integer(1)` \| `NULL`  
  Basis dimension passed to
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html). Defaults to
  `NULL`, which uses `min(6, n_batches - 1)`. Floored at `3` and clamped
  to at most `n_batches - 1` at training time.

- `bs` :: `character(1)`  
  Spline basis: `"cr"` (cubic regression spline, default; extrapolates
  linearly beyond the boundary knots) or `"tp"` (thin plate).
