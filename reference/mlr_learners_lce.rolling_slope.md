# Rolling Slope LCE Learner

Baseline
[LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md)
that fits a straight line through the most recent `window` per-batch
training performances and linearly extrapolates it to the requested test
batches.

This is a stronger "no-curvature" baseline than the constant predictors
of
[LearnerLCEFeatureless](https://mlr-org.github.io/celecx/reference/mlr_learners_lce.featureless.md)
and matches the "rolling slope" baseline from the celecx research plan.
The line is fit on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale.

When `predict_type = "se"`, the standard errors come from the ordinary
least-squares fit: `se_epistemic` is the SD of the fitted mean (so it
*grows* as the requested batch moves away from the window, reflecting
extrapolation uncertainty) and `se` adds the residual variance back to
form the total predictive SD. Both are on the link scale and are `NA`
when fewer than three window batches make the residual variance
undefined.

Creates a new instance of this learner.

## Parameters

- `window` :: `integer(1)`  
  Number of most recent batches used for the slope fit. Initialized to
  `5`. The learner uses `min(window, n_training_batches)` batches.
