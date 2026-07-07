# Parametric Logistic LCE Learner

Fits a four-parameter logistic learning curve \$\$f(b) = \ell +
\frac{u - \ell}{1 + \exp(-k\\(b - b_0))}\$\$ to the per-batch surrogate
performance. Captures S-shaped trajectories with a lower asymptote
\\\ell\\, an upper asymptote \\u\\, a transition midpoint \\b_0\\, and a
steepness \\k \> 0\\. Decreasing trajectories are represented by
`upper < lower` rather than by a negative rate.

The curve is fit on the task's
[lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
scale. When `predict_type = "se"` the learner reports the epistemic
Gauss-Newton delta-method standard error `se_epistemic` and the total
predictive standard error `se` (adding the residual variance), both on
the link scale; predictive quantiles are the exact Normal quantiles of
that predictive.

Creates a new instance of this learner.

## Parameters

- `lower_init`, `upper_init`, `midpoint_init`, `rate_init` ::
  `numeric(1)`  
  Initial values for `lower`, `upper`, `midpoint`, and `rate`. Defaults
  are derived from the training data when unset.

- `rate_lower` :: `numeric(1)`  
  Lower bound for `rate`. Initialized to `1e-6`.

- `maxit` :: `integer(1)`  
  Maximum optim iterations. Initialized to `500`.
