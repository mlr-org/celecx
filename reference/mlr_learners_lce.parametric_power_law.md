# Parametric Power-Law LCE Learner

Fits a three-parameter power-law learning curve \$\$f(b) = c +
a\\b^{-k}\$\$ to the per-batch surrogate performance. Compared to
[LearnerLCEParametricExponential](https://mlr-org.github.io/celecx/reference/mlr_learners_lce.parametric_exponential.md)
the power-law family decays more slowly, matching the heavier-tailed
convergence often seen in sample-complexity bounds.

The curve is fit on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md) scale
and standard errors and quantiles are computed exactly as for
[LearnerLCEParametricExponential](https://mlr-org.github.io/celecx/reference/mlr_learners_lce.parametric_exponential.md)
(link-scale epistemic `se_epistemic` plus total predictive `se`).

Training batches with `batch_nr <= 0` are not supported because
\\b^{-k}\\ is then undefined; such tasks raise an error.

Creates a new instance of this learner.

## Parameters

- `asymptote_init`, `amplitude_init`, `rate_init` :: `numeric(1)`  
  Initial values for `c`, `a`, and `k`.

- `rate_lower` :: `numeric(1)`  
  Lower bound for the decay exponent `k`. Initialized to `1e-6`.

- `maxit` :: `integer(1)`  
  Maximum optim iterations. Initialized to `500`.
