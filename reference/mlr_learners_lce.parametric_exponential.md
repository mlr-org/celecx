# Parametric Exponential LCE Learner

Fits a three-parameter exponential learning curve \$\$f(b) = c + a
\exp(-k\\b)\$\$ to the per-batch surrogate performance. Suitable both
for performance measures that grow to an asymptote (`a` ends up
negative) and for loss measures that decay to an asymptote (`a` ends up
positive).

The curve is fit on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale, so for a non-identity link the family above describes `g(f(b))`
and the natural-scale prediction is `g^{-1}` of the fitted curve.

When `predict_type = "se"` the learner reports two link-scale standard
errors: `se_epistemic`, the Gauss-Newton delta-method SD of the mean
curve (parameter covariance \\\hat\sigma^2 (H/2)^{-1}\\ propagated
through the gradient of `f`), and `se`, the total predictive SD
`sqrt(se_epistemic^2 + sigma2)` that adds the residual variance back. If
the Hessian is singular or the residual degrees of freedom are
non-positive, both are `NA`. Predictive quantiles
(`predict_type = "quantiles"`) are the exact Normal quantiles of that
predictive, back-transformed to the natural scale.

Multiple archive rows belonging to the same batch are collapsed via the
batch-wise mean of `target` before fitting; ties always agree by
construction when the upstream task was produced by
[CallbackSurrogatePerformance](https://mlr-org.github.io/celecx/reference/celecx.surrogate_performance.md).

Creates a new instance of this learner.

## Parameters

- `asymptote_init`, `amplitude_init`, `rate_init` :: `numeric(1)`  
  Initial values for `c`, `a`, and `k`. Defaults derived from the
  training data when unset.

- `rate_lower` :: `numeric(1)`  
  Lower bound for the decay rate `k`. Initialized to `1e-6` so the curve
  stays monotone and the model identifiable.

- `maxit` :: `integer(1)`  
  Maximum optim iterations. Initialized to `500`.
