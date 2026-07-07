# tgp Regression Learner

Bayesian Gaussian process regression. Calls
[`tgp::bgp()`](https://rdrr.io/pkg/tgp/man/btgp.html) from package
[tgp](https://CRAN.R-project.org/package=tgp).

Predictions return the posterior predictive mean and the square root of
the predictive kriging variance as standard error. Several parameters
that appear in both
[`tgp::bgp()`](https://rdrr.io/pkg/tgp/man/btgp.html) and `predict.tgp`
are prefixed with `predict_` to distinguish the predict-time variants
(e.g. `predict_BTE`, `predict_R`).

The learner always trains without collecting training-location
predictive summaries and always predicts with pointwise kriging
variances only. The corresponding upstream options are therefore not
exposed because their effects are discarded by the mlr3 wrapper.

Creates a new instance of this learner.

## Initial Parameter Values

- `verb`: Set to `0` (upstream default is `1`) to suppress progress
  output.
