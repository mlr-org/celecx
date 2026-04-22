# Deep GP Regression Learner

Two-layer deep Gaussian process regression. Calls
[`deepgp::fit_two_layer()`](https://rdrr.io/pkg/deepgp/man/fit_two_layer.html)
from package [deepgp](https://CRAN.R-project.org/package=deepgp).

Predictions return the posterior predictive mean and the square root of
the predictive variance as standard error. The learner always predicts
with `lite = TRUE` and `return_all = TRUE`, then recomputes the
predictive variance from the per-iteration outputs via the law of total
variance. This is more robust than the package's built-in variance
summary for the exposed `response`/`se` outputs.

- If `D` is left unset,
  [`deepgp::fit_two_layer()`](https://rdrr.io/pkg/deepgp/man/fit_two_layer.html)
  defaults it to the number of input columns.

- The upstream `settings` list is represented by explicit learner
  hyperparameters tagged `"settings"`, and reconstructed internally
  before calling
  [`deepgp::fit_two_layer()`](https://rdrr.io/pkg/deepgp/man/fit_two_layer.html).

- `train_cores` maps to the `cores` argument of
  [`deepgp::fit_two_layer()`](https://rdrr.io/pkg/deepgp/man/fit_two_layer.html)
  when `vecchia = TRUE`; `predict_cores` maps to the `cores` argument of
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html).

- Only `mean_map` is exposed at predict time. Other upstream predict
  options that only generate discarded auxiliary outputs are fixed
  internally.

Creates a new instance of this learner.

## Initial Parameter Values

- `verb`: Set to `FALSE` (upstream default is `TRUE`) to suppress
  progress output.

- `train_cores`, `predict_cores`: Set to `1` to disable threading by
  default. They carry the `"threads"` tag so
  [`mlr3::set_threads()`](https://mlr3.mlr-org.com/reference/set_threads.html)
  can update them uniformly.
