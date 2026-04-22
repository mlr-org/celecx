# GPfit Regression Learner

Gaussian process regression. Calls
[`GPfit::GP_fit()`](https://rdrr.io/pkg/GPfit/man/GP_fit.html) from
package [GPfit](https://CRAN.R-project.org/package=GPfit).

Predictions return the posterior mean and the square root of the
predictive mean squared error as standard error.

- `scale` is initialized to `TRUE` and controls whether the learner
  min-max scales non-constant input features to the unit hypercube
  before calling
  [`GPfit::GP_fit()`](https://rdrr.io/pkg/GPfit/man/GP_fit.html). When
  scaling is enabled, constant features are dropped. The fitted `GP`
  object stores the full per-feature scaling map in `feature_offset` and
  `feature_scaling`. Constant features are recorded there with offset
  `0` and scaling `0`.

- `control_search`, `control_best`, and `control_cluster` map to the
  three components of the `control` vector expected by
  [`GPfit::GP_fit()`](https://rdrr.io/pkg/GPfit/man/GP_fit.html). Unset
  components fall back to the package defaults `200 * d`, `80 * d`, and
  `2 * d`, where `d` is the number of input columns.

- The correlation function is configured via the hyperparameters
  `corr_type`, `corr_power` (for `"exponential"`), and `corr_nu` (for
  `"matern"`), which are assembled into the `corr` list expected by
  [`GPfit::GP_fit()`](https://rdrr.io/pkg/GPfit/man/GP_fit.html).

Creates a new instance of this learner.
