# hetGP Regression Learner

Heteroscedastic Gaussian process regression. Calls
[`hetGP::mleHetGP()`](https://rdrr.io/pkg/hetGP/man/mleHetGP.html) from
package [hetGP](https://CRAN.R-project.org/package=hetGP).

Predictions return the posterior mean and the square root of the full
predictive variance (`sd2 + nugs`) as standard error.

`noiseControl` and `settings` are represented by explicit learner
hyperparameters tagged `"noise_control"` and `"settings"`, respectively.
The wrapper reconstructs the nested lists expected by
[`hetGP::mleHetGP()`](https://rdrr.io/pkg/hetGP/man/mleHetGP.html)
internally.

Creates a new instance of this learner.

## Initial Parameter Values

- `return_matrices`: Set to `FALSE` (upstream default is `TRUE`) to
  avoid storing inverse covariance matrices in the fitted model.
  Prediction recomputes them on demand when needed.
