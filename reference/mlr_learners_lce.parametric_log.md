# Parametric Log LCE Learner

Fits a two-parameter logarithmic learning curve \$\$f(b) = c + a \log
b\$\$ to the per-batch surrogate performance. Since the model is linear
in \\(c, a)\\ the fit reduces to ordinary least squares on \\(\log b,
y)\\ and standard errors come from the usual linear-regression
covariance \\\hat\sigma^2 (X^\top X)^{-1}\\ rather than from the
Gauss-Newton delta method used by the nonlinear parametric LCE learners.

Training batches with `batch_nr <= 0` are rejected because \\\log b\\ is
undefined.

Creates a new instance of this learner.
