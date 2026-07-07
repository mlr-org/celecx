# Residual Bootstrap LCE Learner Wrapper

Wraps an arbitrary base
[LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md)
and equips it with an SE prediction via residual bootstrap on the
per-batch fit.

Training proceeds as:

1.  Fit the base learner once on the training task; collect the
    per-batch fitted values and the residuals on the
    [lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
    scale (`residual = g(truth) - g(fitted)`).

2.  For each of `n_bootstrap` replicates, resample the link-scale
    residuals with replacement, add them to the fitted values,
    back-transform to the natural scale (so the synthetic targets
    respect the link's support), and refit the base learner on a clone
    of the training task.

Each replicate is a full curve, so the replicates are draws of the
*mean* curve `f(b)`. The point forecast `response` is their link-scale
mean, back-transformed (the predictive median under the link). The
epistemic `se_epistemic` is the link-scale spread of the replicate mean
curves; the total predictive `se` adds the aleatoric residual variance
back. The `samples`, `quantiles`, and `target_reached` predict types
describe the *realised* future performance `y_b`: each draw is a
replicate mean curve plus a resampled link-scale residual, so they
reflect both the bootstrap mean-curve uncertainty and the residual
noise.

Residual bootstrap is the natural choice here because every batch
contributes a single per-batch performance value: row-bootstrap of the
archive would simply reweight identical rows, while batch-bootstrap with
multiplicity would not be picked up by base learners that aggregate by
batch before fitting.

Creates a new bootstrap-wrapped LCE learner.

## Arguments

- learner:

  ([LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md))  
  Base LCE learner to wrap. Its predict type is forced to `"response"`
  while the bootstrap replicates are fitted.

## Parameters

The bootstrap wrapper's own parameters are exposed alongside the base
learner's parameters via
[paradox::ParamSetCollection](https://paradox.mlr-org.com/reference/ParamSetCollection.html);
the base learner's parameters carry the `base.` prefix (e.g.
`base.rate_lower`).

Own parameters:

- `n_bootstrap` :: `integer(1)`  
  Number of bootstrap replicates. Initialized to `100`.

- `seed` :: `integer(1)` \| `NULL`  
  RNG seed for the residual resampling – both the bootstrap refits at
  train time and the realised-trajectory draws at predict time, so a
  fixed seed makes the whole prediction reproducible. If `NULL`
  (default), the current RNG state is used.
