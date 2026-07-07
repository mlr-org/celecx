# Distributional LCE Measures

Per-batch distributional measures for
[TaskLCE](https://mlr-org.github.io/celecx/reference/TaskLCE.md) /
[LearnerLCE](https://mlr-org.github.io/celecx/reference/LearnerLCE.md)
evaluations. Each measure aggregates the prediction and truth to one
value per batch and then scores the per-batch predictive distribution
against the realised performance. The predictive is interpreted as
Gaussian on the task's
[lce_link](https://mlr-org.github.io/celecx/reference/lce_link.md)
scale, carried by the `se` predict type, except for `lce.pinball`, which
reads the `quantiles` predict type.

- `lce.crps`: closed-form continuous ranked probability score of the
  Normal-on-link predictive against the link-transformed truth. The
  headline proper score for the forecast distribution.

- `lce.reach_brier`: Brier score of the predicted probability that the
  metric has reached `target` against whether the realised per-batch
  performance has. The direction (reach from above / below) is read from
  the task's measure. When the prediction carries a `target_reached`
  column for the requested `target`, that probability is scored directly
  (exact, draw-based for the sample-based learners); otherwise the
  probability is the Gaussian-on-link form computed from `response` /
  `se`. This is the proper, grid-free way to benchmark
  "batches-to-target".

- `lce.coverage`: empirical coverage of the central `level` predictive
  interval (ideally equal to `level`; reported, not optimised).

- `lce.interval_score`: Winkler interval score of the central `level`
  predictive interval (sharpness plus miscoverage penalty).

- `lce.pinball`: average pinball (quantile) loss at quantile `alpha`,
  which must be one of the probabilities the learner predicted. Unlike
  the other distributional measures it is scored on the natural scale,
  since it consumes the natural-scale `quantiles` predict type directly.

All measures support observation weights from a `weights_measure` task
column; the weight of a batch is the sum of weights of its archive rows.
