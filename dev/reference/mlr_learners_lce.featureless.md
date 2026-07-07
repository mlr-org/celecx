# Featureless LCE Learner

Baseline
[LearnerLCE](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md)
which ignores the `batch_nr` value and predicts a constant performance
for every future batch. The constant is one of three summaries of the
per-batch training performances, selected via `type`:

- `"average"`: the mean (or median, when `robust = TRUE`) of the
  considered training batches. The "no learning-curve information used
  at all" sanity-check baseline.

- `"best"`: the best performance observed among the considered training
  batches. The "no progress beyond the best we have already seen"
  baseline. The optimization direction (whether higher or lower is
  better) is taken from the task's `measure`, so the task must carry one
  for this `type`.

- `"last"`: the performance of the most recent batch. The "no progress
  from here" baseline.

The `window` parameter restricts "the considered batches" to the most
recent `window` of them (all of them by default). Location and
dispersion are computed on the task's
[lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
scale. When `predict_type = "se"`, `se_epistemic` is the (robust)
link-scale dispersion of the considered per-batch performances divided
by the square root of their count (the standard error of the constant),
and the total predictive `se` adds that dispersion back as the aleatoric
spread (`se = sqrt(dispersion^2 + se_epistemic^2)`), both replicated for
every prediction. For `type = "average"` this is the textbook predictive
standard deviation of a new observation; for `type = "best"` / `"last"`
the constant is not the window mean, so the symmetric interval it
implies is only a coarse heuristic. The uncertainty reflects nothing
beyond the spread of the observed curve.

Creates a new instance of this learner.

## Parameters

- `type` :: `character(1)`  
  `"average"`, `"best"`, or `"last"`. Initialized to `"best"`.

- `robust` :: `logical(1)`  
  When `TRUE`, use median + MAD instead of mean + SD. Affects the
  `"average"` location and the dispersion of all types. Initialized to
  `FALSE`.

- `window` :: `integer(1)`  
  Number of most recent batches considered, clamped to the number of
  training batches. `Inf` (the default) uses all batches. For
  `type = "last"` only the `se` is affected, since the location is
  always the most recent batch.
