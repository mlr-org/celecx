# Batches-to-Target Forecast

Turns a trained
[LearnerLCE](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md)
into a forecast of *how many further batches* are needed to reach a
target performance `target`, by reading off the learner's predictive
over a future `batch_nr` grid. This is a reported transform, not a
predict type or measure: it summarises the per-batch predictive into a
crossing-batch distribution.

Two crossing semantics are supported (see the celecx research notes):

- `crossing = "expected"` (default): the de-noised expected curve `f(b)`
  crosses `target`. For the principled, monotone forecasters this law is
  fixed by the per-batch marginals alone, so it is computed in closed
  form from the `se_epistemic` (epistemic) standard error: the crossing
  CDF at grid batch `b` is the epistemic probability that `f(b)` has
  passed `target`.

- `crossing = "observed"`: the *noisy realised* `y_b` crosses `target` –
  the literal stop time of an observe-and-stop run. This is a
  first-passage of a correlated noisy sequence and needs the joint
  predictive, so it is estimated from the learner's `samples` predict
  type (each column is a joint sample path). The learner must support
  `"samples"` (the sample-based learners
  [LearnerLCEBootstrap](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.bootstrap.md)
  and
  [LearnerLCESimulate](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.simulate.md)
  do). It is systematically optimistic relative to the expected
  crossing.

The optimization direction (whether the target is reached from above or
below) and the predictive
[lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
scale are read from the learner's training task, which must therefore
carry a `measure` (or, for best-so-far tasks from
[`task_lce_best_so_far()`](https://mlr-org.github.io/celecx/dev/reference/task_lce_best_so_far.md),
a directed codomain).

## Usage

``` r
lce_batches_to_target(
  learner,
  batch_grid,
  target,
  crossing = "expected",
  probs = c(0.1, 0.5, 0.9),
  last_trained_batch = NULL
)
```

## Arguments

- learner:

  ([LearnerLCE](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md))  
  A trained LCE learner.

- batch_grid:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Strictly increasing future `batch_nr` values to evaluate the crossing
  over.

- target:

  (`numeric(1)`)  
  Target performance value.

- crossing:

  (`character(1)`)  
  `"expected"` or `"observed"`.

- probs:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Probabilities at which to report crossing-batch quantiles.

- last_trained_batch:

  (`numeric(1)` \| `NULL`)  
  The last `batch_nr` the learner was trained on – the reference point
  for `quantiles_remaining`. With the default `NULL` it is read from the
  learner's model (all celecx LCE learners store it); supply it
  explicitly for third-party learners that do not.

## Value

A `list` with:

- `quantiles` (named
  [`numeric()`](https://rdrr.io/r/base/numeric.html)): for each `probs`,
  the smallest grid batch at which the crossing CDF reaches that
  probability, or `NA` if the grid never does (the target is not reached
  by that quantile within the grid).

- `quantiles_remaining` (named
  [`numeric()`](https://rdrr.io/r/base/numeric.html)): the same crossing
  quantiles expressed as *remaining* batches beyond the learner's last
  trained batch – the project's "how many more batches" quantity.

- `last_trained_batch` (`numeric(1)`): the last `batch_nr` the learner
  was trained on, i.e. the reference point of `quantiles_remaining`.

- `p_never` (`numeric(1)`): probability the target is never reached
  within the grid (`1 -` the maximum crossing CDF).

- `grid`
  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)):
  columns `batch` and `cdf`, the crossing-batch CDF over the grid.
