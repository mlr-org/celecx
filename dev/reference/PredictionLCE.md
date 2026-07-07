# LCE Prediction Object

Prediction object for
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md).
Carries the per-row true and predicted surrogate performance and,
depending on the producing learner's predict type, a distributional
payload describing the predictive of the performance curve `f(b)`:

- `response` ([`numeric()`](https://rdrr.io/r/base/numeric.html)):
  predictive median on the natural scale. For the sample-based learners
  this is the mean of the draws on the
  [lce_link](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  scale, back-transformed – which is the median whenever the link-scale
  distribution is symmetric (e.g. Gaussian). Always present.

- `se` ([`numeric()`](https://rdrr.io/r/base/numeric.html)): *total*
  predictive standard deviation on the link scale (epistemic +
  aleatoric), the quantity to score against the realised `y_b`.

- `se_epistemic` ([`numeric()`](https://rdrr.io/r/base/numeric.html)):
  standard deviation of the *mean* `f(b)` on the link scale (epistemic
  only), the quantity for expected-crossing decisions.

- `quantiles` ([`matrix()`](https://rdrr.io/r/base/matrix.html)):
  predictive quantiles, one row per observation, one column per
  probability (carried in the `"probs"` attribute).

- `samples` ([`matrix()`](https://rdrr.io/r/base/matrix.html)):
  predictive draws, one row per observation, one column per draw.
  Columns are joint sample paths for the sample-based learners.

- `target_reached` ([`matrix()`](https://rdrr.io/r/base/matrix.html)):
  probability that the metric has reached a target, one row per
  observation, one column per target (carried in the `"target"`
  attribute).

The link scale on which `se` / `se_epistemic` live is a property of the
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
(its `link`); the prediction itself stays a few plain numeric / matrix
columns.

Creates a new PredictionLCE.

## Arguments

- task:

  ([TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md))  
  Task used to derive row ids and truth.

- row_ids:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids of the predictions.

- truth:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  True surrogate performances.

- response:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Predicted surrogate performances (natural-scale predictive median).

- se:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Total predictive standard error on the link scale.

- se_epistemic:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Epistemic standard error of the mean on the link scale.

- quantiles:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Predicted quantiles (rows = observations, columns = probabilities).
  The probabilities must be stored in the `"probs"` attribute.

- samples:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Predictive draws (rows = observations, columns = draws).

- target_reached:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Reach probabilities (rows = observations, columns = targets). The
  targets must be stored in the `"target"` attribute.

- weights:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Optional measure weights.

- check:

  (`logical(1)`)  
  Whether to validate the inputs.

## Fields

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Predicted surrogate performance for each row.

- `se`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Total predictive standard error (link scale), or `NA` vector when
  absent.

- `se_epistemic`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Epistemic standard error of the mean (link scale), or `NA` when
  absent.

- `quantiles`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Matrix of predicted quantiles (rows = observations, columns
  ascending).

- `samples`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Matrix of predictive draws (rows = observations, columns = draws).

- `target_reached`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Matrix of reach probabilities (rows = observations, columns =
  targets).
