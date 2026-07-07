# Predictive Link Functions for LCE Forecasts

A
[LearnerLCE](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md)
models the surrogate-performance curve `f(b)` as Gaussian on a *link
scale*: `g(f(b)) ~ Normal(mu, sigma^2)`. The link `g` maps the support
of the target metric to the whole real line, so that a location-scale
normal on the link scale induces a sensibly-shaped, support-respecting
predictive distribution on the natural scale (e.g. log-normal for
non-negative losses, logit-normal for `[0, 1]` scores).

This is *not* a general distribution object: the predictive is always
"normal on a link scale", carried by the plain numeric `response` / `se`
columns of a
[PredictionLCE](https://mlr-org.github.io/celecx/dev/reference/PredictionLCE.md).
Only the link `g` varies. All links are monotone *increasing* maps from
their support to the reals, so quantiles and tail probabilities map
through `g` / `g^{-1}` without sign bookkeeping.

A link is a plain list with elements `name`, `transform` (`g`),
`inverse` (`g^{-1}`), and `support` (the natural-scale interval `g` maps
to the reals). Retrieve one by name with `lce_link()`. The built-in
links are:

- `"identity"`: support `(-Inf, Inf)`. For unbounded metrics.

- `"log"`: support `(0, Inf)`. For non-negative losses (MAE, RMSE, MSE);
  induces a log-normal predictive.

- `"logit"`: support `(0, 1)`. For bounded scores (accuracy, AUC);
  induces a logit-normal predictive.

`lce_link_from_range()` picks a sensible link name from a metric's
theoretical `range`: `"log"` for `(0, Inf)`, `"logit"` for `(0, 1)`, and
`"identity"` otherwise. This expresses the idea that the link belongs to
the target metric, but the choice is never applied silently: a
[TaskLCE](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
uses whatever `link` it was constructed with (`"identity"` by default).

## Usage

``` r
lce_link(name)

lce_link_from_range(range)
```

## Arguments

- name:

  (`character(1)`)  
  Name of a registered link.

- range:

  (`numeric(2)`)  
  Theoretical lower and upper bound of the metric.

## Value

A link (named `list`).

`lce_link_from_range()` returns a `character(1)` link name.
