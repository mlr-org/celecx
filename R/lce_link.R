#' @title Predictive Link Functions for LCE Forecasts
#'
#' @name lce_link
#'
#' @include aaa.R
#'
#' @description
#' A [LearnerLCE] models the surrogate-performance curve `f(b)` as Gaussian on a
#' *link scale*: `g(f(b)) ~ Normal(mu, sigma^2)`. The link `g` maps the support
#' of the target metric to the whole real line, so that a location-scale normal
#' on the link scale induces a sensibly-shaped, support-respecting predictive
#' distribution on the natural scale (e.g. log-normal for non-negative losses,
#' logit-normal for `[0, 1]` scores).
#'
#' This is *not* a general distribution object: the predictive is always
#' "normal on a link scale", carried by the plain numeric `response` / `se`
#' columns of a [PredictionLCE]. Only the link `g` varies. All links are monotone
#' *increasing* maps from their support to the reals, so quantiles and tail
#' probabilities map through `g` / `g^{-1}` without sign bookkeeping.
#'
#' A link is a plain list with elements `name`, `transform` (`g`), `inverse`
#' (`g^{-1}`), and `support` (the natural-scale interval `g` maps to the reals).
#' Retrieve one by name with `lce_link()`. The built-in links are:
#'
#' * `"identity"`: support `(-Inf, Inf)`. For unbounded metrics.
#' * `"log"`: support `(0, Inf)`. For non-negative losses (MAE, RMSE, MSE);
#'   induces a log-normal predictive.
#' * `"logit"`: support `(0, 1)`. For bounded scores (accuracy, AUC); induces a
#'   logit-normal predictive.
#'
#' @param name (`character(1)`)\cr
#'   Name of a registered link.
#'
#' @return A link (named `list`).
#'
#' @export
lce_link <- function(name) {
  assert_choice(name, names(lce_links))
  lce_links[[name]]
}

# Registry of link specifications. Each link is a monotone-increasing map from
# `support` onto the reals. `transform` is g, `inverse` is g^{-1}.
lce_links <- list(
  identity = list(
    name = "identity",
    transform = function(x) x,
    inverse = function(z) z,
    support = c(-Inf, Inf)
  ),
  log = list(
    name = "log",
    transform = function(x) log(x),
    inverse = function(z) exp(z),
    support = c(0, Inf)
  ),
  logit = list(
    name = "logit",
    transform = function(x) stats::qlogis(x),
    inverse = function(z) stats::plogis(z),
    support = c(0, 1)
  )
)

# Assert that natural-scale metric values lie strictly inside a link's support,
# so that g(value) is finite. Errors with a clear message otherwise (e.g. a
# performance of 0 under the log link, or 0/1 under the logit link). Non-finite
# inputs are left to the learners' own checks. `identity` never triggers.
lce_assert_support <- function(values, link, what) {
  support <- link$support
  bad <- is.finite(values) & (values <= support[[1L]] | values >= support[[2L]])
  if (any(bad)) {
    stopf("%s: performance value(s) outside the open support (%g, %g) of link '%s'; g(value) is not finite",
      what, support[[1L]], support[[2L]], link$name)
  }
  invisible(values)
}

#' @rdname lce_link
#'
#' @description
#' `lce_link_from_range()` picks a sensible link name from a metric's theoretical
#' `range`: `"log"` for `(0, Inf)`, `"logit"` for `(0, 1)`, and `"identity"`
#' otherwise. This expresses the idea that the link belongs to the target metric,
#' but the choice is never applied silently: a [TaskLCE] uses whatever `link` it
#' was constructed with (`"identity"` by default).
#'
#' @param range (`numeric(2)`)\cr
#'   Theoretical lower and upper bound of the metric.
#'
#' @return `lce_link_from_range()` returns a `character(1)` link name.
#'
#' @export
lce_link_from_range <- function(range) {
  assert_numeric(range, len = 2L, any.missing = FALSE)
  lower <- range[[1L]]
  upper <- range[[2L]]
  if (isTRUE(all.equal(lower, 0)) && is.infinite(upper) && upper > 0) {
    return("log")
  }
  if (isTRUE(all.equal(lower, 0)) && isTRUE(all.equal(upper, 1))) {
    return("logit")
  }
  "identity"
}
