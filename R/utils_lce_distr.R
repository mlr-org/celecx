#' @include lce_link.R
NULL

# ---------------------------------------------------------------------------
# Low-level closed-form quantities for a Normal-on-link predictive.
# All `mu` / `sigma` arguments live on the link scale; `g_*` arguments are
# already link-transformed; natural-scale results are produced via the link's
# inverse. These are shared by the Gaussian LCE learners and the distributional
# measures so the two never drift apart.
# ---------------------------------------------------------------------------

# P(metric has reached the target `tau`) for a Normal(mu, sigma) on the link
# scale. `g_tau` is the link-transformed target. `minimize` selects the
# direction: when TRUE the target is reached once the metric drops to/below tau
# (loss-style), otherwise once it rises to/above tau (score-style). Handles the
# degenerate sigma == 0 case as a hard step. Vectorised over all arguments.
lce_reach_prob <- function(g_tau, mu, sigma, minimize) {
  if (!length(mu) || !length(sigma)) {
    return(numeric(0))
  }
  rec <- recycle_vectors(list(g_tau = g_tau, mu = mu, sigma = sigma))
  g_tau <- rec$g_tau
  mu <- rec$mu
  sigma <- rec$sigma
  z <- (g_tau - mu) / sigma
  prob <- if (minimize) stats::pnorm(z) else stats::pnorm(-z)
  degenerate <- !is.na(sigma) & sigma == 0
  if (any(degenerate)) {
    hard <- if (minimize) as.numeric(mu[degenerate] <= g_tau[degenerate]) else
      as.numeric(mu[degenerate] >= g_tau[degenerate])
    prob[degenerate] <- hard
  }
  prob
}

# CRPS of a Normal(mu, sigma) predictive evaluated at observation `y`, all on
# the link scale (so this is the CRPS of the link-transformed target). Closed
# form; reduces to the absolute error when sigma == 0. Vectorised.
lce_gaussian_crps <- function(y, mu, sigma) {
  out <- abs(y - mu)
  out[is.na(sigma)] <- NA_real_
  pos <- !is.na(sigma) & sigma > 0
  if (any(pos)) {
    w <- (y[pos] - mu[pos]) / sigma[pos]
    out[pos] <- sigma[pos] * (w * (2 * stats::pnorm(w) - 1) +
      2 * stats::dnorm(w) - 1 / sqrt(pi))
  }
  out
}

# ---------------------------------------------------------------------------
# Prediction assemblers. Each turns a per-row predictive representation into the
# list returned by a LearnerLCE's `$.predict()` for the requested `predict_type`.
# ---------------------------------------------------------------------------

# Assemble a prediction list from a Normal-on-link representation. `mu` is the
# link-scale mean, `se_total` the total predictive SD (epistemic + aleatoric)
# and `se_epi` the epistemic SD of the mean, both on the link scale. `link` is a
# link spec (see [lce_link]). `reach_target` / `minimize` are required for the
# `"target_reached"` type. Returns a named list with `response` plus the columns
# for `predict_type`.
lce_distr_predict <- function(predict_type, mu, se_total, se_epi, link,
    reach_target = NULL, minimize = NULL) {
  response <- link$inverse(mu)
  out <- list(response = response)
  if (predict_type == "se") {
    out$se <- se_total
    out$se_epistemic <- se_epi
  } else if (predict_type == "target_reached") {
    out$target_reached <- lce_target_reached_matrix(reach_target, minimize,
      function(g_tau) lce_reach_prob(g_tau, mu, se_total, minimize),
      link = link)
  }
  out
}

# Per-row SD across the columns of an already-link-transformed matrix; 0 for a
# single column (one draw carries no measured spread).
lce_rowsd <- function(g_mat) {
  if (ncol(g_mat) > 1L) apply(g_mat, 1L, stats::sd) else rep(0, nrow(g_mat))
}

# Assemble a prediction list for a sample-based learner. `samples` is a rows x K
# matrix of predictive draws of the *realised* trajectory on the natural scale;
# `response`, `se_total`, `se_epi` are computed by the learner (`response` the
# natural-scale point forecast, `se_total` / `se_epi` the link-scale total
# predictive and epistemic SDs). The richer predict types are read off `samples`
# directly. Reach is a natural-scale event compared on the natural scale (the
# monotone link does not change which draws have reached the target).
lce_samples_predict <- function(predict_type, samples, response, se_total, se_epi,
    probs = NULL, reach_target = NULL, minimize = NULL) {
  out <- list(response = response)
  if (predict_type == "se") {
    out$se <- se_total
    out$se_epistemic <- se_epi
  } else if (predict_type == "quantiles") {
    qs <- t(apply(samples, 1L, stats::quantile, probs = probs, names = FALSE,
      type = 7L))
    qs <- matrix(qs, nrow = nrow(samples), ncol = length(probs))
    setattr(qs, "probs", probs)
    out$quantiles <- qs
  } else if (predict_type == "samples") {
    out$samples <- samples
  } else if (predict_type == "target_reached") {
    out$target_reached <- lce_target_reached_matrix(reach_target, minimize,
      function(tau) rowMeans(if (minimize) samples <= tau else samples >= tau))
  }
  out
}

# Build the `target_reached` matrix (rows x length(reach_target)) with a
# `"target"` attribute. `prob_fun(g_tau)` returns the per-row reach probability
# for a single link-transformed target. `link` link-transforms the targets when
# given (the Gaussian path); the samples path passes a function that already
# operates on the link scale, so it transforms internally and `link` is unused.
lce_target_reached_matrix <- function(reach_target, minimize, prob_fun,
    link = NULL) {
  if (is.null(reach_target)) {
    stop("predict_type 'target_reached' requires the 'reach_target' parameter to be set")
  }
  if (is.null(minimize) || is.na(minimize)) {
    stop("predict_type 'target_reached' requires the optimization direction; the task must carry a measure")
  }
  g_targets <- if (is.null(link)) reach_target else link$transform(reach_target)
  cols <- lapply(g_targets, prob_fun)
  mat <- matrix(unlist(cols, use.names = FALSE), ncol = length(reach_target))
  setattr(mat, "target", reach_target)
  mat
}
