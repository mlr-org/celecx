#' @include LearnerLCE.R
NULL

# Aggregate the (batch_nr, target) pairs of a TaskLCE to one row per batch by
# mean. Returns a list with a sorted batch vector and the corresponding
# per-batch values. When `link` is given, the per-batch values are asserted to
# lie inside its support, since the learners fit the curve on the link scale.
lce_train_per_batch <- function(task, link = NULL) {
  bb <- as.numeric(task$data(cols = task$col_roles$feature)[[1L]])
  yy <- as.numeric(task$truth())
  dt <- data.table(batch = bb, value = yy)
  dt <- dt[, list(value = mean(value)), by = "batch"]
  setorderv(dt, "batch")
  if (!is.null(link)) lce_assert_support(dt$value, link, sprintf("task '%s'", task$id))
  list(batch = dt$batch, value = dt$value, n_batches = nrow(dt))
}

# Extract per-row batch numbers from a (test) TaskLCE.
lce_predict_batches <- function(task) {
  as.numeric(task$data(cols = task$col_roles$feature)[[1L]])
}

# Fit a parametric LCE model by box-constrained L-BFGS-B on the squared-loss
# objective. Returns coefficients (named numeric vector), the residual
# variance sigma2, the parameter covariance Sigma, the number of batches,
# and the optimizer convergence code. Sigma uses the Gauss-Newton
# approximation Sigma = sigma2 * (H / 2)^{-1}, where H is the optim Hessian.
# If H is not invertible Sigma is NA and SE predictions degrade to NA.
lce_fit_parametric <- function(par_init, lower, upper, fn, gr = NULL,
    maxit = 500L, hessian = TRUE) {
  fit <- stats::optim(
    par = par_init,
    fn = fn,
    gr = gr,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(maxit = maxit),
    hessian = hessian
  )

  coefficients <- fit$par
  result <- list(
    coefficients = coefficients,
    sse = fit$value,
    convergence = fit$convergence,
    hessian = if (hessian) fit$hessian else NULL,
    sigma2 = NA_real_,
    Sigma = NULL
  )
  result
}

# Finalize the parameter covariance for a parametric LCE fit. Given the
# residual sum of squares, the number of batches, and the optim Hessian,
# returns the Gauss-Newton-style covariance matrix or NULL if not invertible.
lce_param_cov <- function(hessian, sse, n_batches, n_pars) {
  df <- n_batches - n_pars
  if (df <= 0L || is.null(hessian)) {
    return(list(sigma2 = NA_real_, Sigma = NULL))
  }
  sigma2 <- sse / df
  Sigma <- tryCatch(sigma2 * 2 * solve(hessian), error = function(e) NULL)
  # A merely invertible Hessian is not enough: an indefinite one (a rate pinned
  # at its box constraint, or a weakly-identified near-flat curve) yields a
  # covariance with negative variances. Reject it so the SE degrades to NA
  # rather than to a falsely-confident clamped-to-zero value.
  if (!is.null(Sigma)) {
    ev <- tryCatch(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA_real_)
    tol <- sqrt(.Machine$double.eps) * max(abs(ev), 1)
    if (anyNA(ev) || any(ev < -tol)) Sigma <- NULL
  }
  list(sigma2 = sigma2, Sigma = Sigma)
}

# Delta-method SE for parametric LCE predictions. `grad_rows` is a matrix of
# row-wise gradients of the prediction function with respect to the
# parameters (one row per prediction). Sigma is the parameter covariance.
# Returns a numeric vector of standard errors, or NA if Sigma is unavailable.
lce_delta_method_se <- function(grad_rows, Sigma) {
  if (is.null(Sigma)) {
    return(rep(NA_real_, nrow(grad_rows)))
  }
  variances <- rowSums((grad_rows %*% Sigma) * grad_rows)
  variances <- pmax(variances, 0)
  sqrt(variances)
}

# Epistemic and total predictive SD (on the link scale) for a parametric LCE fit
# at the given prediction gradient rows. `se_epi` is the delta-method SD of the
# mean curve f(b); `se_total` adds the aleatoric residual variance `sigma2`
# (the SD against which a realised y_b is scored). Both are NA where the
# parameter covariance / residual variance is unavailable.
lce_se_components <- function(grad_rows, Sigma, sigma2) {
  se_epi <- lce_delta_method_se(grad_rows, Sigma)
  list(se_epi = se_epi, se_total = sqrt(se_epi^2 + sigma2))
}

# Optimization direction for a TaskLCE as a scalar logical, or NA when the task
# carries no measure (or the measure has no direction). Stored in a learner's
# model so it does not depend on the predict-time task.
lce_model_minimize <- function(task) {
  measure <- task$measure
  if (is.null(measure)) return(NA)
  measure$minimize
}

# Resolve a monotone LCE learner's direction. "auto" reads the direction from
# the task measure: loss-style measures decrease, utility-style measures increase.
lce_resolve_monotone_direction <- function(direction, task, learner_id) {
  if (direction != "auto") {
    return(direction)
  }
  minimize <- lce_task_minimize(task,
    sprintf("'%s' with direction = 'auto'", learner_id))
  if (minimize) "decreasing" else "increasing"
}
