#' @include LearnerLCE.R
NULL

# Aggregate the (batch_nr, target) pairs of a TaskLCE to one row per batch by
# mean. Returns a list with a sorted batch vector and the corresponding
# per-batch values. When `link` is given, the per-batch values are asserted to
# lie inside its support, since the learners fit the curve on the link scale.
lce_train_per_batch <- function(task, link = NULL) {
  value <- NULL  # data.table NSE
  bb <- as.numeric(task$data(cols = task$col_roles$feature)[[1L]])
  yy <- as.numeric(task$truth())
  dt <- data.table(batch = bb, value = yy)
  dt <- dt[, list(value = mean(value)), by = "batch"]
  setorderv(dt, "batch")
  if (!is.null(link)) lce_assert_support(dt$value, link, sprintf("task '%s'", task$id))
  list(batch = dt$batch, value = dt$value, n_batches = nrow(dt))
}

# Evaluate `expr` with the RNG seeded to `seed`, restoring the previous RNG
# state afterwards -- including removing .Random.seed again when the RNG had
# not been touched before (which the get_seed()-based idiom cannot do, since
# get_seed() initializes the RNG). A NULL seed evaluates `expr` unchanged.
with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(if (had_seed) {
    assign(".Random.seed", old_seed, envir = globalenv())
  } else {
    rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  expr
}

# Extract per-row batch numbers from a (test) TaskLCE.
lce_predict_batches <- function(task) {
  as.numeric(task$data(cols = task$col_roles$feature)[[1L]])
}

# Fit a parametric LCE model by box-constrained L-BFGS-B on the squared-loss
# objective `fn`. Returns the coefficients, the optimizer convergence code, the
# residual variance sigma2 = SSE / (n_batches - n_pars), and the Gauss-Newton
# parameter covariance Sigma = sigma2 * (H / 2)^{-1}, where H is the optim
# Hessian (always computed so `se` works regardless of the train-time predict
# type). Sigma is NULL -- and SE predictions degrade to NA -- when the residual
# degrees of freedom are non-positive or H is not usable.
lce_fit_parametric <- function(par_init, lower, upper, fn, maxit, n_batches) {
  fit <- stats::optim(
    par = par_init,
    fn = fn,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(maxit = maxit),
    hessian = TRUE
  )

  df <- n_batches - length(par_init)
  sigma2 <- if (df > 0L) fit$value / df else NA_real_
  Sigma <- if (df > 0L) {
    tryCatch(sigma2 * 2 * solve(fit$hessian), error = function(e) NULL)
  } else {
    NULL
  }
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

  list(
    coefficients = fit$par,
    convergence = fit$convergence,
    sigma2 = sigma2,
    Sigma = Sigma
  )
}

# Ordinary least squares of `y` on `cbind(1, x)`, as used by the linear LCE
# learners (parametric_log fits on log(batch), rolling_slope on the batch
# window). Returns the named coefficients, the residual variance sigma2, and
# the linear-model parameter covariance Sigma = sigma2 * (X'X)^{-1}. Sigma is
# NULL (not an all-NA matrix, which lce_delta_method_se would not recognize)
# when the residual variance is undefined or X'X is singular.
lce_fit_ols <- function(x, y) {
  X <- cbind(1, x)
  fit <- stats::lm.fit(X, y)
  df <- length(y) - 2L
  sigma2 <- if (df > 0L) sum(fit$residuals^2) / df else NA_real_
  Sigma <- if (df > 0L) {
    tryCatch(sigma2 * solve(crossprod(X)), error = function(e) NULL)
  } else {
    NULL
  }
  list(
    coefficients = c(intercept = unname(fit$coefficients[[1L]]),
      slope = unname(fit$coefficients[[2L]])),
    sigma2 = sigma2,
    Sigma = Sigma
  )
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

# Optimization direction encoded in a codomain's single target tag: TRUE for
# "minimize", FALSE for "maximize", NA when it cannot be determined (no
# codomain, several targets, or a "learn"-tagged target).
lce_codomain_minimize <- function(codomain) {
  if (is.null(codomain) || length(codomain$target_ids) != 1L) return(NA)
  tags <- codomain$tags[[codomain$target_ids]]
  if ("minimize" %in% tags) TRUE else if ("maximize" %in% tags) FALSE else NA
}

# Optimization direction for a TaskLCE as a scalar logical, or NA when it
# cannot be determined. The task measure takes precedence; without one (e.g.
# best-so-far tasks from task_lce_best_so_far()), the direction of the
# codomain's single target is used. Stored in a learner's model so it does not
# depend on the predict-time task.
lce_model_minimize <- function(task) {
  measure <- task$measure
  if (!is.null(measure)) return(measure$minimize)
  lce_codomain_minimize(task$codomain)
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
