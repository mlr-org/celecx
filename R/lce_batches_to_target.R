#' @title Batches-to-Target Forecast
#'
#' @name lce_batches_to_target
#' @include LearnerLCE.R
#'
#' @description
#' Turns a trained [LearnerLCE] into a forecast of *how many further batches* are
#' needed to reach a target performance `target`, by reading off the learner's
#' predictive over a future `batch_nr` grid. This is a reported transform, not a
#' predict type or measure: it summarises the per-batch predictive into a
#' crossing-batch distribution.
#'
#' Two crossing semantics are supported (see the celecx research notes):
#'
#' * `crossing = "expected"` (default): the de-noised expected curve `f(b)`
#'   crosses `target`. For the principled, monotone forecasters this law is fixed
#'   by the per-batch marginals alone, so it is computed in closed form from the
#'   `se_epistemic` (epistemic) standard error: the crossing CDF at grid batch
#'   `b` is the epistemic probability that `f(b)` has passed `target`.
#' * `crossing = "observed"`: the *noisy realised* `y_b` crosses `target` -- the
#'   literal stop time of an observe-and-stop run. This is a first-passage of a
#'   correlated noisy sequence and needs the joint predictive, so it is estimated
#'   from the learner's `samples` predict type (each column is a joint sample
#'   path). The learner must support `"samples"` (the sample-based learners
#'   [LearnerLCEBootstrap] and [LearnerLCESimulate] do). It is systematically
#'   optimistic relative to the expected crossing.
#'
#' The optimization direction (whether the target is reached from above or below)
#' and the predictive [lce_link] scale are read from the learner's training task,
#' which must therefore carry a `measure`.
#'
#' @param learner ([LearnerLCE])\cr
#'   A trained LCE learner.
#' @param batch_grid (`integer()`)\cr
#'   Strictly increasing future `batch_nr` values to evaluate the crossing over.
#' @param target (`numeric(1)`)\cr
#'   Target performance value.
#' @param crossing (`character(1)`)\cr
#'   `"expected"` or `"observed"`.
#' @param probs (`numeric()`)\cr
#'   Probabilities at which to report crossing-batch quantiles.
#'
#' @return A `list` with:
#' * `quantiles` (named `numeric()`): for each `probs`, the smallest grid batch at
#'   which the crossing CDF reaches that probability, or `NA` if the grid never
#'   does (the target is not reached by that quantile within the grid).
#' * `p_never` (`numeric(1)`): probability the target is never reached within the
#'   grid (`1 -` the maximum crossing CDF).
#' * `grid` ([data.table::data.table]): columns `batch` and `cdf`, the
#'   crossing-batch CDF over the grid.
#'
#' @export
lce_batches_to_target <- function(learner, batch_grid, target,
    crossing = "expected", probs = c(0.1, 0.5, 0.9)) {
  assert_r6(learner, "LearnerLCE")
  if (is.null(learner$model)) {
    stop("`learner` must be trained before forecasting batches to target")
  }
  batch_grid <- assert_integerish(batch_grid, lower = 1L, any.missing = FALSE,
    min.len = 1L, unique = TRUE, sorted = TRUE, coerce = TRUE)
  assert_number(target)
  assert_choice(crossing, c("expected", "observed"))
  assert_numeric(probs, lower = 0, upper = 1, any.missing = FALSE, min.len = 1L,
    sorted = TRUE)

  train_task <- learner$state$train_task
  if (is.null(train_task)) {
    stop("`learner` has no stored training task to read the link and direction from")
  }
  link <- lce_link(train_task$link)
  minimize <- lce_task_minimize(train_task, "lce_batches_to_target()")
  newdata <- setnames(data.table(as.integer(batch_grid)), train_task$batch_nr)

  old_predict_type <- learner$predict_type
  on.exit(learner$predict_type <- old_predict_type, add = TRUE)

  cdf <- if (crossing == "expected") {
    learner$predict_type <- "se"
    pred <- learner$predict_newdata(newdata)
    # P(f(b) has passed the target) from the epistemic uncertainty of the mean.
    reached <- lce_reach_prob(link$transform(target),
      link$transform(pred$response), pred$se_epistemic, minimize)
    cummax(reached)
  } else {
    if ("samples" %nin% learner$predict_types) {
      stopf("crossing = 'observed' needs a learner supporting the 'samples' predict type; '%s' does not",
        learner$id)
    }
    learner$predict_type <- "samples"
    pred <- learner$predict_newdata(newdata)
    samples <- pred$samples
    reached_mat <- if (minimize) samples <= target else samples >= target
    # First-passage per path, then cumulative fraction of paths crossed by batch.
    first_idx <- apply(reached_mat, 2L, function(col) {
      w <- which(col)
      if (length(w)) w[[1L]] else NA_integer_
    })
    vapply(seq_along(batch_grid),
      function(i) mean(!is.na(first_idx) & first_idx <= i), numeric(1))
  }

  crossing_quantiles <- vapply(probs, function(p) {
    idx <- which(cdf >= p)
    if (length(idx)) as.numeric(batch_grid[idx[[1L]]]) else NA_real_
  }, numeric(1))
  names(crossing_quantiles) <- sprintf("q%g", probs)

  list(
    quantiles = crossing_quantiles,
    p_never = 1 - max(cdf),
    grid = data.table(batch = batch_grid, cdf = cdf)
  )
}
