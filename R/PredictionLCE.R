#' @title LCE Prediction Object
#'
#' @name PredictionLCE
#'
#' @include TaskLCE.R
#'
#' @description
#' Prediction object for [TaskLCE]. Carries the per-row true and predicted
#' surrogate performance and, depending on the producing learner's predict type,
#' a distributional payload describing the predictive of the performance curve
#' `f(b)`:
#'
#' * `response` (`numeric()`): predictive median on the natural scale. For the
#'   sample-based learners this is the mean of the draws on the [lce_link] scale,
#'   back-transformed -- which is the median whenever the link-scale distribution
#'   is symmetric (e.g. Gaussian). Always present.
#' * `se` (`numeric()`): *total* predictive standard deviation on the link scale
#'   (epistemic + aleatoric), the quantity to score against the realised `y_b`.
#' * `se_epistemic` (`numeric()`): standard deviation of the *mean* `f(b)` on the
#'   link scale (epistemic only), the quantity for expected-crossing decisions.
#' * `quantiles` (`matrix()`): predictive quantiles, one row per observation, one
#'   column per probability (carried in the `"probs"` attribute).
#' * `samples` (`matrix()`): predictive draws, one row per observation, one column
#'   per draw. Columns are joint sample paths for the sample-based learners.
#' * `target_reached` (`matrix()`): probability that the metric has reached a
#'   target, one row per observation, one column per target (carried in the
#'   `"target"` attribute).
#'
#' The link scale on which `se` / `se_epistemic` live is a property of the
#' [TaskLCE] (its `link`); the prediction itself stays a few plain numeric /
#' matrix columns.
#'
#' @export
PredictionLCE <- R6Class("PredictionLCE",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new PredictionLCE.
    #'
    #' @param task ([TaskLCE])\cr
    #'   Task used to derive row ids and truth.
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predictions.
    #' @param truth (`numeric()`)\cr
    #'   True surrogate performances.
    #' @param response (`numeric()`)\cr
    #'   Predicted surrogate performances (natural-scale predictive median).
    #' @param se (`numeric()`)\cr
    #'   Total predictive standard error on the link scale.
    #' @param se_epistemic (`numeric()`)\cr
    #'   Epistemic standard error of the mean on the link scale.
    #' @param quantiles (`matrix()`)\cr
    #'   Predicted quantiles (rows = observations, columns = probabilities). The
    #'   probabilities must be stored in the `"probs"` attribute.
    #' @param samples (`matrix()`)\cr
    #'   Predictive draws (rows = observations, columns = draws).
    #' @param target_reached (`matrix()`)\cr
    #'   Reach probabilities (rows = observations, columns = targets). The targets
    #'   must be stored in the `"target"` attribute.
    #' @param weights (`numeric()`)\cr
    #'   Optional measure weights.
    #' @param check (`logical(1)`)\cr
    #'   Whether to validate the inputs.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(),
        response = NULL, se = NULL, se_epistemic = NULL, quantiles = NULL,
        samples = NULL, target_reached = NULL, weights = NULL, check = TRUE) {
      pdata <- new_prediction_data_lce(list(
        row_ids = row_ids, truth = truth, response = response,
        se = se, se_epistemic = se_epistemic, quantiles = quantiles,
        samples = samples, target_reached = target_reached, weights = weights
      ))
      if (check) {
        pdata <- check_prediction_data(pdata)
      }
      self$task_type <- "lce"
      self$man <- "celecx::PredictionLCE"
      self$data <- pdata
      predict_types <- intersect(
        names(mlr_reflections$learner_predict_types[["lce"]]), names(pdata))
      # quantiles / samples carry the response implicitly, so always report it.
      if (any(c("quantiles", "samples") %chin% predict_types)) {
        predict_types <- union(predict_types, "response")
      }
      self$predict_types <- predict_types
    }
  ),

  active = list(
    #' @field response (`numeric()`)\cr
    #' Predicted surrogate performance for each row.
    response = function(rhs) {
      assert_ro_binding(rhs)
      self$data$response %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field se (`numeric()`)\cr
    #' Total predictive standard error (link scale), or `NA` vector when absent.
    se = function(rhs) {
      assert_ro_binding(rhs)
      self$data$se %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field se_epistemic (`numeric()`)\cr
    #' Epistemic standard error of the mean (link scale), or `NA` when absent.
    se_epistemic = function(rhs) {
      assert_ro_binding(rhs)
      self$data$se_epistemic %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field quantiles (`matrix()`)\cr
    #' Matrix of predicted quantiles (rows = observations, columns ascending).
    quantiles = function(rhs) {
      assert_ro_binding(rhs)
      self$data$quantiles
    },

    #' @field samples (`matrix()`)\cr
    #' Matrix of predictive draws (rows = observations, columns = draws).
    samples = function(rhs) {
      assert_ro_binding(rhs)
      self$data$samples
    },

    #' @field target_reached (`matrix()`)\cr
    #' Matrix of reach probabilities (rows = observations, columns = targets).
    target_reached = function(rhs) {
      assert_ro_binding(rhs)
      self$data$target_reached
    }
  )
)

#' @export
as.data.table.PredictionLCE <- function(x, ...) {  # nolint
  tab <- as.data.table(x$data[c("row_ids", "truth", "response", "se", "se_epistemic")])
  if (!is.null(x$data$quantiles)) {
    tab <- rcbind(tab, as.data.table(x$data$quantiles))
  }
  if (!is.null(x$data$samples)) {
    tab <- rcbind(tab, as.data.table(x$data$samples))
  }
  if (!is.null(x$data$target_reached)) {
    tab <- rcbind(tab, as.data.table(x$data$target_reached))
  }
  if (!is.null(x$data$weights)) {
    tab$weights <- x$data$weights
  }
  tab
}

#' @title LCE Prediction Data
#'
#' @name PredictionDataLCE
#'
#' @description
#' Methods for the intermediate [mlr3::PredictionData] representation of
#' [PredictionLCE]: validation, missingness detection, row filtering, and
#' concatenation across resampling iterations (which re-applies the attributes
#' of matrix predict-type columns that row-subsetting drops).
#'
#' @param pdata (`PredictionDataLCE`)\cr
#'   Named list of prediction columns, inheriting from `"PredictionDataLCE"`.
#' @param row_ids (`integer()`)\cr
#'   Row indices to keep.
#' @param ... (`PredictionDataLCE` objects | ignored)\cr
#'   For the `c()` method, the objects to concatenate; ignored otherwise.
#' @param keep_duplicates (`logical(1)`)\cr
#'   If `FALSE`, rows with row ids that reappear in later objects are removed,
#'   keeping the last occurrence.
#'
#' @keywords internal
NULL

#' @rdname PredictionDataLCE
#' @export
check_prediction_data.PredictionDataLCE <- function(pdata, ...) {  # nolint
  pdata$row_ids <- assert_row_ids(pdata$row_ids)
  n <- length(pdata$row_ids)
  if (is.null(pdata$truth)) pdata$truth <- NA_real_
  if (!n) pdata$truth <- numeric(0)

  if (!is.null(pdata$response)) {
    pdata$response <- assert_numeric(unname(pdata$response))
    lce_assert_prediction_count(length(pdata$response), n, "response")
  }

  for (col in c("se", "se_epistemic")) {
    if (!is.null(pdata[[col]])) {
      pdata[[col]] <- assert_numeric(unname(pdata[[col]]), lower = 0)
      lce_assert_prediction_count(length(pdata[[col]]), n, col)
    }
  }

  if (!is.null(pdata$quantiles)) {
    quantiles <- pdata$quantiles
    assert_matrix(quantiles)
    lce_assert_prediction_count(nrow(quantiles), n, "quantiles")
    probs <- attr(quantiles, "probs")
    assert_numeric(probs, lower = 0, upper = 1, any.missing = FALSE,
      len = ncol(quantiles), sorted = TRUE, .var.name = "probs attribute of quantiles")
    colnames(pdata$quantiles) <- sprintf("q%g", probs)
  }

  if (!is.null(pdata$samples)) {
    assert_matrix(pdata$samples)
    lce_assert_prediction_count(nrow(pdata$samples), n, "samples")
    colnames(pdata$samples) <- sprintf("s%i", seq_len(ncol(pdata$samples)))
  }

  if (!is.null(pdata$target_reached)) {
    tr <- pdata$target_reached
    assert_matrix(tr)
    lce_assert_prediction_count(nrow(tr), n, "target_reached")
    target <- attr(tr, "target")
    assert_numeric(target, any.missing = FALSE, len = ncol(tr),
      .var.name = "target attribute of target_reached")
    colnames(pdata$target_reached) <- sprintf("reached%g", target)
  }

  if (!is.null(pdata$weights)) {
    pdata$weights <- assert_numeric(unname(pdata$weights), any.missing = FALSE)
    lce_assert_prediction_count(length(pdata$weights), n, "weights")
  }

  pdata
}

#' @rdname PredictionDataLCE
#' @export
is_missing_prediction_data.PredictionDataLCE <- function(pdata, ...) {  # nolint
  miss <- logical(length(pdata$row_ids))
  if (!is.null(pdata$response)) miss <- is.na(pdata$response)
  if (!is.null(pdata$se)) miss <- miss | is.na(pdata$se)
  if (!is.null(pdata$se_epistemic)) miss <- miss | is.na(pdata$se_epistemic)
  for (col in c("quantiles", "samples", "target_reached")) {
    if (!is.null(pdata[[col]])) miss <- miss | apply(pdata[[col]], 1L, anyMissing)
  }
  pdata$row_ids[miss]
}

#' @rdname PredictionDataLCE
#' @export
filter_prediction_data.PredictionDataLCE <- function(pdata, row_ids, ...) {  # nolint
  keep <- pdata$row_ids %in% row_ids
  pdata$row_ids <- pdata$row_ids[keep]
  pdata$truth <- pdata$truth[keep]
  for (col in c("response", "se", "se_epistemic", "weights")) {
    if (!is.null(pdata[[col]])) pdata[[col]] <- pdata[[col]][keep]
  }
  for (col in c("quantiles", "samples", "target_reached")) {
    if (!is.null(pdata[[col]])) {
      pdata[[col]] <- lce_keep_matrix_attrs(pdata[[col]], pdata[[col]][keep, , drop = FALSE])
    }
  }
  pdata
}

# Row-subsetting a matrix drops the probs / target attributes; re-apply them.
lce_keep_matrix_attrs <- function(orig, new) {
  for (nm in c("probs", "target")) {
    a <- attr(orig, nm)
    if (!is.null(a)) setattr(new, nm, a)
  }
  new
}

#' @rdname PredictionDataLCE
#' @export
c.PredictionDataLCE <- function(..., keep_duplicates = TRUE) {  # nolint
  dots <- list(...)
  assert_list(dots, "PredictionDataLCE")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) return(dots[[1L]])

  predict_type_names <- names(mlr_reflections$learner_predict_types[["lce"]])
  per_dot_types <- map(dots, function(x) intersect(names(x), predict_type_names))
  if (!every(per_dot_types[-1L], setequal, y = per_dot_types[[1L]])) {
    stopf("Cannot combine PredictionDataLCE: differing predict types")
  }
  if (length(unique(map_lgl(dots, function(x) is.null(x$weights)))) > 1L) {
    stopf("Cannot combine PredictionDataLCE: some have weights, some do not")
  }

  vector_cols <- c("row_ids", "truth",
    intersect(per_dot_types[[1L]], c("response", "se", "se_epistemic")),
    if (!is.null(dots[[1L]]$weights)) "weights")
  tab <- map_dtr(dots, function(x) x[vector_cols], .fill = FALSE)

  # Matrix columns: row-bind while preserving the probs / target attributes.
  bind_matrix <- function(col, attr_name) {
    if (col %nin% per_dot_types[[1L]]) return(NULL)
    parts <- map(dots, col)
    # `samples` carries no attribute (attr_name = NULL), so guard the read as
    # well as the write -- attr(x, NULL) errors.
    attr_val <- if (!is.null(attr_name)) attr(parts[[1L]], attr_name)
    mat <- do.call(rbind, parts)
    if (!is.null(attr_name)) setattr(mat, attr_name, attr_val)
    mat
  }
  quantiles <- bind_matrix("quantiles", "probs")
  samples <- bind_matrix("samples", NULL)
  target_reached <- bind_matrix("target_reached", "target")

  if (!keep_duplicates) {
    keep <- !duplicated(tab, by = "row_ids", fromLast = TRUE)
    tab <- tab[keep]
    if (!is.null(quantiles)) quantiles <- lce_keep_matrix_attrs(quantiles, quantiles[keep, , drop = FALSE])
    if (!is.null(samples)) samples <- samples[keep, , drop = FALSE]
    if (!is.null(target_reached)) target_reached <- lce_keep_matrix_attrs(target_reached, target_reached[keep, , drop = FALSE])
  }

  result <- as.list(tab)
  result$quantiles <- quantiles
  result$samples <- samples
  result$target_reached <- target_reached
  new_prediction_data_lce(result)
}

#' @export
create_empty_prediction_data.TaskLCE <- function(task, learner) {  # nolint
  predict_types <- mlr_reflections$learner_predict_types[["lce"]][[learner$predict_type]]
  pdata <- list(row_ids = integer(0), truth = numeric(0))
  for (col in intersect(predict_types, c("response", "se", "se_epistemic"))) {
    pdata[[col]] <- numeric(0)
  }
  if ("quantiles" %chin% predict_types) {
    probs <- learner$param_set$values$quantile_probs %??% lce_default_probs
    pdata$quantiles <- structure(matrix(numeric(0), nrow = 0L, ncol = length(probs)),
      probs = probs)
  }
  if ("samples" %chin% predict_types) {
    # The draw count is learner-specific (e.g. n_bootstrap, n_restarts).
    pv <- learner$param_set$values
    k <- pv$n_bootstrap %??% pv$n_restarts %??% lce_default_n_samples
    pdata$samples <- matrix(numeric(0), nrow = 0L, ncol = k)
  }
  if ("target_reached" %chin% predict_types && !is.null(learner$param_set$values$reach_target)) {
    target <- learner$param_set$values$reach_target
    pdata$target_reached <- structure(
      matrix(numeric(0), nrow = 0L, ncol = length(target)), target = target)
  }
  if ("weights_measure" %chin% task$properties) pdata$weights <- numeric(0)
  new_prediction_data_lce(pdata)
}

# mlr3's `assert_prediction_count` is internal; replicate its message for the
# "lce" prediction-data checks.
lce_assert_prediction_count <- function(actual, expected, type) {
  if (actual != expected) {
    stopf("Predicted %s has %i entries, but the task has %i observations",
      type, actual, expected)
  }
}

# mlr3's `new_prediction_data` is internal. Replicate it for the "lce" task
# type by stamping the PredictionDataLCE S3 class on a NULL-stripped list.
new_prediction_data_lce <- function(li) {
  li <- discard(li, is.null)
  class(li) <- c("PredictionDataLCE", "PredictionData")
  li
}

#' @export
as_prediction.PredictionDataLCE <- function(x, check = FALSE, ...) {  # nolint
  invoke(PredictionLCE$new, check = check, .args = x)
}
