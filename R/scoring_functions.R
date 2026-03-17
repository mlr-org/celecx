#' @title Scoring Functions for Pool-Based Active Learning
#'
#' @description
#' Factory functions that return scoring closures for use with [OptimizerGS].
#' Each factory returns a function with signature:
#'
#' `function(pool_x, unqueried_idx, learner, queried_idx, queried_y, target_id,
#'           pool_x_raw = NULL)`
#'
#' where:
#' - `pool_x`: numeric matrix (N x d) of scaled features used for distance
#'   computations
#' - `unqueried_idx`: integer vector of unqueried pool indices
#' - `learner`: fitted [mlr3::LearnerRegr] or `NULL`
#' - `queried_idx`: integer vector of queried pool indices
#' - `queried_y`: numeric vector of true labels for queried points
#' - `target_id`: string, target column name
#' - `pool_x_raw`: optional numeric matrix (N x d) of raw (unscaled) features.
#'   When supplied, model-based scoring functions use these for learner
#'   predictions instead of `pool_x`, keeping the learner's input contract
#'   independent of the distance-scaling convention. When `NULL`, `pool_x` is
#'   used for both distances and predictions (backward-compatible).
#'
#' Returns a numeric vector of scores (length = `length(unqueried_idx)`),
#' where higher is better.
#'
#' @name scoring_functions
NULL

#' @describeIn scoring_functions Random scores (baseline).
#' @export
scoring_random <- function() {
  structure(function(pool_x, unqueried_idx, learner, queried_idx, queried_y,
      target_id, pool_x_raw = NULL) {
    stats::runif(length(unqueried_idx))
  },
  method_id = "random",
  supports_batch = TRUE)
}

#' @describeIn scoring_functions GSx: maximin input-space distance. Model-free.
#' @export
scoring_gsx <- function() {
  structure(function(pool_x, unqueried_idx, learner, queried_idx, queried_y,
      target_id, pool_x_raw = NULL) {
    queried_x <- pool_x[queried_idx, , drop = FALSE]
    cand_x <- pool_x[unqueried_idx, , drop = FALSE]

    n_cand <- nrow(cand_x)
    n_queried <- nrow(queried_x)

    scores <- numeric(n_cand)
    for (i in seq_len(n_cand)) {
      diffs <- sweep(queried_x, 2, cand_x[i, ], "-")
      dists <- sqrt(rowSums(diffs^2))
      scores[i] <- min(dists)
    }
    scores
  },
  method_id = "gsx",
  supports_batch = TRUE)
}

#' @describeIn scoring_functions GSy: maximin output-space distance.
#'   Compares candidate predictions to selected true labels.
#' @export
scoring_gsy <- function() {
  structure(function(pool_x, unqueried_idx, learner, queried_idx, queried_y,
      target_id, pool_x_raw = NULL) {
    predict_x <- pool_x_raw %??% pool_x
    cand_dt <- as.data.table(predict_x[unqueried_idx, , drop = FALSE])
    pred <- learner$predict_newdata(cand_dt)
    yhat <- pred$response

    n_cand <- length(unqueried_idx)
    scores <- numeric(n_cand)
    for (i in seq_len(n_cand)) {
      scores[i] <- min(abs(yhat[i] - queried_y))
    }
    scores
  },
  method_id = "gsy",
  supports_batch = FALSE)
}

#' @describeIn scoring_functions iGS: maximin joint input-output product.
#'   The product is inside the min: `score(n) = min_m(d_x * d_y)`.
#' @export
scoring_igs <- function() {
  structure(function(pool_x, unqueried_idx, learner, queried_idx, queried_y,
      target_id, pool_x_raw = NULL) {
    queried_x <- pool_x[queried_idx, , drop = FALSE]
    cand_x <- pool_x[unqueried_idx, , drop = FALSE]

    predict_x <- pool_x_raw %??% pool_x
    cand_dt <- as.data.table(predict_x[unqueried_idx, , drop = FALSE])
    pred <- learner$predict_newdata(cand_dt)
    yhat <- pred$response

    n_cand <- length(unqueried_idx)
    scores <- numeric(n_cand)
    for (i in seq_len(n_cand)) {
      diffs_x <- sweep(queried_x, 2, cand_x[i, ], "-")
      dists_x <- sqrt(rowSums(diffs_x^2))
      dists_y <- abs(yhat[i] - queried_y)
      # Product inside the min (NOT product of separate minima)
      scores[i] <- min(dists_x * dists_y)
    }
    scores
  },
  method_id = "igs",
  supports_batch = FALSE)
}

#' @describeIn scoring_functions QBC: committee disagreement via bootstrap.
#' @param k_qbc Number of bootstrap committee members (default 5).
#' @export
scoring_qbc <- function(k_qbc = 5L) {
  assert_int(k_qbc, lower = 2L)
  structure(function(pool_x, unqueried_idx, learner, queried_idx, queried_y,
      target_id, pool_x_raw = NULL) {
    predict_x <- pool_x_raw %??% pool_x
    n_queried <- length(queried_idx)
    n_cand <- length(unqueried_idx)

    preds_matrix <- matrix(NA_real_, nrow = n_cand, ncol = k_qbc)
    cand_dt <- as.data.table(predict_x[unqueried_idx, , drop = FALSE])

    for (j in seq_len(k_qbc)) {
      boot_idx <- sample.int(n_queried, n_queried, replace = TRUE)
      boot_x <- predict_x[queried_idx[boot_idx], , drop = FALSE]
      boot_y <- queried_y[boot_idx]

      train_dt <- as.data.table(boot_x)
      train_dt[[target_id]] <- boot_y
      task <- as_task_regr(train_dt, target = target_id)

      model_j <- learner$clone(deep = TRUE)
      model_j$train(task)
      pred <- model_j$predict_newdata(cand_dt)
      preds_matrix[, j] <- pred$response
    }

    # Variance across committee for each candidate
    apply(preds_matrix, 1, var)
  },
  method_id = "qbc",
  supports_batch = FALSE)
}
