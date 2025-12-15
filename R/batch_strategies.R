#' @title Batch Selection Strategies
#'
#' @description
#' Strategies for selecting multiple points from scored candidates. These are
#' used by [BatchProposer] to construct batches for parallel evaluation.
#'
#' @details
#' When batch_size > 1, selecting the top-k candidates by acquisition score
#' may result in redundant evaluations (e.g., many points clustered around
#' the current best). Batch strategies address this by diversifying the
#' selection.
#'
#' All batch strategy functions return a **strategy function** with signature:
#' ```
#' function(candidates, scores, batch_size, surrogate, archive, search_space)
#' ```
#' The strategy function returns integer indices into the candidates table.
#'
#' @section Available Strategies:
#' - `batch_strategy_greedy()`: Select top-k by acquisition score (baseline)
#' - `batch_strategy_local_penalization()`: Penalize around selected points
#' - `batch_strategy_diversity()`: Mix acquisition score with distance to selected
#'
#' @name batch_strategies
NULL


#' @title Greedy Batch Strategy
#'
#' @description
#' Simple strategy that selects the k candidates with the best acquisition
#' scores. No diversification; may select clustered points.
#'
#' @return A batch strategy function.
#'
#' @details
#' This is the baseline strategy. Use when:
#' - batch_size = 1 (single point selection)
#' - Candidates are already diverse (e.g., from LHS)
#' - Speed is critical
#'
#' @export
batch_strategy_greedy <- function() {
  function(candidates, scores, batch_size, surrogate, archive, search_space) {
    # Order by scores (ascending, lower is better) and return first batch_size
    order(scores)[seq_len(batch_size)]
  }
}


#' @title Local Penalization Batch Strategy
#'
#' @description
#' After selecting each point, penalizes nearby candidates before selecting
#' the next. Encourages spatial diversity in the batch.
#'
#' @param bandwidth (`numeric(1)`)\cr
#'   Penalization bandwidth. Points within this distance (normalized) of a
#'   selected point have their scores penalized. Default is 0.1 (10% of domain
#'   range).
#' @param penalization (`numeric(1)`)\cr
#'   How much to worsen scores of nearby points. Added to acquisition score.
#'   Default is Inf (effectively removes nearby candidates).
#'
#' @return A batch strategy function.
#'
#' @details
#' Algorithm:
#' 1. Select the best candidate
#' 2. Penalize scores of candidates within `bandwidth` of the selected point
#' 3. Select the next best (from penalized scores)
#' 4. Repeat until batch is complete
#'
#' Distance is computed using Gower distance (handles mixed types).
#'
#' @references
#' Ginsbourger, D., Le Riche, R., & Carraro, L. (2010). Kriging is well-suited
#' to parallelize optimization.
#'
#' @export
batch_strategy_local_penalization <- function(bandwidth = 0.1, penalization = Inf) {
  assert_number(bandwidth, lower = 0, upper = 1)
  assert_number(penalization, lower = 0)

  function(candidates, scores, batch_size, surrogate, archive, search_space) {
    n_candidates <- nrow(candidates)
    selected <- integer(0)
    working_scores <- scores

    for (i in seq_len(batch_size)) {
      # Find best unselected candidate
      available <- setdiff(seq_len(n_candidates), selected)
      best_idx <- available[which_min(working_scores[available])]
      selected <- c(selected, best_idx)

      if (i < batch_size && bandwidth > 0) {
        # Compute distances from selected point to all candidates
        selected_point <- candidates[best_idx]
        distances <- compute_gower_distance(candidates, selected_point, search_space)

        # Penalize candidates within bandwidth
        nearby <- which(distances[, 1] <= bandwidth)
        working_scores[nearby] <- working_scores[nearby] + penalization
      }
    }

    selected
  }
}


#' @title Diversity Batch Strategy
#'
#' @description
#' Combines acquisition score with distance to already-selected points.
#' Trades off exploitation (good acquisition) with exploration (diversity).
#'
#' @param diversity_weight (`numeric(1)`)\cr
#'   Weight for distance component. Range `[0, 1]`.
#'   - 0: Pure acquisition (same as greedy)
#'   - 1: Pure diversity (ignore acquisition scores)
#'   - 0.5: Equal weight to both
#'
#' @return A batch strategy function.
#'
#' @details
#' Combined score for candidate i:
#' ```
#' combined = (1 - w) * normalized_acq[i] + w * min_distance_to_selected[i]
#' ```
#'
#' After each selection, distances are recomputed.
#'
#' @export
batch_strategy_diversity <- function(diversity_weight = 0.5) {
  assert_number(diversity_weight, lower = 0, upper = 1)

  function(candidates, scores, batch_size, surrogate, archive, search_space) {
    n_candidates <- nrow(candidates)
    w <- diversity_weight

    # Normalize acquisition scores to [0, 1], lower is better
    score_range <- max(scores) - min(scores)
    if (score_range == 0) {
      acq_norm <- rep(0, n_candidates)
    } else {
      acq_norm <- (scores - min(scores)) / score_range
    }

    selected <- integer(0)

    for (i in seq_len(batch_size)) {
      available <- setdiff(seq_len(n_candidates), selected)

      if (i == 1 || w == 0) {
        # First point or pure acquisition: select by acquisition score
        best_idx <- available[which_min(acq_norm[available])]
      } else {
        # Compute min distance from each candidate to selected set
        selected_points <- candidates[selected]
        dist_matrix <- compute_gower_distance(candidates, selected_points, search_space)
        min_dist <- apply(dist_matrix, 1, min)

        # Normalize distances to [0, 1], higher distance = better
        dist_range <- max(min_dist[available]) - min(min_dist[available])
        if (dist_range == 0) {
          dist_norm <- rep(0.5, n_candidates)
        } else {
          dist_norm <- (min_dist - min(min_dist[available])) / dist_range
        }

        # Combined score: lower is better
        # acq_norm: lower = better acquisition
        # dist_norm: higher = more diverse (farther from selected)
        # We want to minimize (1-w)*acq - w*dist
        combined <- (1 - w) * acq_norm - w * dist_norm

        best_idx <- available[which_min(combined[available])]
      }

      selected <- c(selected, best_idx)
    }

    selected
  }
}


# =============================================================================
# Helper Functions
# =============================================================================

#' @title Compute Gower Distance Matrix
#'
#' @description
#' Computes pairwise Gower distances, which handle mixed numeric and
#' categorical variables.
#'
#' @param x (`data.table`) First set of points.
#' @param y (`data.table`) Second set of points (optional; if NULL, computes
#'   distances within x).
#' @param search_space ([paradox::ParamSet]) For normalization bounds.
#'
#' @return Distance matrix with nrow(x) rows and nrow(y) columns (or nrow(x)
#'   columns if y is NULL).
#'
#' @keywords internal
compute_gower_distance <- function(x, y = NULL, search_space = NULL) {
  self_distance <- is.null(y)
  if (self_distance) y <- x

  n_x <- nrow(x)
  n_y <- nrow(y)
  col_names <- names(x)
  n_cols <- length(col_names)

  # Get param classes and bounds from search_space
  param_classes <- search_space$class
  lower <- search_space$lower
  upper <- search_space$upper

  # Initialize distance matrix
  dist_matrix <- matrix(0, nrow = n_x, ncol = n_y)

  # Compute per-variable distance contributions
  for (col in col_names) {
    param_class <- param_classes[[col]]
    x_col <- x[[col]]
    y_col <- y[[col]]

    if (param_class %in% c("ParamDbl", "ParamInt")) {
      # Numeric: normalized absolute difference
      range_col <- upper[[col]] - lower[[col]]
      if (range_col == 0) {
        # All values are the same
        col_dist <- matrix(0, nrow = n_x, ncol = n_y)
      } else {
        # |x_i - y_j| / range
        col_dist <- abs(outer(x_col, y_col, "-")) / range_col
      }
    } else {
      # Categorical (ParamFct, ParamLgl): 0 if equal, 1 if different
      col_dist <- outer(x_col, y_col, function(a, b) as.numeric(a != b))
    }

    dist_matrix <- dist_matrix + col_dist
  }

  # Average across all variables
  dist_matrix <- dist_matrix / n_cols

  dist_matrix
}

