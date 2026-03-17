#' @title IDW Geometry for IDEAL
#'
#' @description
#' Inverse-distance-weighted geometry computations for the IDEAL algorithm.
#' Implements the exponentially damped IDW weights, exploration term, and
#' acquisition function from the IDEAL paper.
#'
#' @name idw_geometry
#' @keywords internal
NULL

#' Compute IDW weights and exploration term for a single point.
#'
#' Given a query point and all previously evaluated points, computes
#' normalized IDW weights \eqn{v_k(x)} and exploration term \eqn{z(x)}.
#'
#' The raw weight is \eqn{w_k(x) = exp(-d^2) / d^2} where \eqn{d^2} is the
#' squared Euclidean distance in scaled space. The exact-match branch sets
#' \eqn{v_k = 1} and \eqn{z = 0} when a query point coincides with an
#' evaluated point.
#'
#' @param x_scaled (`numeric(d)`)\cr
#'   Query point.
#' @param attempted_x_scaled (`matrix`)\cr
#'   K x d matrix of all evaluated points.
#' @param eps_eq (`numeric(1)`)\cr
#'   Tolerance for exact-match detection.
#' @return Named `list` with entries `v` (numeric vector length K) and `z` (scalar).
#' @keywords internal
idw_geometry <- function(x_scaled, attempted_x_scaled, eps_eq = 1e-12) {
  K <- nrow(attempted_x_scaled)
  d2 <- rowSums(sweep(attempted_x_scaled, 2, x_scaled, "-")^2)

  # Exact-match branch
  exact <- which(d2 <= eps_eq)
  if (length(exact) > 0L) {
    v <- rep(0, K)
    v[exact[1L]] <- 1
    return(list(v = v, z = 0))
  }

  # Raw IDW weights: w_k = exp(-d2_k) / d2_k
  raw_w <- exp(-d2) / d2
  W <- sum(raw_w)
  v <- raw_w / W
  z <- (2 / pi) * atan(1 / W)

  list(v = v, z = z)
}

#' Compute IDEAL acquisition values for candidate pool points.
#'
#' For each candidate, computes:
#' \deqn{a(x) = \sum_{k \in Q} v_k(x) (y_k - \hat{y}(x))^2 + \delta z(x)}
#'
#' where \eqn{v_k} are normalized IDW weights over all attempted points
#' and the uncertainty sum uses only successful points in Q.
#'
#' @param candidates_scaled (`matrix`)\cr
#'   M x d matrix of scaled candidate points.
#' @param candidates_yhat (`numeric(M)`)\cr
#'   Model predictions for candidate points.
#' @param attempted_x_scaled (`matrix`)\cr
#'   K x d matrix of all evaluated points (scaled).
#' @param attempted_y (`numeric(K)`)\cr
#'   Targets for evaluated points (may contain NA for failures).
#' @param Q (`integer`)\cr
#'   Vector of indices into evaluated points for successful queries.
#' @param delta (`numeric(1)`)\cr
#'   Exploration weight (>= 0).
#' @param target_scale (`numeric(1)`)\cr
#'   Scaling factor for residuals. Default 1.
#' @param eps_eq (`numeric(1)`)\cr
#'   Tolerance for exact-match detection.
#' @return `numeric(M)`: Acquisition values.
#' @keywords internal
idw_acquisition <- function(candidates_scaled, candidates_yhat,
    attempted_x_scaled, attempted_y, Q, delta, target_scale = 1,
    eps_eq = 1e-12) {
  M <- nrow(candidates_scaled)
  scores <- numeric(M)
  target_scale_sq <- target_scale ^ 2

  for (i in seq_len(M)) {
    geom <- idw_geometry(candidates_scaled[i, ], attempted_x_scaled, eps_eq)
    # The paper writes s2 without an explicit output-scale divisor, but the
    # released reference implementation normalizes by the current labeled-target
    # range squared. We keep that alignment here.
    s2 <- sum(geom$v[Q] * (attempted_y[Q] - candidates_yhat[i])^2) / target_scale_sq
    scores[i] <- s2 + delta * geom$z
  }

  scores
}
