#' @title Feature Scaling Utilities
#'
#' @description
#' Scaling functions for pool-based active learning methods.
#'
#' - `standardize_features()`: standardize to mean 0, sd 1 (GS family)
#' - `affine_scale_features()`: affine scale to \eqn{[-1, 1]} (IDEAL)
#' - `affine_scaler_from_bounds()`: build affine scaler from explicit bounds
#'
#' @name feature_scaling
#' @keywords internal
NULL

#' Standardize features to mean 0, sd 1.
#'
#' Computes column means and standard deviations from the pool, scales
#' all features accordingly. Zero-variance features are set to 0.
#'
#' @param pool_x (`matrix`)\cr
#'   Numeric matrix (N x d).
#' @return Named `list` with `scaled` (`matrix`), `center` (`numeric`), `scale` (`numeric`),
#'   `zero_var` (`logical`).
#' @keywords internal
standardize_features <- function(pool_x) {
  center <- colMeans(pool_x)
  sds <- apply(pool_x, 2, sd)
  zero_var <- sds == 0
  sds[zero_var] <- 1  # avoid division by zero; centered values are already 0
  scaled <- sweep(pool_x, 2, center, "-")
  scaled <- sweep(scaled, 2, sds, "/")
  if (any(zero_var)) scaled[, zero_var] <- 0
  list(scaled = scaled, center = center, scale = sds, zero_var = zero_var)
}

#' Apply standardization to new data.
#'
#' @param x (`matrix`)\cr
#'   Numeric matrix with same number of columns as original pool.
#' @param scaler (`list`)\cr
#'   List returned by [standardize_features()].
#' @return Scaled `matrix`.
#' @keywords internal
standardize_apply <- function(x, scaler) {
  scaled <- sweep(x, 2, scaler$center, "-")
  scaled <- sweep(scaled, 2, scaler$scale, "/")
  if (any(scaler$zero_var)) scaled[, scaler$zero_var] <- 0
  scaled
}

#' Affine scale features to [-1, 1].
#'
#' For each dimension, maps to \eqn{[-1, 1]} using the pool's min and max.
#' Zero-range features are set to 0.
#'
#' @param pool_x (`matrix`)\cr
#'   Numeric matrix (N x d).
#' @return Named `list` with entries `scaled` (`matrix`), `mins` (`numeric`),
#'    and `maxs` (`numeric`).
#' @keywords internal
affine_scale_features <- function(pool_x) {
  scaler <- affine_scaler_from_bounds(
    lower = apply(pool_x, 2, min),
    upper = apply(pool_x, 2, max)
  )
  scaler$scaled <- affine_scale_apply(pool_x, scaler)
  scaler
}

#' Build affine scaler from explicit bounds.
#'
#' @param lower (`numeric`)\cr
#'   Vector of lower bounds.
#' @param upper (`numeric`)\cr
#'   Vector of upper bounds.
#' @return Named `list` with entries `mins` (`numeric`) and `maxs` (`numeric`)
#' @keywords internal
affine_scaler_from_bounds <- function(lower, upper) {
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  assert_numeric(lower, any.missing = FALSE, min.len = 1L)
  assert_numeric(upper, any.missing = FALSE, len = length(lower))

  list(
    mins = lower,
    maxs = upper
  )
}

#' Apply affine scaling to new data.
#'
#' @param x (`matrix`)\cr
#'   Numeric matrix with same number of columns as original pool.
#' @param scaler (`list`)\cr
#'   List returned by [affine_scale_features()].
#' @return Scaled `matrix`.
#' @keywords internal
affine_scale_apply <- function(x, scaler) {
  ranges <- scaler$maxs - scaler$mins
  mids <- (scaler$maxs + scaler$mins) / 2
  zero_range <- scaler$maxs == scaler$mins
  ranges[zero_range] <- 1
  scaled <- sweep(x, 2, mids, "-")
  scaled <- sweep(scaled, 2, ranges / 2, "/")
  if (any(zero_range)) scaled[, zero_range] <- 0
  scaled
}
