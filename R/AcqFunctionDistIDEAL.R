#' @title Distance-Aware IDEAL Acquisition Function
#'
#' @include AcqFunctionDist.R
#'
#' @description
#' Inverse-distance-weighted active-learning acquisition function using an
#' [ALDistance].
#'
#' The basic score is an IDW-weighted squared residual plus the IDEAL
#' exploration term. With `omega > 0`, `$fit_pool()` precomputes the IDEAL
#' pool-density multiplier on the fitted candidate pool. When fitted with
#' `xdt = NULL`, the population-case density multiplier is set to `1`.
#'
#' @export
#' @family Acquisition Function
AcqFunctionDistIDEAL <- R6Class("AcqFunctionDistIDEAL",
  inherit = AcqFunctionDist,

  public = list(

    #' @description
    #' Creates a new distance-aware IDEAL acquisition function.
    #'
    #' @param surrogate (`NULL` | [mlr3mbo::SurrogateLearner])\cr
    #'   Surrogate used for mean predictions.
    #' @param al_distance ([ALDistance] | `NULL`)\cr
    #'   Distance used for the IDW geometry.
    #' @param delta (`numeric(1)`)\cr
    #'   Exploration weight.
    #' @param omega (`numeric(1)`)\cr
    #'   Density multiplier weight.
    #' @param tolerance_equality (`numeric(1)`)\cr
    #'   Squared-distance tolerance for the exact-match branch.
    initialize = function(surrogate = NULL,
        al_distance = clx_ald("affine"),
        delta = 1,
        omega = 0,
        tolerance_equality = 1e-12) {

      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)

      constants <- ps(
        delta = p_dbl(lower = 0, init = delta),
        omega = p_dbl(lower = 0, init = omega),
        tolerance_equality = p_dbl(lower = 0, init = tolerance_equality)
      )

      super$initialize(
        id = "acq_dist_ideal",
        constants = constants,
        surrogate = surrogate,
        al_distance = al_distance,
        requires_predict_type_se = FALSE,
        surrogate_class = "SurrogateLearner",
        direction = "maximize",
        label = "Distance-Aware IDEAL Acquisition Function",
        man = "celecx::AcqFunctionDistIDEAL"
      )
    },

    #' @description
    #' Clears precomputed density values.
    #'
    #' @return `self`.
    reset = function() {
      private$.rho_dt <- NULL
      private$.rho_feature_ids <- NULL
      private$.rho_search_space <- FALSE
      super$reset()
      invisible(self)
    }
  ),

  private = list(
    .rho_dt = NULL,
    .rho_feature_ids = NULL,
    .rho_search_space = FALSE,

    .fun_dist = function(xdt, distances, delta, omega, tolerance_equality) {
      p <- self$surrogate$predict(xdt)
      y_obs <- self$archive$data[[self$surrogate$cols_y]]
      distances_sq <- distances^2

      score <- acq_function_ideal_score(
        distances_sq = distances_sq,
        pred_mean = p$mean,
        y_obs = y_obs,
        delta = delta,
        tolerance_equality = tolerance_equality
      )

      if (omega > 0) {
        rho_values <- private$.rho_lookup(xdt)
        score <- (1 + omega * rho_values) * score
      }

      data.table(acq_dist_ideal = score)
    },

    .fit_pool = function(xdt, search_space) {
      private$.rho_dt <- NULL
      private$.rho_feature_ids <- NULL
      private$.rho_search_space <- FALSE

      if ((self$constants$values$omega %??% 0) <= 0) {
        return(invisible(NULL))
      }

      feature_ids <- search_space$ids()
      private$.rho_feature_ids <- feature_ids
      if (is.null(xdt)) {
        private$.rho_search_space <- TRUE
        return(invisible(NULL))
      }

      rho_dt <- unique(xdt[, feature_ids, with = FALSE], by = feature_ids)
      rho_dt[, .rho := private$.rho_values(rho_dt, search_space)]
      setkeyv(rho_dt, feature_ids)

      private$.rho_dt <- rho_dt

      invisible(NULL)
    },

    .rho_lookup = function(xdt) {
      if (private$.rho_search_space) {
        return(rep(1, nrow(xdt)))
      }
      if (is.null(private$.rho_dt)) {
        stop("IDEAL density values have not been initialized; call fit_pool() before evaluating with omega > 0")
      }
      assert_names(names(xdt), must.include = private$.rho_feature_ids)

      xdt <- xdt[, private$.rho_feature_ids, with = FALSE]
      rho_values <- private$.rho_dt[xdt, on = private$.rho_feature_ids, .rho, nomatch = 0L]

      if (length(rho_values) != nrow(xdt)) {
        stop("IDEAL density values are only defined for rows in the fitted pool")
      }

      rho_values
    },

    .rho_values = function(xdt, search_space) {
      n_pool <- nrow(xdt)
      if (n_pool <= 1L) {
        return(rep(1, n_pool))
      }

      feature_ids <- search_space$ids()
      dimension <- if (inherits(self$al_distance, "ALDistanceGeometry")) {
        self$al_distance$dimension
      } else {
        length(feature_ids)
      }
      dimension <- assert_count(dimension, positive = TRUE, tol = 0)
      n_neighbors <- min(dimension, n_pool - 1L)

      # Use a clone so pool-density references do not replace archive references.
      distance <- self$al_distance$clone(deep = TRUE)
      distance$set_reference_points(xdt)
      distances <- distance$distances(xdt)
      diag(distances) <- Inf

      nearest_mean <- apply(distances, 1L, function(row) {
        mean(sort(row, partial = seq_len(n_neighbors))[seq_len(n_neighbors)])
      })
      density_denominator <- nearest_mean^dimension
      zero_density_denominator <- density_denominator <= 0

      if (all(zero_density_denominator)) {
        return(rep(1, n_pool))
      }

      rho_values <- numeric(n_pool)
      rho_values[zero_density_denominator] <- 1
      positive <- !zero_density_denominator
      rho_values[positive] <- min(density_denominator[positive]) / density_denominator[positive]

      rho_values
    }
  )
)

acq_functions[["dist_ideal"]] <- AcqFunctionDistIDEAL

acq_function_ideal_score <- function(distances_sq, pred_mean, y_obs, delta, tolerance_equality) {
  residual_sq <- outer(pred_mean, y_obs, "-")^2
  score <- numeric(nrow(distances_sq))

  for (i in seq_len(nrow(distances_sq))) {
    exact <- which(distances_sq[i, ] <= tolerance_equality)
    if (length(exact)) {
      score[[i]] <- mean(residual_sq[i, exact])
      next
    }

    log_weights <- -distances_sq[i, ] - log(distances_sq[i, ])
    offset <- max(log_weights)

    if (!is.finite(offset)) {
      z <- 1
      s2 <- 0
    } else {
      log_weight_sum <- offset + log(sum(exp(log_weights - offset)))
      weights <- exp(log_weights - log_weight_sum)
      z <- (2 / pi) * atan(exp(-log_weight_sum))
      s2 <- sum(weights * residual_sq[i, ])
    }

    score[[i]] <- s2 + delta * z
  }

  score
}
