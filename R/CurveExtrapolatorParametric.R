#' @title Parametric Curve Extrapolator
#'
#' @description
#' Learning curve extrapolator based on parametric transformed-linear models.
#'
#' @details
#' The extrapolator fits multiple curve families with linear models on transformed
#' evaluation counts and combines them via Akaike weights. Forecast intervals are
#' obtained from residual bootstrap refits.
#'
#' Supported families:
#' - `"inv_x"`: metric ~ 1 / x
#' - `"inv_sqrt_x"`: metric ~ 1 / sqrt(x)
#' - `"log_x"`: metric ~ log(x)
#'
#' @export
CurveExtrapolatorParametric <- R6Class("CurveExtrapolatorParametric",
  inherit = CurveExtrapolator,

  public = list(

    #' @description
    #' Creates a new CurveExtrapolatorParametric.
    #'
    #' @param id (`character(1)` | `NULL`)
    #'   Optional identifier.
    #' @param families (`character()`)
    #'   Parametric families to fit.
    #' @param x_col (`character(1)`)
    #'   Name of the evaluation-count column.
    #' @param metric_col (`character(1)`)
    #'   Name of the metric column.
    #' @param direction (`character(1)`)
    #'   Either `"minimize"` or `"maximize"`.
    #' @param n_bootstrap (`integer(1)`)
    #'   Number of residual bootstrap refits.
    initialize = function(id = NULL,
        families = c("inv_x", "inv_sqrt_x", "log_x"),
        x_col = "n_evals",
        metric_col = "metric",
        direction = c("minimize", "maximize"),
        n_bootstrap = 200L) {
      families <- assert_character(families, min.len = 1L, unique = TRUE, any.missing = FALSE)
      assert_subset(families, c("inv_x", "inv_sqrt_x", "log_x"))
      assert_int(n_bootstrap, lower = 20L)

      private$.families <- families

      param_set <- ps(
        n_bootstrap = p_int(lower = 20L, tags = "required")
      )
      param_set$set_values(n_bootstrap = n_bootstrap)

      super$initialize(
        id = id,
        x_col = x_col,
        metric_col = metric_col,
        direction = direction,
        packages = character(0),
        param_set = param_set
      )
    }
  ),

  active = list(

    #' @field families (`character()`)
    #'   Fitted candidate families.
    families = function(rhs) {
      assert_ro_binding(rhs)
      private$.families
    }
  ),

  private = list(
    .families = NULL,

    .train = function(history, ...) {
      x <- history[[self$x_col]]
      y <- history[[self$metric_col]]
      n_bootstrap <- self$param_set$values$n_bootstrap

      fits <- map(private$.families, function(family) {
        private$.fit_family(family, x, y, n_bootstrap)
      })
      fits <- discard(fits, is.null)

      if (length(fits) == 0L) {
        stopf("No parametric family could be fitted")
      }

      aic_values <- map_dbl(fits, "aic")
      keep <- is.finite(aic_values)
      if (any(keep)) {
        fits <- fits[keep]
        aic_values <- aic_values[keep]
        delta <- aic_values - min(aic_values)
        weights <- exp(-0.5 * delta)
      } else {
        weights <- rep(1, length(fits))
      }
      if (any(!is.finite(weights)) || sum(weights) <= 0) {
        weights <- rep(1, length(fits))
      }
      weights <- weights / sum(weights)

      names(weights) <- map_chr(fits, "family")
      list(
        fits = fits,
        weights = weights,
        n_bootstrap = n_bootstrap
      )
    },

    .predict = function(n_evals_future, ...) {
      fit_list <- self$model$fits
      weights <- self$model$weights
      n_bootstrap <- self$model$n_bootstrap

      mean_matrix <- map_dtc(fit_list, function(info) {
        private$.predict_family_mean(info, n_evals_future)
      })
      setnames(mean_matrix, map_chr(fit_list, "family"))

      mu <- rowSums(mean_matrix * rep(weights[colnames(mean_matrix)], each = nrow(mean_matrix)))

      draw_matrix <- matrix(NA_real_, nrow = length(n_evals_future), ncol = n_bootstrap)
      family_ids <- map_chr(fit_list, "family")

      for (b in seq_len(n_bootstrap)) {
        family_pick <- sample(family_ids, size = 1L, prob = weights)
        family_info <- fit_list[[which(family_ids == family_pick)[1L]]]
        draw_matrix[, b] <- private$.predict_family_bootstrap_draw(family_info, n_evals_future)
      }

      # fall back to mean-based intervals if all draws are missing
      if (all(is.na(draw_matrix))) {
        sigma <- apply(mean_matrix, 1L, sd)
        sigma <- pmax(sigma, .Machine$double.eps)
        z <- stats::qnorm(c(0.05, 0.95))
        q05 <- mu + z[[1L]] * sigma
        q95 <- mu + z[[2L]] * sigma
        q50 <- mu
      } else {
        q05 <- apply(draw_matrix, 1L, quantile, probs = 0.05, na.rm = TRUE)
        q50 <- apply(draw_matrix, 1L, quantile, probs = 0.50, na.rm = TRUE)
        q95 <- apply(draw_matrix, 1L, quantile, probs = 0.95, na.rm = TRUE)
      }

      data.table(
        n_evals = as.integer(n_evals_future),
        mean = as.numeric(mu),
        q05 = as.numeric(q05),
        q50 = as.numeric(q50),
        q95 = as.numeric(q95)
      )
    },

    .fit_family = function(family, x, y, n_bootstrap) {
      transform_fun <- private$.family_transform(family)
      z <- transform_fun(x)
      train_dt <- data.table(metric = y, z = z)

      fit <- tryCatch(
        stats::lm(metric ~ z, data = train_dt),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(NULL)
      }

      fitted <- as.numeric(stats::fitted(fit))
      residuals <- as.numeric(stats::residuals(fit))

      bootstrap_coef <- matrix(NA_real_, nrow = 2L, ncol = n_bootstrap)
      rownames(bootstrap_coef) <- c("(Intercept)", "z")

      for (b in seq_len(n_bootstrap)) {
        boot_y <- fitted + sample(residuals, size = length(residuals), replace = TRUE)
        boot_fit <- tryCatch(
          stats::lm(boot_y ~ z, data = train_dt),
          error = function(e) NULL
        )
        if (!is.null(boot_fit)) {
          coef_vec <- stats::coef(boot_fit)
          bootstrap_coef[, b] <- coef_vec[c("(Intercept)", "z")]
        }
      }

      list(
        family = family,
        fit = fit,
        transform_fun = transform_fun,
        aic = stats::AIC(fit),
        bootstrap_coef = bootstrap_coef
      )
    },

    .predict_family_mean = function(info, n_evals_future) {
      z_new <- info$transform_fun(n_evals_future)
      as.numeric(stats::predict(info$fit, newdata = data.table(z = z_new), type = "response"))
    },

    .predict_family_bootstrap_draw = function(info, n_evals_future) {
      z_new <- info$transform_fun(n_evals_future)
      coef_mat <- info$bootstrap_coef
      valid_cols <- which(colSums(is.na(coef_mat)) == 0L)
      if (length(valid_cols) == 0L) {
        return(private$.predict_family_mean(info, n_evals_future))
      }
      pick <- sample(valid_cols, size = 1L)
      intercept <- coef_mat["(Intercept)", pick]
      slope <- coef_mat["z", pick]
      intercept + slope * z_new
    },

    .family_transform = function(family) {
      switch(family,
        inv_x = function(x) 1 / x,
        inv_sqrt_x = function(x) 1 / sqrt(x),
        log_x = function(x) log(x),
        stopf("Unknown family '%s'", family)
      )
    }
  )
)
