#' @title GAM Curve Extrapolator
#'
#' @description
#' Learning curve extrapolator based on `mgcv::gam()`.
#'
#' @details
#' The model fits a smooth trend of metric over evaluation count and returns
#' normal-approximation prediction intervals using fitted standard errors.
#'
#' @export
CurveExtrapolatorGam <- R6Class("CurveExtrapolatorGam",
  inherit = CurveExtrapolator,

  public = list(

    #' @description
    #' Creates a new CurveExtrapolatorGam.
    #'
    #' @param id (`character(1)` | `NULL`)
    #'   Optional identifier.
    #' @param x_col (`character(1)`)
    #'   Name of the evaluation-count column.
    #' @param metric_col (`character(1)`)
    #'   Name of the metric column.
    #' @param direction (`character(1)`)
    #'   Either `"minimize"` or `"maximize"`.
    #' @param packages (`character()`)
    #'   Required package names. Defaults to `"mgcv"`.
    #' @param k (`integer(1)`)
    #'   Basis dimension for the smooth term.
    #' @param method (`character(1)`)
    #'   GAM fitting method.
    #' @param gamma (`numeric(1)`)
    #'   Smoothing parameter inflation.
    initialize = function(id = NULL,
        x_col = "n_evals",
        metric_col = "metric",
        direction = c("minimize", "maximize"),
        packages = "mgcv",
        k = 10L,
        method = c("REML", "ML", "GCV.Cp"),
        gamma = 1) {
      assert_int(k, lower = 3L)
      method <- match.arg(method)
      assert_number(gamma, lower = 0)

      param_set <- ps(
        k = p_int(lower = 3L, tags = "required"),
        method = p_fct(levels = c("REML", "ML", "GCV.Cp"), tags = "required"),
        gamma = p_dbl(lower = 0, tags = "required")
      )
      param_set$set_values(k = k, method = method, gamma = gamma)

      super$initialize(
        id = id,
        x_col = x_col,
        metric_col = metric_col,
        direction = direction,
        packages = packages,
        param_set = param_set
      )
    }
  ),

  private = list(
    .train = function(history, ...) {
      pv <- self$param_set$values

      formula <- stats::as.formula(sprintf(
        "%s ~ s(%s, k = %i)",
        self$metric_col,
        self$x_col,
        pv$k
      ))

      invoke(
        mgcv::gam,
        formula = formula,
        data = history,
        method = pv$method,
        gamma = pv$gamma
      )
    },

    .predict = function(n_evals_future, ...) {
      newdata <- data.table(n_evals = n_evals_future)
      setnames(newdata, "n_evals", self$x_col)

      prediction <- stats::predict(
        self$model,
        newdata = newdata,
        type = "response",
        se.fit = TRUE
      )

      z05 <- stats::qnorm(0.05)
      z95 <- stats::qnorm(0.95)

      mean <- as.numeric(prediction$fit)
      se <- as.numeric(prediction$se.fit)

      data.table(
        n_evals = as.integer(n_evals_future),
        mean = mean,
        q05 = mean + z05 * se,
        q50 = mean,
        q95 = mean + z95 * se
      )
    }
  )
)
