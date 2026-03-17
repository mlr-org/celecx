#' @title Learning Curve Extrapolation Learner via CurveExtrapolator
#'
#' @description
#' Wraps a [CurveExtrapolator] instance as an mlr3 [LearnerLCE], enabling
#' the standard `resample()` / `benchmark()` workflow for evaluating
#' learning curve extrapolation methods.
#'
#' @details
#' During training, the order column and target are extracted from the
#' [TaskLCE] and passed to the extrapolator's `$train()` method.
#' Other feature columns are ignored (`"featureless"` property).
#'
#' During prediction, the order column values are extracted and passed to
#' the extrapolator's `$predict()` method, returning point estimates
#' and/or quantiles.
#'
#' @section Predict Types:
#' * `"response"`: Returns the median forecast (`q50`).
#' * `"se"`: Returns `q50` as response and a standard error derived from
#'   the normal approximation to the 90% prediction interval.
#' * `"quantiles"`: Returns quantile forecasts at levels 0.05, 0.50, 0.95.
#'
#' @export
LearnerLCEExtrapolator = R6Class("LearnerLCEExtrapolator",
  inherit = LearnerLCE,
  public = list(

    #' @description
    #' Creates a new LearnerLCEExtrapolator.
    #'
    #' @param extrapolator ([CurveExtrapolator])\cr
    #'   The extrapolator to wrap.
    #' @param id (`character(1)` | `NULL`)\cr
    #'   Optional identifier. Defaults to a name derived from the
    #'   extrapolator class.
    initialize = function(extrapolator, id = NULL) {
      assert_r6(extrapolator, "CurveExtrapolator")
      private$.extrapolator = extrapolator$clone(deep = TRUE)

      if (is.null(id)) {
        cls = class(extrapolator)[[1L]]
        short = tolower(gsub("CurveExtrapolator", "", cls))
        if (nchar(short) == 0L) short = "base"
        id = sprintf("lce.extrapolator.%s", short)
      }

      private$.own_param_set = ps()

      super$initialize(
        id = id,
        param_set = ps(),  # replaced by active binding
        predict_types = c("response", "se", "quantiles"),
        feature_types = c("integer", "numeric"),
        properties = "featureless",
        packages = extrapolator$packages,
        label = sprintf("LCE Extrapolator (%s)", class(extrapolator)[[1L]]),
        man = "celecx::LearnerLCEExtrapolator"
      )
      private$.param_set = NULL
    }
  ),

  active = list(

    #' @field extrapolator ([CurveExtrapolator])
    #'   The wrapped extrapolator (read-only template).
    extrapolator = function(rhs) {
      assert_ro_binding(rhs)
      private$.extrapolator
    },

    #' @field param_set ([paradox::ParamSet])
    #'   Combined parameter set.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(list(
          private$.own_param_set,
          private$.extrapolator$param_set
        ))
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .extrapolator = NULL,
    .own_param_set = NULL,
    .param_set = NULL,

    .train = function(task) {
      order_col = private$.order_col(task)
      target_col = task$target_names

      # Extract ordered data
      dt = task$data(cols = c(order_col, target_col))
      setorderv(dt, order_col)

      # Build history with the extrapolator's expected column names
      extrap = private$.extrapolator$clone(deep = TRUE)
      history = data.table(
        x = dt[[order_col]],
        y = dt[[target_col]]
      )
      setnames(history, c(extrap$x_col, extrap$metric_col))

      extrap$train(history)
      extrap
    },

    .predict = function(task) {
      order_col = private$.order_col(task)
      n_evals = task$data(cols = order_col)[[1L]]

      prediction = self$model$predict(n_evals)

      predict_type = self$predict_type
      if (predict_type == "quantiles") {
        # Build quantile matrix: rows = observations, cols = quantile levels
        mat = cbind(prediction$q05, prediction$q50, prediction$q95)
        probs = c(0.05, 0.5, 0.95)
        attr(mat, "probs") = probs
        attr(mat, "response") = 0.5
        list(quantiles = mat)
      } else if (predict_type == "se") {
        z95 = stats::qnorm(0.95)
        se = (prediction$q95 - prediction$q05) / (2 * z95)
        list(response = prediction$q50, se = pmax(se, 0))
      } else {
        list(response = prediction$q50)
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        .extrapolator = {
          private$.param_set = NULL
          value$clone(deep = TRUE)
        },
        .own_param_set = {
          private$.param_set = NULL
          value$clone(deep = TRUE)
        },
        .param_set = NULL,
        value
      )
    }
  )
)
