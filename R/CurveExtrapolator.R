#' @title Curve Extrapolator Base Class
#'
#' @description
#' Abstract base class for learning curve extrapolation models.
#'
#' @details
#' Subclasses implement `$train()` and `$predict()` to model progress over
#' evaluation budget (`x_col`) and return forecast distributions for future
#' evaluation counts.
#'
#' The expected prediction output is a `data.table` with columns:
#' - `n_evals`
#' - `mean`
#' - `q05`
#' - `q50`
#' - `q95`
#'
#' @export
CurveExtrapolator <- R6Class("CurveExtrapolator",
  inherit = ConfigurableComponent,

  public = list(

    #' @description
    #' Creates a new CurveExtrapolator.
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
    #'   Optional package names required by the extrapolator.
    #' @param param_set ([paradox::ParamSet])
    #'   Configuration parameter set.
    initialize = function(id = NULL,
        x_col = "n_evals",
        metric_col = "metric",
        direction = c("minimize", "maximize"),
        packages = character(0),
        param_set = ps()) {
      assert_string(x_col, min.chars = 1L)
      assert_string(metric_col, min.chars = 1L)
      assert_true(x_col != metric_col)
      direction <- match.arg(direction)
      packages <- assert_character(packages, unique = TRUE, any.missing = FALSE)
      if (length(packages) > 0L) {
        assert_character(packages, min.chars = 1L, any.missing = FALSE)
      }

      private$.x_col <- x_col
      private$.metric_col <- metric_col
      private$.direction <- direction
      private$.packages <- packages

      super$initialize(
        id = id,
        param_set = param_set,
        additional_configuration = character(0),
        additional_phash_input = c(".x_col", ".metric_col", ".direction", ".packages")
      )
    },

    #' @description
    #' Train the extrapolator on observed history.
    #'
    #' @param history (`data.table`)
    #'   History containing `x_col` and `metric_col`.
    #' @param ...
    #'   Additional arguments forwarded to subclass implementation.
    #'
    #' @return `self`.
    train = function(history, ...) {
      history <- private$.prepare_history(history)
      private$.check_packages()
      private$.model <- private$.train(history, ...)
      private$.history <- history
      private$.is_trained <- TRUE
      invisible(self)
    },

    #' @description
    #' Predict the metric distribution at future evaluation counts.
    #'
    #' @param n_evals_future (`integerish()` | `numeric()`)
    #'   Evaluation counts where predictions should be returned.
    #' @param ...
    #'   Additional arguments forwarded to subclass implementation.
    #'
    #' @return [data.table::data.table()].
    predict = function(n_evals_future, ...) {
      if (!private$.is_trained) {
        stopf("CurveExtrapolator is not trained")
      }
      assert_numeric(n_evals_future, lower = 1, any.missing = FALSE, min.len = 1L)

      n_evals_future <- unique(as.integer(round(n_evals_future)))
      n_evals_future <- sort(n_evals_future)

      private$.check_packages()
      prediction <- private$.predict(n_evals_future, ...)
      private$.assert_prediction(prediction, n_evals_future)
      prediction[order(get("n_evals"))]
    },

    #' @description
    #' Predict target-reaching quantities under the forecast distribution.
    #'
    #' @param target (`numeric(1)`)
    #'   Target metric value.
    #' @param n_evals_budget (`integer(1)`)
    #'   Budget horizon to evaluate probability of success.
    #' @param ...
    #'   Additional arguments forwarded to `$predict()`.
    #'
    #' @return Named `list()` with entries:
    #' - `p_reach_budget`
    #' - `n_evals_q50`
    #' - `n_evals_q90`
    predict_target = function(target, n_evals_budget, ...) {
      if (!private$.is_trained) {
        stopf("CurveExtrapolator is not trained")
      }
      assert_number(target)
      assert_int(n_evals_budget, lower = 1L)

      n_current <- private$.history[[private$.x_col]][[nrow(private$.history)]]
      horizon_grid <- seq.int(from = n_current, to = n_evals_budget)
      prediction <- self$predict(horizon_grid, ...)
      p_reach <- curve_probability_reach(
        mean = prediction$mean,
        q05 = prediction$q05,
        q95 = prediction$q95,
        target = target,
        direction = private$.direction
      )

      if (private$.direction == "minimize") {
        reached_observed <- private$.history[[private$.metric_col]] <= target
      } else {
        reached_observed <- private$.history[[private$.metric_col]] >= target
      }

      observed_n <- if (any(reached_observed)) {
        private$.history[[private$.x_col]][[which(reached_observed)[1L]]]
      } else {
        NA_integer_
      }

      q50_n <- horizon_grid[which(p_reach >= 0.5)[1L]] %??% NA_integer_
      q90_n <- horizon_grid[which(p_reach >= 0.9)[1L]] %??% NA_integer_

      list(
        p_reach_budget = unname(p_reach[[length(p_reach)]]),
        n_evals_q50 = if (is.na(observed_n)) q50_n else observed_n,
        n_evals_q90 = if (is.na(observed_n)) q90_n else observed_n
      )
    },

    #' @description
    #' Clear trained state.
    #'
    #' @return `self`.
    clear = function() {
      private$.model <- NULL
      private$.history <- NULL
      private$.is_trained <- FALSE
      invisible(self)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat(sprintf("<%s>\n", class(self)[[1L]]))
      cat(sprintf("* Trained: %s\n", private$.is_trained))
      cat(sprintf("* Direction: %s\n", private$.direction))
      cat(sprintf("* X column: %s\n", private$.x_col))
      cat(sprintf("* Metric column: %s\n", private$.metric_col))
      if (length(private$.packages) > 0L) {
        cat(sprintf("* Packages: %s\n", str_collapse(private$.packages)))
      }
      if (private$.is_trained) {
        cat(sprintf("* Rows trained: %i\n", nrow(private$.history)))
      }
      invisible(self)
    }
  ),

  active = list(

    #' @field x_col (`character(1)`)
    #'   Name of the evaluation-count column.
    x_col = function(rhs) {
      assert_ro_binding(rhs)
      private$.x_col
    },

    #' @field metric_col (`character(1)`)
    #'   Name of the metric column.
    metric_col = function(rhs) {
      assert_ro_binding(rhs)
      private$.metric_col
    },

    #' @field direction (`character(1)`)
    #'   Optimization direction.
    direction = function(rhs) {
      assert_ro_binding(rhs)
      private$.direction
    },

    #' @field packages (`character()`)
    #'   Required packages.
    packages = function(rhs) {
      assert_ro_binding(rhs)
      private$.packages
    },

    #' @field is_trained (`logical(1)`)
    #'   Whether the extrapolator was trained.
    is_trained = function(rhs) {
      assert_ro_binding(rhs)
      private$.is_trained
    },

    #' @field history (`data.table` | `NULL`)
    #'   Training history.
    history = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(private$.history)) return(NULL)
      copy(private$.history)
    },

    #' @field model (`any`)
    #'   Fitted model state returned by the subclass.
    model = function(rhs) {
      assert_ro_binding(rhs)
      private$.model
    }
  ),

  private = list(
    .x_col = NULL,
    .metric_col = NULL,
    .direction = NULL,
    .packages = character(0),
    .history = NULL,
    .model = NULL,
    .is_trained = FALSE,

    .train = function(history, ...) {
      stopf("Abstract method '.train' not implemented in class '%s'", class(self)[[1L]])
    },

    .predict = function(n_evals_future, ...) {
      stopf("Abstract method '.predict' not implemented in class '%s'", class(self)[[1L]])
    },

    .check_packages = function() {
      if (length(private$.packages) == 0L) {
        return(invisible(NULL))
      }
      require_namespaces(private$.packages)
      invisible(NULL)
    },

    .prepare_history = function(history) {
      assert_data_table(history, min.rows = 2L)
      assert_names(names(history), must.include = c(private$.x_col, private$.metric_col))

      history <- copy(history)
      history <- history[, c(private$.x_col, private$.metric_col), with = FALSE]
      setnames(history, c("x", "metric"))

      assert_numeric(history$x, lower = 1, any.missing = FALSE)
      assert_numeric(history$metric, any.missing = FALSE)

      setorderv(history, cols = "x")
      if (anyDuplicated(history$x)) {
        stopf("History column '%s' must be strictly increasing (no duplicate values)", private$.x_col)
      }
      if (nrow(history) > 1L && any(diff(history$x) <= 0)) {
        stopf("History column '%s' must be strictly increasing", private$.x_col)
      }

      setnames(history, c(private$.x_col, private$.metric_col))
      history
    },

    .assert_prediction = function(prediction, n_evals_future) {
      assert_data_table(prediction, min.rows = 1L)
      required_cols <- c("n_evals", "mean", "q05", "q50", "q95")
      assert_names(names(prediction), must.include = required_cols)

      assert_integerish(prediction$n_evals, tol = 0, lower = 1L, any.missing = FALSE)
      assert_numeric(prediction$mean, any.missing = FALSE)
      assert_numeric(prediction$q05, any.missing = FALSE)
      assert_numeric(prediction$q50, any.missing = FALSE)
      assert_numeric(prediction$q95, any.missing = FALSE)

      if (!setequal(prediction$n_evals, n_evals_future)) {
        stopf("Prediction must return exactly one row per requested n_evals")
      }
      if (any(prediction$q05 > prediction$q50) || any(prediction$q50 > prediction$q95)) {
        stopf("Prediction quantiles must satisfy q05 <= q50 <= q95")
      }
    }
  )
)

#' @title Probability to Reach Target from Forecast Quantiles
#'
#' @description
#' Internal helper that approximates success probabilities using a normal
#' distribution reconstructed from the 5% and 95% quantiles.
#'
#' @param mean (`numeric()`)
#' @param q05 (`numeric()`)
#' @param q95 (`numeric()`)
#' @param target (`numeric(1)`)
#' @param direction (`character(1)`)
#'
#' @return `numeric()`
#'
#' @keywords internal
curve_probability_reach <- function(mean, q05, q95, target, direction) {
  z95 <- stats::qnorm(0.95)
  sigma <- (q95 - q05) / (2 * z95)
  sigma <- pmax(sigma, .Machine$double.eps)

  if (direction == "minimize") {
    stats::pnorm(target, mean = mean, sd = sigma)
  } else {
    stats::pnorm(target, mean = mean, sd = sigma, lower.tail = FALSE)
  }
}
