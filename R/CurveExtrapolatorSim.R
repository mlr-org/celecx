#' @title Simulation-Based Curve Extrapolator
#'
#' @description
#' Learning curve extrapolator based on optimizer replay simulations.
#'
#' @details
#' This extrapolator continues an observed run by simulating future optimizer
#' steps on a proxy objective and aggregating multiple replay traces.
#'
#' `train()` requires replay context passed via `...`:
#' - `archive`: current [ArchiveBatch]
#' - either `optimizer_factory` (function returning an optimizer) or `optimizer`
#' - optional `objective_builder` (defaults to `curve_objective_builder_default()`)
#'
#' The default objective builder trains an [ObjectiveLearner] proxy on the
#' current archive.
#'
#' @export
CurveExtrapolatorSim <- R6Class("CurveExtrapolatorSim",
  inherit = CurveExtrapolator,

  public = list(

    #' @description
    #' Creates a new CurveExtrapolatorSim.
    #'
    #' @param id (`character(1)` | `NULL`)
    #'   Optional identifier.
    #' @param x_col (`character(1)`)
    #'   Name of the evaluation-count column.
    #' @param metric_col (`character(1)`)
    #'   Name of the metric column.
    #' @param direction (`character(1)`)
    #'   Either `"minimize"` or `"maximize"`.
    #' @param n_sim (`integer(1)`)
    #'   Number of replay simulations.
    #' @param seed (`integer(1)` | `NULL`)
    #'   Optional base seed for deterministic replay.
    initialize = function(id = NULL,
        x_col = "n_evals",
        metric_col = "metric",
        direction = c("minimize", "maximize"),
        n_sim = 50L,
        seed = NULL) {
      assert_int(n_sim, lower = 1L)
      assert_int(seed, lower = 1L, null.ok = TRUE)

      param_set <- ps(
        n_sim = p_int(lower = 1L, tags = "required"),
        seed = p_int(lower = 1L)
      )
      param_set$set_values(n_sim = n_sim, seed = seed)

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

  private = list(
    .train = function(history, ...) {
      args <- list(...)

      archive <- args$archive
      assert_r6(archive, "ArchiveBatch")

      optimizer_factory <- args$optimizer_factory
      optimizer <- args$optimizer
      if (is.null(optimizer_factory) && is.null(optimizer)) {
        stopf("CurveExtrapolatorSim$train() requires either 'optimizer_factory' or 'optimizer'")
      }
      if (!is.null(optimizer_factory) && !is.function(optimizer_factory)) {
        stopf("optimizer_factory must be a function")
      }
      if (is.null(optimizer_factory)) {
        assert_r6(optimizer, "Optimizer")
        optimizer_factory <- function(sim_id) optimizer$clone(deep = TRUE)
      }

      objective_builder <- args$objective_builder %??% curve_objective_builder_default
      assert_function(objective_builder)

      target_col <- args$target_col %??% archive$cols_y[[1L]]
      assert_string(target_col)
      assert_true(target_col %in% archive$cols_y)

      list(
        archive = archive$clone(deep = TRUE),
        optimizer_factory = optimizer_factory,
        objective_builder = objective_builder,
        target_col = target_col,
        metric_last = history[[self$metric_col]][[nrow(history)]],
        n_current = history[[self$x_col]][[nrow(history)]]
      )
    },

    .predict = function(n_evals_future, ...) {
      model <- self$model
      n_sim <- self$param_set$values$n_sim
      seed <- self$param_set$values$seed

      n_future_max <- max(n_evals_future)
      sim_matrix <- matrix(NA_real_, nrow = length(n_evals_future), ncol = n_sim)

      for (sim_id in seq_len(n_sim)) {
        if (!is.null(seed)) {
          set.seed(seed + sim_id - 1L)
        }

        sim_curve <- tryCatch(
          private$.run_single_replay(model, n_future_max, sim_id),
          error = function(e) NULL
        )

        if (is.null(sim_curve)) {
          next
        }

        sim_matrix[, sim_id] <- private$.curve_step(sim_curve$n_evals, sim_curve$metric, n_evals_future)
      }

      observed_x <- self$history[[self$x_col]]
      observed_metric <- self$history[[self$metric_col]]
      observed_idx <- n_evals_future <= observed_x[[length(observed_x)]]

      mean <- apply(sim_matrix, 1L, function(values) {
        if (all(is.na(values))) return(NA_real_)
        mean(values, na.rm = TRUE)
      })
      q05 <- apply(sim_matrix, 1L, function(values) {
        if (all(is.na(values))) return(NA_real_)
        as.numeric(quantile(values, probs = 0.05, na.rm = TRUE))
      })
      q50 <- apply(sim_matrix, 1L, function(values) {
        if (all(is.na(values))) return(NA_real_)
        as.numeric(quantile(values, probs = 0.50, na.rm = TRUE))
      })
      q95 <- apply(sim_matrix, 1L, function(values) {
        if (all(is.na(values))) return(NA_real_)
        as.numeric(quantile(values, probs = 0.95, na.rm = TRUE))
      })

      missing_rows <- is.na(mean)
      if (any(missing_rows)) {
        fallback <- private$.curve_step(observed_x, observed_metric, n_evals_future[missing_rows])
        mean[missing_rows] <- fallback
        q05[missing_rows] <- fallback
        q50[missing_rows] <- fallback
        q95[missing_rows] <- fallback
      }

      if (any(observed_idx)) {
        observed_values <- private$.curve_step(observed_x, observed_metric, n_evals_future[observed_idx])
        mean[observed_idx] <- observed_values
        q05[observed_idx] <- observed_values
        q50[observed_idx] <- observed_values
        q95[observed_idx] <- observed_values
      }

      data.table(
        n_evals = as.integer(n_evals_future),
        mean = as.numeric(mean),
        q05 = as.numeric(q05),
        q50 = as.numeric(q50),
        q95 = as.numeric(q95)
      )
    },

    .run_single_replay = function(model, n_future_max, sim_id) {
      if (n_future_max <= model$archive$n_evals) {
        sim_metric <- private$.archive_progress_curve(model$archive, model$target_col)
        return(sim_metric)
      }

      objective <- model$objective_builder(model$archive, sim_id = sim_id)
      assert_r6(objective, "Objective")

      sim_archive <- model$archive$clone(deep = TRUE)
      sim_terminator <- trm("evals", n_evals = n_future_max)
      sim_instance <- SearchInstance$new(
        objective = objective,
        terminator = sim_terminator,
        archive = sim_archive,
        search_space = sim_archive$search_space,
        check_values = TRUE
      )

      optimizer <- model$optimizer_factory(sim_id)
      if (!inherits(optimizer, "Optimizer")) {
        stopf("optimizer_factory must return an Optimizer")
      }

      tryCatch(
        optimizer$optimize(sim_instance),
        search_terminated_error = function(e) NULL,
        terminated_error = function(e) NULL
      )

      private$.archive_progress_curve(sim_instance$archive, model$target_col)
    },

    .archive_progress_curve = function(archive, target_col) {
      y <- archive$data[[target_col]]
      if (self$direction == "minimize") {
        metric <- cummin(y)
      } else {
        metric <- cummax(y)
      }
      data.table(
        n_evals = seq_along(metric),
        metric = metric
      )
    },

    .curve_step = function(x, y, x_new) {
      idx <- findInterval(x_new, x)
      idx[idx < 1L] <- 1L
      y[idx]
    }
  )
)


#' @title Default Objective Builder for Replay Simulation
#'
#' @description
#' Builds an [ObjectiveLearner] proxy objective from an archive.
#'
#' @param archive ([ArchiveBatch])
#'   Archive with existing evaluations.
#' @param learner ([mlr3::LearnerRegr])
#'   Learner used to fit the proxy objective.
#' @param target_col (`character(1)` | `NULL`)
#'   Target column to model. If `NULL`, uses first `archive$cols_y` entry.
#' @param sim_id (`integer(1)` | `NULL`)
#'   Optional simulation id.
#'
#' @return [ObjectiveLearner]
#'
#' @export
curve_objective_builder_default <- function(archive,
    learner = lrn("regr.featureless"),
    target_col = NULL,
    sim_id = NULL) {
  assert_r6(archive, "ArchiveBatch")
  assert_r6(learner, "LearnerRegr")
  assert_string(target_col, null.ok = TRUE)
  assert_int(sim_id, lower = 1L, null.ok = TRUE)

  target_col <- target_col %??% archive$cols_y[[1L]]
  assert_true(target_col %in% archive$cols_y)

  train_dt <- copy(archive$data[, c(archive$cols_x, target_col), with = FALSE])
  task <- as_task_regr(train_dt, target = target_col)

  learner_trained <- learner$clone(deep = TRUE)
  learner_trained$train(task)

  objective_id <- if (is.null(sim_id)) "sim_proxy" else sprintf("sim_proxy_%i", sim_id)

  ObjectiveLearner$new(
    learner = learner_trained,
    domain = archive$search_space$clone(deep = TRUE),
    codomain = archive$codomain$clone(deep = TRUE),
    id = objective_id,
    check_values = TRUE
  )
}
