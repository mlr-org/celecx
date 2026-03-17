#' @title IDEAL Active Learning Optimizer
#'
#' @name mlr_optimizers_ideal
#'
#' @description
#' Active learning optimizer implementing the IDEAL algorithm
#' (Active Learning by Inverse Distance Weighting) from Alberto Bemporad,
#' "Active Learning for Regression by Inverse Distance Weighting". Uses model-
#' external uncertainty estimation via IDW-weighted residuals plus an
#' exploration term.
#'
#' Features are affine-scaled to \eqn{[-1, 1]} for the IDEAL geometry, while
#' the regression learner is trained and queried on the raw feature values.
#'
#' Pool mode is available for [ObjectiveDataset] objectives. Population mode is
#' available for bounded numeric objectives and optimizes the acquisition with
#' either an injected `continuous_optimizer` or a PSO-based default.
#'
#' The optimizer targets [SearchInstance] directly and is not routed through the
#' MBO acquisition function pipeline.
#'
#' @section Parameters:
#' * `delta` (`numeric(1)`)\cr
#'   Exploration-exploitation tradeoff. Default 1.
#'
#' * `n_init` (`integer(1)`)\cr
#'   Number of initial samples required. Default: dimension of search space.
#'
#' * `init_method` (`character(1)`)\cr
#'   Initialization strategy for pool mode: `"kmeans"` (default, per the IDEAL
#'   paper), `"gsx"` (centroid-nearest + farthest-first, per the GS paper), or
#'   `"random"`. Population mode always uses LHS regardless of this setting.
#'
#' * `batch_size` (`integer(1)`)\cr
#'   Points per iteration. Candidates within a batch are selected sequentially
#'   with the IDW geometry updated after each provisional pick, and the model
#'   is refit after the whole batch has been evaluated.
#'
#' @section Construction:
#' ```
#' OptimizerIDEAL$new(learner, continuous_optimizer = NULL)
#' ```
#'
#' * `learner` ([mlr3::LearnerRegr])\cr
#'   Regression learner used for predictions.
#'
#' * `continuous_optimizer` (`function` | `NULL`)\cr
#'   Optional acquisition optimizer used in population mode. It must have
#'   signature `function(search_space, score_function)` where `score_function`
#'   maps a numeric candidate vector to a scalar acquisition value. The return
#'   value must describe a single candidate as a named numeric vector, named
#'   list, or one-row `data.table`. If `NULL`, population mode uses
#'   `pso::psoptim()`.
#'
#' Or use the convenience constructor [optimizer_pool_al()] for pool mode.
#'
#' @section State Persistence:
#' After optimization, the attempt history is persisted in
#' `archive$data_extra[["OptimizerIDEAL"]]` for replay compatibility
#' with [CurveExtrapolatorSim].
#'
#' @export
OptimizerIDEAL <- R6Class("OptimizerIDEAL",
  inherit = OptimizerBatch,

  public = list(

    #' @field learner ([mlr3::LearnerRegr])\cr
    #'   Regression learner.
    learner = NULL,

    #' @field continuous_optimizer (`function` | `NULL`)\cr
    #'   Optional population-mode acquisition optimizer.
    continuous_optimizer = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr])\cr
    #'   Regression learner used for predictions.
    #' @param continuous_optimizer (`function` | `NULL`)\cr
    #'   Optional acquisition optimizer for population mode.
    initialize = function(learner, continuous_optimizer = NULL) {
      assert_r6(learner, "LearnerRegr")
      if (!is.null(continuous_optimizer)) {
        assert_function(continuous_optimizer)
      }
      self$learner <- learner
      self$continuous_optimizer <- continuous_optimizer

      param_set <- ps(
        delta = p_dbl(lower = 0),
        n_init = p_int(lower = 1L),
        init_method = p_fct(levels = c("kmeans", "gsx", "random")),
        batch_size = p_int(lower = 1L)
      )
      param_set$set_values(delta = 1, init_method = "kmeans", batch_size = 1L)

      super$initialize(
        id = "ideal",
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamInt"),
        properties = c("single-crit"),
        packages = character(),
        label = "IDEAL Active Learning Optimizer",
        man = "celecx::mlr_optimizers_ideal"
      )
    }
  ),

  private = list(

    .optimize = function(inst) {
      tryCatch(
        private$.run_ideal(inst),
        search_terminated_error = function(e) invisible(NULL)
      )
    },

    .run_ideal = function(inst) {
      pv <- self$param_set$get_values()
      objective <- inst$objective
      domain_ids <- inst$search_space$ids()
      target_id <- get_first_target_id(objective$codomain)

      if (inherits(objective, "ObjectiveDataset")) {
        return(private$.run_pool_ideal(inst, domain_ids, target_id, pv))
      }

      private$.assert_population_search_space(inst$search_space)
      private$.run_population_ideal(inst, domain_ids, target_id, pv)
    },

    .run_pool_ideal = function(inst, domain_ids, target_id, pv) {
      delta <- pv$delta
      batch_size <- pv$batch_size %??% 1L

      pool_prep <- prepare_pool_al_data(inst$objective$data, domain_ids)
      pool_dt <- pool_prep$pool_dt
      private$.assert_numeric_feature_columns(pool_dt, domain_ids)

      pool_x_raw <- as.matrix(pool_dt[, domain_ids, with = FALSE])
      scaler <- affine_scale_features(pool_x_raw)
      pool_x_scaled <- scaler$scaled

      N <- nrow(pool_x_raw)
      d <- ncol(pool_x_raw)
      n_init <- pv$n_init %??% d
      n_init <- min(n_init, N)
      n_init <- clamp_batch_size(n_init, N, remaining_search_capacity(inst))
      if (n_init < 1L) {
        return(invisible(NULL))
      }

      init_method <- pv$init_method %??% "kmeans"
      init_idx <- switch(init_method,
        kmeans = kmeans_init(pool_x_scaled, n_init),
        gsx = gsx_init(pool_x_scaled, n_init),
        random = sample.int(N, n_init, replace = FALSE)
      )
      xdt_init <- pool_dt[init_idx, domain_ids, with = FALSE]
      ydt_init <- inst$eval_batch(xdt_init)

      queried_mask <- rep(FALSE, N)
      queried_mask[init_idx] <- TRUE

      attempted_idx <- init_idx
      attempted_x_raw <- pool_x_raw[init_idx, , drop = FALSE]
      attempted_x_scaled <- pool_x_scaled[init_idx, , drop = FALSE]
      attempted_y <- ydt_init[[target_id]]
      Q <- seq_along(init_idx)
      target_scale <- private$.target_scale(attempted_y[Q])

      learner_clone <- self$learner$clone(deep = TRUE)
      private$.fit_learner(
        learner = learner_clone,
        train_x_raw = attempted_x_raw[Q, , drop = FALSE],
        train_y = attempted_y[Q],
        domain_ids = domain_ids,
        target_id = target_id
      )

      while (!inst$is_terminated && any(!queried_mask)) {
        current_batch_size <- clamp_batch_size(
          batch_size,
          sum(!queried_mask),
          remaining_search_capacity(inst)
        )
        if (current_batch_size < 1L) {
          break
        }

        batch_idx <- private$.select_batch_pool(
          pool_x_raw = pool_x_raw,
          pool_x_scaled = pool_x_scaled,
          queried_mask = queried_mask,
          learner = learner_clone,
          attempted_x_scaled = attempted_x_scaled,
          attempted_y = attempted_y,
          Q = Q,
          delta = delta,
          batch_size = current_batch_size,
          target_scale = target_scale,
          domain_ids = domain_ids
        )

        xdt <- pool_dt[batch_idx, domain_ids, with = FALSE]
        ydt <- inst$eval_batch(xdt)

        queried_mask[batch_idx] <- TRUE
        attempted_idx <- c(attempted_idx, batch_idx)
        attempted_x_raw <- rbind(
          attempted_x_raw,
          pool_x_raw[batch_idx, , drop = FALSE]
        )
        attempted_x_scaled <- rbind(
          attempted_x_scaled,
          pool_x_scaled[batch_idx, , drop = FALSE]
        )
        attempted_y <- c(attempted_y, ydt[[target_id]])
        Q <- seq_along(attempted_y)
        target_scale <- private$.target_scale(attempted_y[Q])

        private$.fit_learner(
          learner = learner_clone,
          train_x_raw = attempted_x_raw[Q, , drop = FALSE],
          train_y = attempted_y[Q],
          domain_ids = domain_ids,
          target_id = target_id
        )
      }

      private$.persist_state(inst, list(
        mode = "pool",
        attempted_idx = attempted_idx,
        attempted_x_raw = attempted_x_raw,
        attempted_y = attempted_y,
        Q = Q,
        scaler = scaler,
        target_scale = target_scale
      ))
    },

    .run_population_ideal = function(inst, domain_ids, target_id, pv) {
      delta <- pv$delta
      batch_size <- pv$batch_size %??% 1L
      remaining_capacity <- remaining_search_capacity(inst)
      if (!is.finite(remaining_capacity)) {
        stopf("Population-mode IDEAL requires a finite evaluation budget")
      }

      scaler <- private$.population_scaler(inst$search_space, domain_ids)
      d <- length(domain_ids)
      n_init <- pv$n_init %??% d
      n_init <- clamp_batch_size(n_init, Inf, remaining_capacity)
      if (n_init < 1L) {
        return(invisible(NULL))
      }

      xdt_init <- generate_design_lhs(inst$search_space, n = n_init)$data
      ydt_init <- inst$eval_batch(xdt_init)

      attempted_x_raw <- as.matrix(xdt_init[, domain_ids, with = FALSE])
      attempted_x_scaled <- affine_scale_apply(attempted_x_raw, scaler)
      attempted_y <- ydt_init[[target_id]]
      Q <- seq_along(attempted_y)
      target_scale <- private$.target_scale(attempted_y[Q])

      learner_clone <- self$learner$clone(deep = TRUE)
      private$.fit_learner(
        learner = learner_clone,
        train_x_raw = attempted_x_raw[Q, , drop = FALSE],
        train_y = attempted_y[Q],
        domain_ids = domain_ids,
        target_id = target_id
      )

      while (!inst$is_terminated) {
        current_batch_size <- clamp_batch_size(
          batch_size,
          Inf,
          remaining_search_capacity(inst)
        )
        if (current_batch_size < 1L) {
          break
        }

        xdt <- private$.select_batch_population(
          search_space = inst$search_space,
          scaler = scaler,
          learner = learner_clone,
          attempted_x_scaled = attempted_x_scaled,
          attempted_y = attempted_y,
          Q = Q,
          delta = delta,
          batch_size = current_batch_size,
          target_scale = target_scale,
          domain_ids = domain_ids
        )

        ydt <- inst$eval_batch(xdt)
        batch_x_raw <- as.matrix(xdt[, domain_ids, with = FALSE])
        attempted_x_raw <- rbind(attempted_x_raw, batch_x_raw)
        attempted_x_scaled <- rbind(
          attempted_x_scaled,
          affine_scale_apply(batch_x_raw, scaler)
        )
        attempted_y <- c(attempted_y, ydt[[target_id]])
        Q <- seq_along(attempted_y)
        target_scale <- private$.target_scale(attempted_y[Q])

        private$.fit_learner(
          learner = learner_clone,
          train_x_raw = attempted_x_raw[Q, , drop = FALSE],
          train_y = attempted_y[Q],
          domain_ids = domain_ids,
          target_id = target_id
        )
      }

      private$.persist_state(inst, list(
        mode = "population",
        attempted_x_raw = attempted_x_raw,
        attempted_y = attempted_y,
        Q = Q,
        scaler = scaler,
        target_scale = target_scale
      ))
    },

    .fit_learner = function(learner, train_x_raw, train_y, domain_ids, target_id) {
      train_dt <- as.data.table(train_x_raw)
      setnames(train_dt, domain_ids)
      train_dt[[target_id]] <- train_y
      task <- as_task_regr(train_dt, target = target_id)
      learner$train(task)
    },

    .select_batch_pool = function(pool_x_raw, pool_x_scaled, queried_mask, learner,
        attempted_x_scaled, attempted_y, Q, delta, batch_size, target_scale,
        domain_ids) {
      selected <- integer(batch_size)
      working_queried_mask <- queried_mask
      working_attempted_x_scaled <- attempted_x_scaled
      working_attempted_y <- attempted_y

      for (k in seq_len(batch_size)) {
        unqueried_idx <- which(!working_queried_mask)
        candidate_x_raw <- pool_x_raw[unqueried_idx, , drop = FALSE]
        candidate_yhat <- private$.predict_candidates(
          learner = learner,
          candidate_x_raw = candidate_x_raw,
          domain_ids = domain_ids
        )

        acq_values <- idw_acquisition(
          candidates_scaled = pool_x_scaled[unqueried_idx, , drop = FALSE],
          candidates_yhat = candidate_yhat,
          attempted_x_scaled = working_attempted_x_scaled,
          attempted_y = working_attempted_y,
          Q = Q,
          delta = delta,
          target_scale = target_scale
        )

        best_pool_idx <- unqueried_idx[[which.max(acq_values)]]
        selected[[k]] <- best_pool_idx
        working_queried_mask[[best_pool_idx]] <- TRUE
        working_attempted_x_scaled <- rbind(
          working_attempted_x_scaled,
          pool_x_scaled[best_pool_idx, , drop = FALSE]
        )
        working_attempted_y <- c(working_attempted_y, NA_real_)
      }

      selected
    },

    .select_batch_population = function(search_space, scaler, learner,
        attempted_x_scaled, attempted_y, Q, delta, batch_size, target_scale,
        domain_ids) {
      selected_x <- vector("list", batch_size)
      working_attempted_x_scaled <- attempted_x_scaled
      working_attempted_y <- attempted_y

      for (k in seq_len(batch_size)) {
        score_function <- function(x_raw) {
          private$.score_population_candidate(
            x_raw = x_raw,
            scaler = scaler,
            learner = learner,
            attempted_x_scaled = working_attempted_x_scaled,
            attempted_y = working_attempted_y,
            Q = Q,
            delta = delta,
            target_scale = target_scale,
            domain_ids = domain_ids
          )
        }

        candidate <- private$.optimize_population_candidate(
          search_space = search_space,
          score_function = score_function,
          domain_ids = domain_ids
        )

        selected_x[[k]] <- candidate
        candidate_scaled <- affine_scale_apply(
          matrix(candidate, nrow = 1L, dimnames = list(NULL, domain_ids)),
          scaler
        )
        working_attempted_x_scaled <- rbind(working_attempted_x_scaled, candidate_scaled)
        working_attempted_y <- c(working_attempted_y, NA_real_)
      }

      selected_dt <- rbindlist(
        lapply(selected_x, function(candidate) {
          as.data.table(as.list(candidate))
        }),
        fill = FALSE
      )
      setcolorder(selected_dt, domain_ids)
      selected_dt
    },

    .score_population_candidate = function(x_raw, scaler, learner,
        attempted_x_scaled, attempted_y, Q, delta, target_scale, domain_ids) {
      x_raw <- as.numeric(x_raw)
      x_scaled <- affine_scale_apply(
        matrix(x_raw, nrow = 1L, dimnames = list(NULL, domain_ids)),
        scaler
      )
      if (private$.has_duplicate_scaled(x_scaled, attempted_x_scaled)) {
        return(-Inf)
      }

      candidate_yhat <- private$.predict_candidates(
        learner = learner,
        candidate_x_raw = matrix(x_raw, nrow = 1L, dimnames = list(NULL, domain_ids)),
        domain_ids = domain_ids
      )

      idw_acquisition(
        candidates_scaled = x_scaled,
        candidates_yhat = candidate_yhat,
        attempted_x_scaled = attempted_x_scaled,
        attempted_y = attempted_y,
        Q = Q,
        delta = delta,
        target_scale = target_scale
      )[[1L]]
    },

    .predict_candidates = function(learner, candidate_x_raw, domain_ids) {
      candidate_dt <- as.data.table(candidate_x_raw)
      setnames(candidate_dt, domain_ids)
      learner$predict_newdata(candidate_dt)$response
    },

    .optimize_population_candidate = function(search_space, score_function, domain_ids) {
      optimizer <- self$continuous_optimizer %??% private$.default_continuous_optimizer
      candidate <- optimizer(search_space, score_function)
      private$.coerce_population_candidate(candidate, domain_ids)
    },

    .default_continuous_optimizer = function(search_space, score_function) {
      if (!requireNamespace("pso", quietly = TRUE)) {
        stopf(
          "Population-mode IDEAL requires package 'pso' unless continuous_optimizer is supplied"
        )
      }

      lower <- as.numeric(search_space$lower)
      upper <- as.numeric(search_space$upper)
      start <- (lower + upper) / 2

      result <- pso::psoptim(
        par = start,
        fn = function(x) {
          value <- score_function(x)
          if (!is.finite(value)) {
            return(.Machine$double.xmax / 1000)
          }
          -value
        },
        lower = lower,
        upper = upper,
        control = list(maxit = 100L, trace = 0L)
      )

      set_names(as.numeric(result$par), search_space$ids())
    },

    .coerce_population_candidate = function(candidate, domain_ids) {
      if (inherits(candidate, "data.table")) {
        if (nrow(candidate) != 1L) {
          stopf("continuous_optimizer must return exactly one candidate")
        }
        assert_subset(domain_ids, names(candidate))
        candidate <- as.numeric(candidate[1L, domain_ids, with = FALSE])
        names(candidate) <- domain_ids
      } else if (is.list(candidate)) {
        if (is.null(names(candidate))) {
          candidate <- unlist(candidate, use.names = FALSE)
          names(candidate) <- domain_ids
        } else {
          assert_subset(domain_ids, names(candidate))
          candidate <- unlist(candidate[domain_ids], use.names = TRUE)
        }
      } else {
        candidate <- as.numeric(candidate)
        if (is.null(names(candidate))) {
          names(candidate) <- domain_ids
        }
      }

      assert_numeric(candidate, any.missing = FALSE, len = length(domain_ids))
      candidate <- as.numeric(candidate)
      names(candidate) <- domain_ids
      candidate
    },

    .population_scaler = function(search_space, domain_ids) {
      affine_scaler_from_bounds(
        lower = search_space$lower[domain_ids],
        upper = search_space$upper[domain_ids]
      )
    },

    .assert_population_search_space = function(search_space) {
      if (!all(search_space$class == "ParamDbl")) {
        stopf("Population-mode IDEAL requires a bounded numeric search space")
      }

      lower <- as.numeric(search_space$lower)
      upper <- as.numeric(search_space$upper)
      if (!all(is.finite(lower)) || !all(is.finite(upper))) {
        stopf("Population-mode IDEAL requires finite lower and upper bounds")
      }
    },

    .assert_numeric_feature_columns = function(dt, domain_ids) {
      is_numeric <- vapply(dt[, domain_ids, with = FALSE], is.numeric, logical(1L))
      if (!all(is_numeric)) {
        stopf("IDEAL requires numeric feature columns")
      }
    },

    .target_scale = function(y) {
      y_range <- max(y) - min(y)
      if (!is.finite(y_range) || y_range < 1e-8) {
        return(1)
      }
      y_range
    },

    .has_duplicate_scaled = function(candidate_scaled, attempted_x_scaled, eps_eq = 1e-12) {
      any(rowSums(sweep(attempted_x_scaled, 2L, candidate_scaled[1L, ], "-") ^ 2) <= eps_eq)
    },

    .persist_state = function(inst, state) {
      inst$archive$data_extra[["OptimizerIDEAL"]] <- state
    },

    .assign_result = function(inst) {
      invisible(NULL)
    }
  )
)

#' @include aaa.R
optimizers[["ideal"]] <- OptimizerIDEAL
