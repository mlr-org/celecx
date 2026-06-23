#' @title Forward-Simulation LCE Learner
#'
#' @name mlr_learners_lce.simulate
#'
#' @include LearnerLCE.R
#' @include utils_objective.R
#'
#' @description
#' A [LearnerLCE] that predicts the future learning curve by *simulating the
#' active-learning loop forward* on a surrogate fit over the archive.
#'
#' At train time it fits an "oracle" regression learner on the archive
#' `(x -> y)` mapping; the oracle serves as a ground-truth proxy for the
#' objective. At predict time it re-runs the same [OptimizerAL] configuration
#' forward, starting from the training archive, using the oracle to generate
#' outcomes for newly proposed points, and scores the tracked surrogate after
#' each simulated batch on a held-out evaluation set whose targets are the
#' oracle's own predictions. The per-batch scores form the predicted curve.
#'
#' The optimizer configuration and the oracle learner are properties of the
#' *method* and are supplied at construction. The regression measure and (for
#' pool-based runs) the candidate pool are read from the [TaskLCE] (they are
#' data provenance), so the same learner can be benchmarked across tasks built
#' from different runs.
#'
#' @section Prediction:
#' The simulation produces a value *exactly* at each requested `batch_nr` (no
#' interpolation). Consecutive requested `batch_nr`s define the simulation's
#' per-step batch size: a request from `b_prev` to `b_next` is realized as one
#' optimizer proposal batch of size `b_next - b_prev`. When the requested
#' spacing varies, the optimizer's `batch_size` is adjusted and the optimizer
#' is run again. This matches the recorded curve when the originating run used
#' `batch_size = 1` (so each `batch_nr` step is one evaluation) or when the
#' requested spacing matches the originating batch sizes.
#'
#' @section Prediction types:
#' Each restart is an independent forward simulation, i.e. one joint sample path
#' of the *realised* trajectory over the requested batches. The `response` is the
#' across-restart mean on the [lce_link] scale, back-transformed (the predictive
#' median under the link). The total predictive `se` is the across-restart
#' standard deviation on the link scale (`0` with a single restart); the
#' epistemic `se_epistemic` is that divided by `sqrt(n_restarts)` (the standard
#' error of the expected curve). The `samples` predict type returns the restart
#' paths themselves (one column per restart), `quantiles` their per-row sample
#' quantiles, and `target_reached` the fraction of restart paths that have
#' reached the target. Richer-than-`response` predict types therefore need
#' `n_restarts > 1` (with a stochastic proposer).
#'
#' @section Parameters:
#' * `n_eval_points` :: `integer(1)`\cr
#'   Size of the fabricated held-out evaluation set (continuous runs).
#'   Initialized to `100`. Ignored for pool-based runs (the pool is used).
#' * `n_restarts` :: `integer(1)`\cr
#'   Number of independent forward simulations, which is also the number of
#'   predictive sample paths. Initialized to `1` (then `se` is `0`, since no
#'   spread is measured). Only meaningful with a stochastic proposer.
#' * `extrapolation` :: `character(1)`\cr
#'   How to extend beyond the simulated horizon (only reached via
#'   `max_batch_cap`): `"hold"` repeats the last value, `"linear"` continues the
#'   slope of the last two simulated points. Initialized to `"hold"`.
#' * `max_batch_cap` :: `integer(1)` | `NULL`\cr
#'   Maximum number of future points to simulate; further requested points use
#'   `extrapolation`. `NULL` (default) simulates every requested future point.
#' * `seed` :: `integer(1)` | `NULL`\cr
#'   RNG seed. Governs both the train-time evaluation-set sampling and the
#'   predict-time simulation, so a fixed seed makes the whole prediction
#'   reproducible. `NULL` (default) uses the current RNG state.
#'
#' @section Known limitations:
#' * The predicted quantity is the surrogate scored against the oracle's
#'   predictions, which can differ in level and plateau from the original
#'   surrogate-vs-test-set target (inherent to a self-contained forecast).
#' * Search-space transformations are ignored: the oracle is fit on, and
#'   proposals are made on, the untransformed scale.
#' * `se` reflects only proposer/sampler stochasticity across restarts; a
#'   deterministic configuration yields `se = 0`.
#'
#' @export
LearnerLCESimulate <- R6Class("LearnerLCESimulate",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new forward-simulation LCE learner.
    #'
    #' @param optimizer ([OptimizerAL])\cr
    #'   Active-learning optimizer configuration to run forward. Cloned on
    #'   construction.
    #' @param oracle_learner ([mlr3::LearnerRegr])\cr
    #'   Ground-truth proxy fit on the archive `(x -> y)` mapping. Cloned on
    #'   construction. There is deliberately no default: the choice of oracle
    #'   model is consequential and must be made explicitly.
    #' @param surrogate_id (`character(1)`)\cr
    #'   Id of the surrogate in `optimizer$surrogates` whose performance is
    #'   tracked. Defaults to `"uncertainty"` (the [optimizer_active_learning]
    #'   default).
    #' @param eval_sampler ([SpaceSampler])\cr
    #'   Sampler for the fabricated evaluation set on continuous search spaces.
    #'   Defaults to [SpaceSamplerSobol].
    initialize = function(optimizer, oracle_learner, surrogate_id = "uncertainty",
        eval_sampler = SpaceSamplerSobol$new()) {
      private$.optimizer <- assert_r6(optimizer, "OptimizerAL")$clone(deep = TRUE)
      private$.oracle_learner <- assert_r6(oracle_learner, "LearnerRegr")$clone(deep = TRUE)
      private$.surrogate_id <- assert_string(surrogate_id, min.chars = 1L)
      private$.eval_sampler <- assert_r6(eval_sampler, "SpaceSampler")$clone(deep = TRUE)

      param_set <- ps(
        n_eval_points = p_int(lower = 1L, init = 100L, tags = c("train", "required")),
        n_restarts = p_int(lower = 1L, init = 1L, tags = c("predict", "required")),
        extrapolation = p_fct(c("hold", "linear"), init = "hold",
          tags = c("predict", "required")),
        max_batch_cap = p_int(lower = 1L, special_vals = list(NULL), default = NULL,
          tags = "predict"),
        seed = p_int(special_vals = list(NULL), default = NULL,
          tags = c("train", "predict"))
      )

      super$initialize(
        id = "lce.simulate",
        param_set = param_set,
        predict_types = c("response", "se", "quantiles", "samples", "target_reached"),
        feature_types = "integer",
        properties = character(0),
        packages = union(c("celecx", private$.oracle_learner$packages),
          private$.optimizer$packages),
        label = "Forward-Simulation LCE",
        man = "celecx::mlr_learners_lce.simulate"
      )
    }
  ),

  active = list(
    #' @field optimizer ([OptimizerAL])\cr
    #' Active-learning optimizer configuration run forward by the learner.
    optimizer = function(rhs) {
      assert_ro_binding(rhs)
      private$.optimizer
    },

    #' @field oracle_learner ([mlr3::LearnerRegr])\cr
    #' Ground-truth proxy learner.
    oracle_learner = function(rhs) {
      assert_ro_binding(rhs)
      private$.oracle_learner
    },

    #' @field surrogate_id (`character(1)`)\cr
    #' Tracked surrogate id.
    surrogate_id = function(rhs) {
      assert_ro_binding(rhs)
      private$.surrogate_id
    },

    #' @field eval_sampler ([SpaceSampler])\cr
    #' Evaluation-set sampler for continuous runs.
    eval_sampler = function(rhs) {
      assert_ro_binding(rhs)
      private$.eval_sampler
    }
  ),

  private = list(
    .optimizer = NULL,
    .oracle_learner = NULL,
    .surrogate_id = NULL,
    .eval_sampler = NULL,

    .train = function(task) {
      if (length(task$archive_y) != 1L) {
        stopf("LearnerLCESimulate supports single-target archives only, got %s",
          str_collapse(task$archive_y, quote = "'"))
      }
      measure <- task$measure
      if (is.null(measure)) {
        stop(paste0("TaskLCE has no stored regression measure. Build it with ",
          "CallbackSurrogatePerformance / replay_surrogate_performance, or pass ",
          "`measure` to TaskLCE$new()."))
      }
      if (private$.surrogate_id %nin% names(private$.optimizer$surrogates)) {
        stopf("surrogate_id '%s' is not in the optimizer's surrogates: %s",
          private$.surrogate_id,
          str_collapse(names(private$.optimizer$surrogates), quote = "'"))
      }

      pv <- self$param_set$get_values(tags = "train")
      if (!is.null(pv$seed)) {
        # Seed the (otherwise RNG-dependent) evaluation-set sampling so the
        # fitted model -- and hence the prediction -- is reproducible.
        old_seed <- get_seed()
        on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
        set.seed(pv$seed)
      }

      # Archive prefix: one row per training evaluation.
      x_data <- task$archive_x_data()
      y_data <- task$archive_y_data()
      batch_nrs <- as.integer(round(task$batch_nrs))
      pool <- task$pool

      # Fit the oracle on (x -> y).
      oracle_dt <- cbind(copy(x_data), y_data)
      oracle_task <- TaskRegr$new(id = "lce_oracle", backend = oracle_dt,
        target = task$archive_y)
      oracle <- private$.oracle_learner$clone(deep = TRUE)
      tryCatch(oracle$train(oracle_task), error = function(e) {
        stopf("LearnerLCESimulate could not fit the oracle learner '%s': %s",
          private$.oracle_learner$id, conditionMessage(e))
      })

      # Normalized (trafo-free, finite-bound) oracle objective.
      built <- objective_learner_from_archive(oracle, task$search_space,
        task$codomain, x_data, pool = pool)

      # Held-out evaluation task: oracle truth on the evaluation points.
      eval_ids <- built$search_space$ids()
      eval_x <- if (!is.null(pool)) {
        as.data.table(pool)[, eval_ids, with = FALSE]
      } else {
        private$.eval_sampler$sample(n = pv$n_eval_points,
          search_space = built$search_space, known_pool = unique(x_data))
      }
      eval_dt <- copy(eval_x)
      set(eval_dt, j = task$archive_y,
        value = oracle$predict_newdata(eval_x)$response)
      eval_task <- TaskRegr$new(id = "lce_eval", backend = eval_dt,
        target = task$archive_y)

      list(
        objective = built$objective,
        search_space = built$search_space,
        codomain = task$codomain$clone(deep = TRUE),
        eval_task = eval_task,
        measure = measure$clone(deep = TRUE),
        surrogate_id = private$.surrogate_id,
        prefix_x = copy(x_data),
        prefix_y = copy(y_data),
        n_prefix_evals = nrow(x_data),
        last_prefix_batch = max(batch_nrs),
        pool = if (is.null(pool)) NULL else copy(as.data.table(pool)),
        recorded_curve = lce_train_per_batch(task, lce_link(task$link)),
        link = task$link,
        minimize = lce_model_minimize(task)
      )
    },

    .predict = function(task) {
      pv <- self$param_set$get_values(tags = "predict")
      m <- self$model
      n_restarts <- pv$n_restarts

      req_rows <- lce_predict_batches(task)
      req_unique <- sort(unique(req_rows))
      last_prefix <- m$last_prefix_batch

      future_points <- req_unique[req_unique > last_prefix]
      n_future <- length(future_points)
      n_sim <- if (is.null(pv$max_batch_cap)) n_future else min(n_future, pv$max_batch_cap)
      sim_points <- if (n_sim) future_points[seq_len(n_sim)] else numeric(0)
      extrap_points <- if (n_sim < n_future) future_points[(n_sim + 1L):n_future] else numeric(0)

      # Each restart is an independent forward simulation: one joint sample path
      # over the requested batches. Assemble a (unique batch) x (restart) matrix.
      sim_mat <- matrix(NA_real_, nrow = length(sim_points), ncol = n_restarts)
      if (length(sim_points)) {
        for (r in seq_len(n_restarts)) {
          seed_r <- if (is.null(pv$seed)) NULL else pv$seed + r - 1L
          sim_mat[, r] <- private$.simulate(sim_points, last_prefix, seed_r)
        }
      }

      # Extrapolate each restart's path beyond the simulated horizon.
      extrap_mat <- matrix(NA_real_, nrow = length(extrap_points), ncol = n_restarts)
      if (length(extrap_points)) {
        link <- lce_link(m$link)
        np <- length(sim_points)
        for (r in seq_len(n_restarts)) {
          col <- sim_mat[, r]
          if (identical(pv$extrapolation, "linear") && np >= 2L) {
            # Extrapolate the slope on the link scale, so the extension stays in
            # the metric's support (e.g. non-negative for a log link).
            g <- link$transform(col)
            slope <- (g[np] - g[np - 1L]) / (sim_points[np] - sim_points[np - 1L])
            extrap_mat[, r] <- link$inverse(g[np] + slope * (extrap_points - sim_points[np]))
          } else {
            extrap_mat[, r] <- col[length(col)]
          }
        }
      }

      paths_unique <- matrix(NA_real_, nrow = length(req_unique), ncol = n_restarts)
      in_idx <- which(req_unique <= last_prefix)
      if (length(in_idx)) {
        recorded <- m$recorded_curve
        nearest <- vapply(req_unique[in_idx],
          function(b) which.min(abs(recorded$batch - b)), integer(1))
        # in-sample carries no measured spread: identical across restarts.
        paths_unique[in_idx, ] <- recorded$value[nearest]
      }
      fut_idx <- which(req_unique > last_prefix)
      if (length(fut_idx)) {
        paths_unique[fut_idx, ] <- rbind(sim_mat, extrap_mat)
      }

      idx <- match(req_rows, req_unique)
      paths_rows <- paths_unique[idx, , drop = FALSE]

      link <- lce_link(m$link)
      g_paths <- link$transform(paths_rows)
      response <- link$inverse(rowMeans(g_paths))  # link-scale mean = median
      if (self$predict_type == "response") {
        return(list(response = response))
      }
      # Across-restart spread is the total predictive SD of a single realised
      # trajectory; the epistemic SD of the expected curve is its standard error.
      se_total <- lce_rowsd(g_paths)
      se_epi <- se_total / sqrt(n_restarts)
      lce_samples_predict(self$predict_type, paths_rows, response, se_total, se_epi,
        probs = pv$quantile_probs %??% lce_default_probs,
        reach_target = pv$reach_target, minimize = m$minimize)
    },

    # Run one forward simulation, scoring the surrogate exactly at `sim_points`.
    # Returns a numeric vector aligned with `sim_points`.
    .simulate = function(sim_points, last_prefix, seed = NULL) {
      if (!is.null(seed)) {
        old_seed <- get_seed()
        on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
        set.seed(seed)
      }
      m <- self$model

      objective <- m$objective$clone(deep = TRUE)
      if (!is.null(m$pool)) {
        objective <- ObjectivePoolWrapper$new(pool = m$pool, objective = objective)
      }

      # Seed the prefix archive (single batch; surrogate fits on all prefix rows).
      archive <- ArchiveBatch$new(search_space = m$search_space,
        codomain = m$codomain, check_values = FALSE)
      archive$add_evals(xdt = copy(m$prefix_x), xss_trafoed = NULL,
        ydt = copy(m$prefix_y))

      perf_cb <- CallbackSurrogatePerformance$new(
        surrogate_id = m$surrogate_id, task = m$eval_task,
        measures = list(perf = m$measure$clone(deep = TRUE)))
      terminator <- trm("evals", n_evals = m$n_prefix_evals)
      instance <- SearchInstance$new(
        objective = objective, search_space = m$search_space,
        terminator = terminator, archive = archive, check_values = FALSE,
        callbacks = list(perf_cb))

      optimizer <- private$.optimizer$clone(deep = TRUE)
      optimizer$param_set$set_values(n_init = 0L)

      values <- numeric(length(sim_points))
      prev <- last_prefix
      cum_evals <- m$n_prefix_evals
      last_good <- NA_real_
      for (i in seq_along(sim_points)) {
        step <- sim_points[i] - prev
        cum_evals <- cum_evals + step
        optimizer$param_set$set_values(batch_size = as.integer(step))
        terminator$param_set$set_values(n_evals = as.integer(cum_evals))
        optimizer$optimize(instance)

        new_rows <- perf_cb$data
        if (!nrow(new_rows)) {
          # Could not advance (e.g. pool exhausted); hold the last value.
          values[i:length(sim_points)] <- last_good
          break
        }
        last_good <- new_rows$perf[nrow(new_rows)]
        values[i] <- last_good
        prev <- sim_points[i]
      }
      values
    },

    deep_clone = function(name, value) {
      if (name == "state") {
        value <- super$deep_clone(name, value)
        if (!is.null(value$model)) {
          model <- value$model
          for (key in c("objective", "eval_task", "search_space", "codomain", "measure")) {
            if (!is.null(model[[key]])) model[[key]] <- model[[key]]$clone(deep = TRUE)
          }
          value$model <- model
        }
        return(value)
      }
      switch(name,
        .optimizer = value$clone(deep = TRUE),
        .oracle_learner = value$clone(deep = TRUE),
        .eval_sampler = value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)

#' @include aaa.R
learners[["lce.simulate"]] <- LearnerLCESimulate
