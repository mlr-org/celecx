#' @title Greedy Sampling Optimizer
#'
#' @name mlr_optimizers_gs
#'
#' @description
#' Pool-based active learning optimizer implementing the greedy sampling family
#' (GSx, GSy, iGS, QBC, random) specified in Wu et al., "Active Learning for
#' Regression using Greedy Sampling". Uses pluggable scoring closures created by
#' the factory functions in [scoring_functions].
#'
#' Features are standardized to mean 0, sd 1 for distance computations.
#' The regression learner is trained and queried on raw (unscaled) features,
#' keeping the learner's input contract independent of the distance scaling.
#' By default, GSx / GSy / iGS initialize with [gsx_init()] while random / QBC
#' initialize by random sampling, matching the paper-family defaults. This can
#' be overridden via `init_method`.
#'
#' The optimizer targets [SearchInstance] with an [ObjectiveDataset] objective.
#' It is not routed through the MBO acquisition function pipeline.
#'
#' @section Parameters:
#' * `n_init` (`integer(1)`)\cr
#'   Number of initial samples via GSx before model-based scoring begins.
#'   Default: dimension of search space (post-preprocessing).
#'
#' * `batch_size` (`integer(1)`)\cr
#'   Points per iteration. Exact batching is implemented for `"random"` and
#'   `"gsx"`. Other greedy-sampling methods currently require `batch_size = 1`.
#'
#' * `init_method` (`character(1)`)\cr
#'   Initialization rule for the first `n_init` samples: `"gsx"` (centroid-
#'   nearest + farthest-first), `"random"`, or `"kmeans"` (K-means clustering).
#'   Defaults to the paper-style method family default.
#'
#' @section Construction:
#' ```
#' OptimizerGS$new(scoring, learner = NULL)
#' ```
#'
#' Or use the convenience constructor [optimizer_pool_al()].
#'
#' @export
OptimizerGS <- R6Class("OptimizerGS",
  inherit = OptimizerBatch,

  public = list(

    #' @field scoring (`function`)\cr
    #'   Scoring closure. See [scoring_functions].
    scoring = NULL,

    #' @field learner ([mlr3::LearnerRegr] | `NULL`)\cr
    #'   Regression learner for model-based methods. `NULL` for GSx/random.
    learner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param scoring (`function`)\cr
    #'   Scoring function closure from [scoring_functions].
    #' @param learner ([mlr3::LearnerRegr] | `NULL`)\cr
    #'   Optional regression learner for model-based methods (GSy, iGS, QBC).
    initialize = function(scoring, learner = NULL) {
      assert_function(scoring)
      if (!is.null(learner)) assert_r6(learner, "LearnerRegr")

      self$scoring <- scoring
      self$learner <- learner
      private$.method_id <- attr(scoring, "method_id") %??% "custom"
      private$.supports_batch <- isTRUE(attr(scoring, "supports_batch"))
      default_init_method <- if (private$.method_id %in% c("random", "qbc")) {
        "random"
      } else {
        "gsx"
      }

      param_set <- ps(
        n_init = p_int(lower = 1L),
        batch_size = p_int(lower = 1L),
        init_method = p_fct(levels = c("gsx", "random", "kmeans"))
      )
      param_set$set_values(batch_size = 1L, init_method = default_init_method)

      super$initialize(
        id = "gs",
        param_set = param_set,
        param_classes = c("ParamDbl", "ParamInt"),
        properties = c("single-crit"),
        packages = character(),
        label = "Greedy Sampling Optimizer",
        man = "celecx::mlr_optimizers_gs"
      )
    }
  ),

  private = list(

    .optimize = function(inst) {
      tryCatch(
        private$.run_pool_al(inst),
        search_terminated_error = function(e) invisible(NULL)
      )
    },

    .run_pool_al = function(inst) {
      pv <- self$param_set$get_values()
      batch_size <- pv$batch_size %??% 1L
      private$.assert_batch_supported(batch_size)

      objective <- inst$objective
      assert_r6(objective, "ObjectiveDataset")

      domain_ids <- inst$search_space$ids()
      target_id <- get_first_target_id(objective$codomain)

      pool_prep <- prepare_pool_al_data(objective$data, domain_ids)
      pool_dt <- pool_prep$pool_dt

      # Extract and standardize features
      pool_x_raw <- as.matrix(pool_dt[, domain_ids, with = FALSE])
      scaler <- standardize_features(pool_x_raw)
      pool_x <- scaler$scaled

      N <- nrow(pool_x)
      d <- ncol(pool_x)

      # Determine n_init (default: dimension of feature space)
      n_init <- pv$n_init %??% d
      n_init <- min(n_init, N)
      n_init <- clamp_batch_size(n_init, N, remaining_search_capacity(inst))
      if (n_init < 1L) {
        return(invisible(NULL))
      }

      init_method <- pv$init_method %??% if (private$.method_id %in% c("random", "qbc")) {
        "random"
      } else {
        "gsx"
      }
      init_idx <- switch(init_method,
        gsx = gsx_init(pool_x, n_init),
        random = sample.int(N, n_init, replace = FALSE),
        kmeans = kmeans_init(pool_x, n_init)
      )

      # Query initial points
      xdt_init <- pool_dt[init_idx, domain_ids, with = FALSE]
      inst$eval_batch(xdt_init)

      queried_idx <- init_idx
      queried_y <- pool_dt[[target_id]][init_idx]
      unqueried_mask <- rep(TRUE, N)
      unqueried_mask[init_idx] <- FALSE

      # Train model if model-based (on raw features)
      learner_clone <- NULL
      if (!is.null(self$learner)) {
        learner_clone <- self$learner$clone(deep = TRUE)
        private$.fit_learner(
          learner_clone, pool_x_raw, queried_idx, queried_y, target_id
        )
      }

      # Main loop: score, select, query, refit
      while (!inst$is_terminated && any(unqueried_mask)) {
        current_batch_size <- clamp_batch_size(
          batch_size,
          sum(unqueried_mask),
          remaining_search_capacity(inst)
        )
        if (current_batch_size < 1L) {
          break
        }

        batch_idx <- if (current_batch_size == 1L) {
          unqueried_idx <- which(unqueried_mask)

          scores <- self$scoring(
            pool_x, unqueried_idx, learner_clone, queried_idx, queried_y,
            target_id, pool_x_raw = pool_x_raw
          )

          unqueried_idx[which.max(scores)]
        } else {
          private$.select_batch(
            pool_x = pool_x,
            pool_x_raw = pool_x_raw,
            unqueried_mask = unqueried_mask,
            learner = learner_clone,
            queried_idx = queried_idx,
            queried_y = queried_y,
            target_id = target_id,
            batch_size = current_batch_size
          )
        }

        # Query
        xdt <- pool_dt[batch_idx, domain_ids, with = FALSE]
        inst$eval_batch(xdt)

        # Update state
        queried_idx <- c(queried_idx, batch_idx)
        queried_y <- c(queried_y, pool_dt[[target_id]][batch_idx])
        unqueried_mask[batch_idx] <- FALSE

        # Refit model (on raw features)
        if (!is.null(learner_clone)) {
          private$.fit_learner(
            learner_clone, pool_x_raw, queried_idx, queried_y, target_id
          )
        }
      }
    },

    .fit_learner = function(learner, pool_x_raw, queried_idx, queried_y,
        target_id) {
      train_dt <- as.data.table(pool_x_raw[queried_idx, , drop = FALSE])
      train_dt[[target_id]] <- queried_y
      task <- as_task_regr(train_dt, target = target_id)
      learner$train(task)
    },

    .select_batch = function(pool_x, pool_x_raw, unqueried_mask, learner,
        queried_idx, queried_y, target_id, batch_size) {
      selected <- integer(batch_size)
      working_unqueried_mask <- unqueried_mask
      working_queried_idx <- queried_idx

      for (k in seq_len(batch_size)) {
        candidate_idx <- which(working_unqueried_mask)
        scores <- self$scoring(
          pool_x,
          candidate_idx,
          learner,
          working_queried_idx,
          queried_y,
          target_id,
          pool_x_raw = pool_x_raw
        )

        best_pool_idx <- candidate_idx[[which.max(scores)]]
        selected[[k]] <- best_pool_idx
        working_unqueried_mask[[best_pool_idx]] <- FALSE

        if (private$.method_id == "gsx") {
          working_queried_idx <- c(working_queried_idx, best_pool_idx)
        }
      }

      selected
    },

    .assert_batch_supported = function(batch_size) {
      if (batch_size == 1L || private$.supports_batch) {
        return(invisible(NULL))
      }

      stopf("Batching for method '%s' not yet implemented", private$.method_id)
    },

    .assign_result = function(inst) {
      # No-op: SearchInstance does not support result assignment
      invisible(NULL)
    },

    .method_id = NULL,
    .supports_batch = FALSE
  )
)

#' @include aaa.R
optimizers[["gs"]] <- OptimizerGS
