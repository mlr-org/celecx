#' @title Residual Bootstrap LCE Learner Wrapper
#'
#' @name mlr_learners_lce.bootstrap
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Wraps an arbitrary base [LearnerLCE] and equips it with an SE prediction
#' via residual bootstrap on the per-batch fit.
#'
#' Training proceeds as:
#' 1. Fit the base learner once on the training task; collect the per-batch
#'    fitted values and the residuals on the [lce_link] scale
#'    (`residual = g(truth) - g(fitted)`).
#' 2. For each of `n_bootstrap` replicates, resample the link-scale residuals
#'    with replacement, add them to the fitted values, back-transform to the
#'    natural scale (so the synthetic targets respect the link's support), and
#'    refit the base learner on a clone of the training task.
#'
#' Each replicate is a full curve, so the replicates are draws of the *mean*
#' curve `f(b)`. The point forecast `response` is their link-scale mean,
#' back-transformed (the predictive median under the link). The epistemic
#' `se_epistemic` is the link-scale spread of the replicate mean curves; the
#' total predictive `se` adds the aleatoric residual variance back. The
#' `samples`, `quantiles`, and `target_reached` predict types describe the
#' *realised* future performance `y_b`: each draw is a replicate mean curve plus
#' a resampled link-scale residual, so they reflect both the bootstrap
#' mean-curve uncertainty and the residual noise.
#'
#' Residual bootstrap is the natural choice here because every batch
#' contributes a single per-batch performance value: row-bootstrap of the
#' archive would simply reweight identical rows, while batch-bootstrap with
#' multiplicity would not be picked up by base learners that aggregate by
#' batch before fitting.
#'
#' @section Parameters:
#' The bootstrap wrapper's own parameters are exposed alongside the base
#' learner's parameters via [paradox::ParamSetCollection].
#'
#' Own parameters:
#' * `n_bootstrap` :: `integer(1)`\cr
#'   Number of bootstrap replicates. Initialized to `100`.
#' * `seed` :: `integer(1)` | `NULL`\cr
#'   RNG seed for the residual resampling -- both the bootstrap refits at train
#'   time and the realised-trajectory draws at predict time, so a fixed seed
#'   makes the whole prediction reproducible. If `NULL` (default), the current
#'   RNG state is used.
#'
#' @export
LearnerLCEBootstrap <- R6Class("LearnerLCEBootstrap",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new bootstrap-wrapped LCE learner.
    #'
    #' @param learner ([LearnerLCE])\cr
    #'   Base LCE learner to wrap. Its predict type is forced to `"response"`
    #'   while the bootstrap replicates are fitted.
    initialize = function(learner) {
      assert_r6(learner, "LearnerLCE")
      private$.base_learner_obj <- learner$clone(deep = TRUE)
      private$.own_param_set <- c(
        ps(
          n_bootstrap = p_int(lower = 1L, init = 100L, tags = c("train", "required")),
          seed = p_int(special_vals = list(NULL), default = NULL, tags = c("train", "predict"))
        ),
        lce_predict_type_params(c("quantiles", "target_reached"))
      )

      super$initialize(
        id = sprintf("lce.bootstrap.%s", learner$id),
        param_set = ps(),
        predict_types = c("response", "se", "quantiles", "samples", "target_reached"),
        feature_types = learner$feature_types,
        properties = intersect(learner$properties,
          mlr_reflections$learner_properties$lce),
        packages = union("celecx", learner$packages),
        label = "Bootstrap LCE",
        man = "celecx::mlr_learners_lce.bootstrap"
      )

      # Replace the placeholder ParamSet with a lazy collection that merges
      # own + base learner params.
      private$.param_set <- NULL
    }
  ),

  active = list(
    #' @field wrapped ([LearnerLCE])\cr
    #' Read-only access to the wrapped base learner.
    wrapped = function(val) {
      if (!missing(val) && !identical(val, private$.base_learner_obj)) {
        stop("$wrapped is read-only.")
      }
      private$.base_learner_obj
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #' Combined parameter set of the wrapper and the base learner.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set <- ParamSetCollection$new(list(
          private$.own_param_set,
          base = private$.base_learner_obj$param_set
        ))
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .base_learner_obj = NULL,
    .own_param_set = NULL,

    .train = function(task) {
      pv <- private$.own_param_set$get_values(tags = "train")
      n_boot <- pv$n_bootstrap
      link <- lce_link(task$link)

      base_template <- private$.base_learner_obj$clone(deep = TRUE)
      base_template$predict_type <- "response"
      base_template$train(task)
      fitted_pred <- base_template$predict(task)

      # Per-batch fitted values and residuals on the link scale (the scale the
      # base learner models on), so synthetic targets respect the link's support
      # and the aleatoric variance matches the modelling assumption.
      batch_col <- task$col_roles$feature
      row_batches <- task$data(cols = batch_col)[[1L]]
      fitted_dt <- data.table(
        batch = row_batches,
        truth = task$truth(),
        fitted = fitted_pred$response
      )
      per_batch <- fitted_dt[,
        list(truth = mean(truth), fitted = mean(fitted)),
        by = "batch"]
      setorderv(per_batch, "batch")
      fitted_link <- link$transform(per_batch$fitted)
      residuals_link <- link$transform(per_batch$truth) - fitted_link
      sigma2 <- if (length(residuals_link) >= 2L) stats::var(residuals_link) else 0

      orig_dt <- task$data(cols = c(
        task$target_names, batch_col,
        task$col_roles$archive_x, task$col_roles$archive_y
      ))
      target_col <- task$target_names

      seed <- pv$seed
      if (!is.null(seed)) {
        old_seed <- get_seed()
        on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
        set.seed(seed)
      }

      bootstrap_learners <- lapply(seq_len(n_boot), function(i) {
        # Resample link-scale residuals and back-transform, so synthetic targets
        # stay inside the link's support.
        boot_per_batch <- link$inverse(fitted_link + sample(residuals_link, replace = TRUE))
        # Map per-batch synthetic targets back to per-row.
        row_new_target <- boot_per_batch[match(orig_dt[[batch_col]], per_batch$batch)]
        boot_dt <- copy(orig_dt)
        set(boot_dt, j = target_col, value = row_new_target)
        new_task <- TaskLCE$new(
          id = sprintf("%s_boot%i", task$id, i),
          backend = boot_dt,
          target = target_col,
          batch_nr = batch_col,
          archive_x = task$col_roles$archive_x,
          archive_y = task$col_roles$archive_y,
          search_space = task$search_space,
          codomain = task$codomain,
          measure = task$measure,
          pool = task$pool,
          link = task$link
        )
        learner_i <- private$.base_learner_obj$clone(deep = TRUE)
        learner_i$predict_type <- "response"
        learner_i$train(new_task)
        learner_i
      })

      list(
        base_template = base_template,
        bootstrap_learners = bootstrap_learners,
        n_bootstrap = n_boot,
        residuals = residuals_link,
        sigma2 = sigma2,
        link = task$link,
        minimize = lce_model_minimize(task)
      )
    },

    .predict = function(task) {
      # The base learners take the *prediction* task as-is. Each replicate is a
      # full curve; columns of `mean_curves` are epistemic draws of the mean.
      m <- self$model
      link <- lce_link(m$link)
      preds <- map(m$bootstrap_learners, function(l) l$predict(task)$response)
      mean_curves <- do.call(cbind, preds)
      g_curves <- link$transform(mean_curves)
      response <- link$inverse(rowMeans(g_curves))  # mean on the link scale = median
      if (self$predict_type == "response") {
        return(list(response = response))
      }
      # Epistemic SD = spread of the replicate mean curves (link scale); the
      # total predictive SD adds the aleatoric residual variance.
      se_epi <- lce_rowsd(g_curves)
      se_total <- sqrt(se_epi^2 + m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      if (self$predict_type == "se") {
        return(list(response = response, se = se_total, se_epistemic = se_epi))
      }
      # Draws of the realised future y_b: each replicate mean curve plus a
      # resampled link-scale residual.
      samples <- private$.realised_samples(g_curves, link, pv$seed)
      lce_samples_predict(self$predict_type, samples, response, se_total, se_epi,
        probs = pv$quantile_probs %??% lce_default_probs,
        reach_target = pv$reach_target, minimize = m$minimize)
    },

    # Realised-trajectory draws: replicate mean curve + resampled link-scale
    # residual, back-transformed to the natural scale. Seeded for reproducibility.
    .realised_samples = function(g_curves, link, seed) {
      resid <- self$model$residuals
      if (!is.null(seed)) {
        old_seed <- get_seed()
        on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
        set.seed(seed)
      }
      n <- nrow(g_curves)
      k <- ncol(g_curves)
      noise <- if (length(resid) && any(resid != 0)) {
        matrix(sample(resid, n * k, replace = TRUE), nrow = n, ncol = k)
      } else {
        matrix(0, nrow = n, ncol = k)
      }
      link$inverse(g_curves + noise)
    },

    deep_clone = function(name, value) {
      switch(name,
        .base_learner_obj = value$clone(deep = TRUE),
        .own_param_set = value$clone(deep = TRUE),
        .param_set = {
          private$.param_set <- NULL
          NULL
        },
        state = {
          if (!is.null(value$bootstrap_learners)) {
            value$bootstrap_learners <- lapply(value$bootstrap_learners,
              function(l) l$clone(deep = TRUE))
          }
          if (!is.null(value$base_template)) {
            value$base_template <- value$base_template$clone(deep = TRUE)
          }
          value
        },
        value
      )
    }
  )
)

#' @include aaa.R
learners[["lce.bootstrap"]] <- LearnerLCEBootstrap
