#' @title Residual Bootstrap LCE Learner Wrapper
#'
#' @name mlr_learners_lce.bootstrap
#'
#' @include LearnerLCEWrapper.R
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
#' learner's parameters via [paradox::ParamSetCollection]; the base learner's
#' parameters carry the `base.` prefix (e.g. `base.rate_lower`).
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
  inherit = LearnerLCEWrapper,
  public = list(
    #' @description
    #' Creates a new bootstrap-wrapped LCE learner.
    #'
    #' @param learner ([LearnerLCE])\cr
    #'   Base LCE learner to wrap. Its predict type is forced to `"response"`
    #'   while the bootstrap replicates are fitted.
    initialize = function(learner) {
      super$initialize(
        learner = learner,
        id_prefix = "lce.bootstrap",
        param_set = ps(
          n_bootstrap = p_int(lower = 1L, init = 100L, tags = c("train", "required")),
          seed = p_int(special_vals = list(NULL), default = NULL, tags = c("train", "predict"))
        ),
        predict_types = c("response", "se", "quantiles", "samples", "target_reached"),
        label = "Bootstrap LCE",
        man = "celecx::mlr_learners_lce.bootstrap"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv <- private$.own_param_set$get_values(tags = "train")
      n_boot <- pv$n_bootstrap
      link <- lce_link(task$link)

      fitted_pred <- with_learner_state(private$.base_learner_obj,
        function(l) {
          l$train(task)
          l$predict(task)
        }, predict_type = "response")

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

      # Seeded so the residual resampling is reproducible; the predict-time
      # realised-sample noise uses seed + 1L so the two phases never share a
      # stream (see .realised_samples).
      bootstrap_states <- with_seed(pv$seed, lapply(seq_len(n_boot), function(i) {
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
        with_learner_state(private$.base_learner_obj,
          function(l) l$train(new_task)$state, predict_type = "response")
      }))

      list(
        bootstrap_states = bootstrap_states,
        n_bootstrap = n_boot,
        residuals = residuals_link,
        sigma2 = sigma2,
        link = task$link,
        minimize = lce_model_minimize(task),
        last_train_batch = max(as.numeric(task$batch_nrs))
      )
    },

    .predict = function(task) {
      # Each stored replicate state is injected into the shared base learner
      # and predicts on the *prediction* task as-is. Each replicate is a full
      # curve; columns of `mean_curves` are epistemic draws of the mean.
      m <- self$model
      link <- lce_link(m$link)
      preds <- map(m$bootstrap_states, function(state) {
        with_learner_state(private$.base_learner_obj,
          function(l) l$predict(task)$response,
          state = state, predict_type = "response")
      })
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
      pv <- private$.own_param_set$get_values(tags = "predict")
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
    # residual, back-transformed to the natural scale. Seeded with seed + 1L
    # for reproducibility on a stream distinct from the train-time residual
    # resampling (which is seeded with `seed`).
    .realised_samples = function(g_curves, link, seed) {
      resid <- self$model$residuals
      n <- nrow(g_curves)
      k <- ncol(g_curves)
      with_seed(if (is.null(seed)) NULL else seed + 1L, {
        noise <- if (length(resid) && any(resid != 0)) {
          matrix(sample(resid, n * k, replace = TRUE), nrow = n, ncol = k)
        } else {
          matrix(0, nrow = n, ncol = k)
        }
        link$inverse(g_curves + noise)
      })
    }
  )
)

#' @include aaa.R
learners[["lce.bootstrap"]] <- LearnerLCEBootstrap
