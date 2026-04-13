#' @title Deep GP Regression Learner
#'
#' @name mlr_learners_regr.deepgp
#'
#' @description
#' Two-layer deep Gaussian process regression.
#' Calls [deepgp::fit_two_layer()] from package \CRANpkg{deepgp}.
#'
#' Predictions return the posterior predictive mean and the square root of the
#' predictive variance as standard error. The learner always predicts with
#' `lite = TRUE` and `return_all = TRUE`, then recomputes the predictive
#' variance from the per-iteration outputs via the law of total variance. This
#' is more robust than the package's built-in variance summary for the exposed
#' `response`/`se` outputs.
#'
#' * If `D` is left unset, [deepgp::fit_two_layer()] defaults it to the number
#'   of input columns.
#' * The upstream `settings` list is represented by explicit learner
#'   hyperparameters tagged `"settings"`, and reconstructed internally before
#'   calling [deepgp::fit_two_layer()].
#' * `train_cores` maps to the `cores` argument of
#'   [deepgp::fit_two_layer()] when `vecchia = TRUE`; `predict_cores` maps to
#'   the `cores` argument of [stats::predict()].
#' * Only `mean_map` is exposed at predict time. Other upstream predict options
#'   that only generate discarded auxiliary outputs are fixed internally.
#'
#' @section Initial Parameter Values:
#' * `verb`: Set to `FALSE` (upstream default is `TRUE`) to suppress progress
#'   output.
#' * `train_cores`, `predict_cores`: Set to `1` to disable threading by
#'   default. They carry the `"threads"` tag so [mlr3::set_threads()] can
#'   update them uniformly.
#'
#' @export
LearnerRegrDeepGP <- R6Class("LearnerRegrDeepGP",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        dydx = p_uty(default = NULL, tags = "train"),
        nmcmc = p_int(lower = 1L, default = 10000L, tags = "train"),
        D = p_int(lower = 1L, tags = "train"),
        monowarp = p_uty(default = FALSE, tags = "train"),
        pmx = p_lgl(default = FALSE, tags = "train"),
        verb = p_lgl(default = TRUE, init = FALSE, tags = "train"),
        w_0 = p_uty(default = NULL, tags = "train"),
        theta_y_0 = p_dbl(lower = 0, default = 0.01, tags = "train"),
        theta_w_0 = p_uty(default = 0.1, tags = "train"),
        g_0 = p_dbl(lower = 0, default = 0.001, tags = "train"),
        true_g = p_dbl(lower = 0, tags = "train"),
        v = p_dbl(
          lower = 0,
          default = 2.5,
          tags = "train",
          depends = quote(cov == "matern")
        ),
        proposal_window_lower = p_dbl(
          lower = 0,
          default = 1,
          tags = c("train", "settings")
        ),
        proposal_window_upper = p_dbl(
          lower = 0,
          default = 2,
          tags = c("train", "settings")
        ),
        g_alpha = p_dbl(lower = 0, tags = c("train", "settings")),
        g_beta = p_dbl(lower = 0, tags = c("train", "settings")),
        theta_w_alpha = p_dbl(lower = 0, tags = c("train", "settings")),
        theta_w_beta = p_dbl(lower = 0, tags = c("train", "settings")),
        theta_y_alpha = p_dbl(lower = 0, tags = c("train", "settings")),
        theta_y_beta = p_dbl(lower = 0, tags = c("train", "settings")),
        tau2_w = p_dbl(lower = 0, default = 1, tags = c("train", "settings")),
        cov = p_fct(c("matern", "exp2"), default = "matern", tags = "train"),
        vecchia = p_lgl(default = FALSE, tags = "train"),
        m = p_int(lower = 1L, tags = "train", depends = quote(vecchia == TRUE)),
        ord = p_uty(default = NULL, tags = "train", depends = quote(vecchia == TRUE)),
        train_cores = p_int(lower = 1L, init = 1L, tags = c("train", "threads")),
        mean_map = p_lgl(default = TRUE, tags = "predict"),
        predict_cores = p_int(lower = 1L, init = 1L, tags = c("predict", "threads"))
      )

      super$initialize(
        id = "regr.deepgp",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "deepgp"),
        label = "deepgp Two-Layer DGP",
        man = "celecx::mlr_learners_regr.deepgp"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      settings_ids <- self$param_set$ids(tags = "settings")
      train_args <- remove_named(pv, c(settings_ids, "train_cores"))

      settings_values <- pv[intersect(names(pv), settings_ids)]
      if (length(settings_values)) {
        settings <- compact(list(
          l = settings_values$proposal_window_lower,
          u = settings_values$proposal_window_upper,
          tau2_w = settings_values$tau2_w
        ))

        if (!is.null(settings_values$g_alpha) || !is.null(settings_values$g_beta)) {
          settings$g <- compact(list(
            alpha = settings_values$g_alpha,
            beta = settings_values$g_beta
          ))
        }

        if (!is.null(settings_values$theta_w_alpha) || !is.null(settings_values$theta_w_beta)) {
          settings$theta_w <- compact(list(
            alpha = settings_values$theta_w_alpha,
            beta = settings_values$theta_w_beta
          ))
        }

        if (!is.null(settings_values$theta_y_alpha) || !is.null(settings_values$theta_y_beta)) {
          settings$theta_y <- compact(list(
            alpha = settings_values$theta_y_alpha,
            beta = settings_values$theta_y_beta
          ))
        }

        train_args$settings <- settings
      }

      if (isTRUE(pv$vecchia)) {
        train_args$cores <- pv$train_cores
      }

      invoke(
        deepgp::fit_two_layer,
        x = task_feature_matrix(task),
        y = task$truth(),
        .args = train_args
      )
    },

    .predict = function(task) {
      pv <- self$param_set$get_values(tags = "predict")
      predict_args <- remove_named(pv, "predict_cores")
      predict_args$cores <- pv$predict_cores
      predict_args$lite <- TRUE
      predict_args$return_all <- TRUE

      prediction <- invoke(
        stats::predict,
        self$model,
        x_new = task_feature_matrix(task),
        .args = predict_args
      )

      if (is.null(prediction$s2_all) || is.null(prediction$mean_all)) {
        stopf("deepgp prediction did not return per-iteration variances.")
      }

      s2_all <- as.matrix(prediction$s2_all)
      mean_all <- as.matrix(prediction$mean_all)
      variance <- apply(s2_all, 2L, function(x) {
        if (all(is.na(x))) {
          NA_real_
        } else {
          mean(x, na.rm = TRUE)
        }
      }) + apply(mean_all, 2L, function(x) {
        x <- x[is.finite(x)]
        if (length(x) <= 1L) {
          0
        } else {
          stats::var(x)
        }
      })

      list(
        response = prediction$mean,
        se = prediction_se_from_variance(variance)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.deepgp"]] <- LearnerRegrDeepGP
