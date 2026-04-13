#' @title hetGP Regression Learner
#'
#' @name mlr_learners_regr.hetgp
#'
#' @description
#' Heteroscedastic Gaussian process regression.
#' Calls [hetGP::mleHetGP()] from package \CRANpkg{hetGP}.
#'
#' Predictions return the posterior mean and the square root of the full
#' predictive variance (`sd2 + nugs`) as standard error.
#'
#' `noiseControl` and `settings` are represented by explicit learner
#' hyperparameters tagged `"noise_control"` and `"settings"`, respectively.
#' The wrapper reconstructs the nested lists expected by [hetGP::mleHetGP()]
#' internally.
#'
#' @section Initial Parameter Values:
#' * `return_matrices`: Set to `FALSE` (upstream default is `TRUE`) to avoid
#'   storing inverse covariance matrices in the fitted model. Prediction
#'   recomputes them on demand when needed.
#'
#' @export
LearnerRegrHetGP <- R6Class("LearnerRegrHetGP",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        lower = p_uty(default = NULL, tags = "train"),
        upper = p_uty(default = NULL, tags = "train"),
        link_thetas = p_fct(
          levels = c("joint", "constr", "independent"),
          default = "joint",
          tags = c("train", "settings")
        ),
        log_n = p_lgl(default = TRUE, tags = c("train", "settings")),
        init_strategy = p_fct(
          levels = c("simple", "residuals", "smoothed"),
          default = "residuals",
          tags = c("train", "settings")
        ),
        check_hom = p_lgl(default = TRUE, tags = c("train", "settings")),
        penalty = p_lgl(default = TRUE, tags = c("train", "settings")),
        trace = p_int(
          lower = -1L,
          upper = 3L,
          default = 0L,
          init = -1L,
          tags = c("train", "settings")
        ),
        return_matrices = p_lgl(
          default = TRUE,
          init = FALSE,
          tags = c("train", "settings")
        ),
        return_hom = p_lgl(default = FALSE, tags = c("train", "settings")),
        factr = p_dbl(lower = 0, default = 1e+09, tags = c("train", "settings")),
        pgtol = p_dbl(lower = 0, tags = c("train", "settings")),
        k_theta_g_bound_lower = p_dbl(
          lower = 0,
          default = 1,
          tags = c("train", "noise_control"),
          depends = quote(link_thetas == "joint")
        ),
        k_theta_g_bound_upper = p_dbl(
          lower = 0,
          default = 100,
          tags = c("train", "noise_control"),
          depends = quote(link_thetas == "joint")
        ),
        g_max = p_dbl(lower = 0, default = 100, tags = c("train", "noise_control")),
        g_min = p_dbl(lower = 0, tags = c("train", "noise_control")),
        g_bound_lower = p_dbl(
          lower = 0,
          default = 1e-06,
          tags = c("train", "noise_control")
        ),
        g_bound_upper = p_dbl(
          lower = 0,
          default = 1,
          tags = c("train", "noise_control")
        ),
        lower_delta = p_uty(default = NULL, tags = c("train", "noise_control")),
        upper_delta = p_uty(default = NULL, tags = c("train", "noise_control")),
        lower_theta_g = p_uty(
          default = NULL,
          tags = c("train", "noise_control"),
          depends = quote(link_thetas %in% c("constr", "independent"))
        ),
        upper_theta_g = p_uty(
          default = NULL,
          tags = c("train", "noise_control"),
          depends = quote(link_thetas %in% c("constr", "independent"))
        ),
        sink = p_lgl(default = FALSE, tags = c("train", "noise_control")),
        sink_eps = p_dbl(
          lower = 0,
          default = 1e-04,
          tags = c("train", "noise_control"),
          depends = quote(sink == TRUE)
        ),
        lower_p_x = p_uty(default = NULL, tags = c("train", "noise_control")),
        upper_p_x = p_uty(default = NULL, tags = c("train", "noise_control")),
        covtype = p_fct(
          levels = c("Gaussian", "Matern5_2", "Matern3_2"),
          default = "Gaussian",
          tags = "train"
        ),
        maxit = p_int(lower = 1L, default = 100L, tags = "train"),
        known = p_uty(default = NULL, tags = "train"),
        init = p_uty(default = NULL, tags = "train"),
        eps = p_dbl(
          lower = 0,
          default = sqrt(.Machine$double.eps),
          tags = "train"
        )
      )

      super$initialize(
        id = "regr.hetgp",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "hetGP"),
        label = "hetGP",
        man = "celecx::mlr_learners_regr.hetgp"
      )
    }
  ),
  private = list(
    .train = function(task) {
      train_values <- self$param_set$get_values(tags = "train")
      settings_ids <- self$param_set$ids(tags = "settings")
      noise_control_ids <- self$param_set$ids(tags = "noise_control")
      train_args <- remove_named(train_values, c(settings_ids, noise_control_ids))

      settings_values <- train_values[intersect(names(train_values), settings_ids)]
      if (length(settings_values)) {
        link_thetas <- settings_values$link_thetas
        if (!is.null(link_thetas)) {
          link_thetas <- switch(link_thetas,
            joint = "joint",
            constr = "constr",
            independent = FALSE
          )
        }

        train_args$settings <- compact(list(
          linkThetas = link_thetas,
          logN = settings_values$log_n,
          initStrategy = settings_values$init_strategy,
          checkHom = settings_values$check_hom,
          penalty = settings_values$penalty,
          trace = settings_values$trace,
          return.matrices = settings_values$return_matrices,
          return.hom = settings_values$return_hom,
          factr = settings_values$factr,
          pgtol = settings_values$pgtol
        ))
      }

      noise_control_values <- train_values[intersect(names(train_values), noise_control_ids)]
      if (length(noise_control_values)) {
        train_args$noiseControl <- compact(list(
          k_theta_g_bounds = c(
            noise_control_values$k_theta_g_bound_lower %??%
              self$param_set$default$k_theta_g_bound_lower,
            noise_control_values$k_theta_g_bound_upper %??%
              self$param_set$default$k_theta_g_bound_upper
          ),
          g_max = noise_control_values$g_max %??%
            self$param_set$default$g_max,
          g_min = noise_control_values$g_min,
          g_bounds = c(
            noise_control_values$g_bound_lower %??%
              self$param_set$default$g_bound_lower,
            noise_control_values$g_bound_upper %??%
              self$param_set$default$g_bound_upper
          ),
          lowerDelta = noise_control_values$lower_delta,
          upperDelta = noise_control_values$upper_delta,
          lowerTheta_g = noise_control_values$lower_theta_g,
          upperTheta_g = noise_control_values$upper_theta_g,
          SiNK = noise_control_values$sink,
          SiNK_eps = noise_control_values$sink_eps,
          lowerpX = noise_control_values$lower_p_x,
          upperpX = noise_control_values$upper_p_x
        ))
      }

      invoke(
        hetGP::mleHetGP,
        X = task_feature_matrix(task),
        Z = task$truth(),
        .args = train_args
      )
    },

    .predict = function(task) {
      prediction <- invoke(
        stats::predict,
        self$model,
        x = task_feature_matrix(task)
      )

      list(
        response = prediction$mean,
        se = prediction_se_from_variance(prediction$sd2 + prediction$nugs)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.hetgp"]] <- LearnerRegrHetGP
