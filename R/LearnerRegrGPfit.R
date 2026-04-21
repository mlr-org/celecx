#' @title GPfit Regression Learner
#'
#' @name mlr_learners_regr.gpfit
#'
#' @description
#' Gaussian process regression.
#' Calls [GPfit::GP_fit()] from package \CRANpkg{GPfit}.
#'
#' Predictions return the posterior mean and the square root of the predictive
#' mean squared error as standard error.
#'
#' * `scale` is initialized to `TRUE` and controls whether the learner min-max
#'   scales non-constant input features to the unit hypercube before calling
#'   [GPfit::GP_fit()]. When scaling is enabled, constant features are dropped.
#'   The fitted `GP` object stores the full per-feature scaling map in
#'   `feature_offset` and `feature_scaling`. Constant features are recorded
#'   there with offset `0` and scaling `0`.
#' * `control_search`, `control_best`, and `control_cluster` map to the three
#'   components of the `control` vector expected by [GPfit::GP_fit()]. Unset
#'   components fall back to the package defaults `200 * d`, `80 * d`, and
#'   `2 * d`, where `d` is the number of input columns.
#' * The correlation function is configured via the hyperparameters `corr_type`,
#'   `corr_power` (for `"exponential"`), and `corr_nu` (for `"matern"`), which
#'   are assembled into the `corr` list expected by [GPfit::GP_fit()].
#'
#' @export
LearnerRegrGPfit <- R6Class("LearnerRegrGPfit",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        scale = p_lgl(init = TRUE, tags = c("train", "required")),
        control_search = p_int(lower = 1L, tags = c("train", "control")),
        control_best = p_int(lower = 1L, tags = c("train", "control")),
        control_cluster = p_int(lower = 1L, tags = c("train", "control")),
        nug_thres = p_dbl(lower = 0, default = 20, tags = "train"),
        trace = p_lgl(default = FALSE, tags = "train"),
        maxit = p_int(lower = 1L, default = 100L, tags = "train"),
        corr_type = p_fct(
          levels = c("exponential", "matern"),
          default = "exponential",
          tags = "train"
        ),
        corr_power = p_dbl(
          lower = 1,
          upper = 2,
          default = 1.95,
          tags = "train",
          depends = quote(corr_type == "exponential")
        ),
        corr_nu = p_dbl(
          lower = 0,
          default = 2.5,
          tags = "train",
          depends = quote(corr_type == "matern")
        ),
        optim_start = p_uty(default = NULL, tags = "train"),
        M = p_int(lower = 1L, default = 1L, tags = "predict")
      )

      super$initialize(
        id = "regr.gpfit",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric"),
        packages = c("celecx", "GPfit"),
        label = "GPfit",
        man = "celecx::mlr_learners_regr.gpfit"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      control_ids <- self$param_set$ids(tags = "control")
      scale_input <- pv$scale
      train_args <- remove_named(
        pv,
        c("scale", control_ids, "corr_type", "corr_power", "corr_nu")
      )
      feature_matrix <- task_feature_matrix(task)
      feature_count <- ncol(feature_matrix)

      if (isTRUE(scale_input)) {
        feature_offset <- apply(feature_matrix, 2L, min)
        feature_scaling <- apply(feature_matrix, 2L, max) - feature_offset
        is_constant <- feature_scaling == 0

        feature_offset[is_constant] <- 0
        feature_scaling[is_constant] <- 0
      } else {
        feature_offset <- set_names(rep(0, feature_count), colnames(feature_matrix))
        feature_scaling <- set_names(rep(1, feature_count), colnames(feature_matrix))
      }

      keep_feature <- feature_scaling != 0

      if (!any(keep_feature)) {
        stopf("GPfit requires at least one non-constant feature.")
      }

      scaled_feature_matrix <- sweep(
        feature_matrix[, keep_feature, drop = FALSE],
        2L,
        feature_offset[keep_feature],
        "-"
      )
      scaled_feature_matrix <- sweep(
        scaled_feature_matrix,
        2L,
        feature_scaling[keep_feature],
        "/"
      )

      if (!is.null(train_args$optim_start)) {
        optim_start <- train_args$optim_start
        kept_feature_count <- sum(keep_feature)

        if (is.matrix(optim_start)) {
          if (ncol(optim_start) == feature_count) {
            optim_start <- optim_start[, keep_feature, drop = FALSE]
          } else if (ncol(optim_start) != kept_feature_count) {
            stopf(
              "optim_start must have %i or %i columns.",
              feature_count,
              kept_feature_count
            )
          }
        } else if (length(optim_start) %% feature_count == 0L) {
          optim_start <- matrix(optim_start, ncol = feature_count, byrow = TRUE)
          optim_start <- optim_start[, keep_feature, drop = FALSE]
        } else if (length(optim_start) %% kept_feature_count == 0L) {
          optim_start <- matrix(
            optim_start,
            ncol = kept_feature_count,
            byrow = TRUE
          )
        } else {
          stopf(
            "optim_start must have length divisible by %i or %i.",
            feature_count,
            kept_feature_count
          )
        }

        train_args$optim_start <- optim_start
      }

      control_values <- pv[intersect(names(pv), control_ids)]
      if (length(control_values)) {
        d <- ncol(scaled_feature_matrix)
        control <- c(
          control_search = 200L * d,
          control_best = 80L * d,
          control_cluster = 2L * d
        )
        control[names(control_values)] <- unlist(control_values, use.names = FALSE)

        if (control[["control_search"]] < control[["control_best"]]) {
          stopf("control_search must be greater than or equal to control_best.")
        }
        if (control[["control_best"]] < control[["control_cluster"]]) {
          stopf("control_best must be greater than or equal to control_cluster.")
        }

        train_args$control <- unname(control)
      }

      corr_args <- pv[c("corr_type", "corr_power", "corr_nu")]
      if (any(!vapply(corr_args, is.null, logical(1L)))) {
        corr_type <- corr_args$corr_type %??%
          self$param_set$default$corr_type
        corr <- list(type = corr_type)

        if (corr_type == "exponential") {
          corr$power <- corr_args$corr_power %??%
            self$param_set$default$corr_power
        } else {
          corr$nu <- corr_args$corr_nu %??%
            self$param_set$default$corr_nu
        }

        train_args$corr <- corr
      }

      model <- invoke(
        GPfit::GP_fit,
        X = scaled_feature_matrix,
        Y = task$truth(),
        .args = train_args
      )

      model$feature_offset <- feature_offset
      model$feature_scaling <- feature_scaling
      model
    },

    .predict = function(task) {
      model <- self$model
      feature_offset <- model$feature_offset
      feature_scaling <- model$feature_scaling
      keep_feature <- feature_scaling != 0
      feature_matrix <- as.matrix(task$data(cols = names(feature_offset)))
      feature_matrix <- sweep(
        feature_matrix[, keep_feature, drop = FALSE],
        2L,
        feature_offset[keep_feature],
        "-"
      )
      feature_matrix <- sweep(
        feature_matrix,
        2L,
        feature_scaling[keep_feature],
        "/"
      )

      model$feature_offset <- NULL
      model$feature_scaling <- NULL

      prediction <- invoke(
        stats::predict,
        model,
        xnew = feature_matrix,
        .args = self$param_set$get_values(tags = "predict")
      )

      list(
        response = prediction$Y_hat,
        se = prediction_se_from_variance(prediction$MSE)
      )
    }
  )
)

#' @include aaa.R
learners[["regr.gpfit"]] <- LearnerRegrGPfit
