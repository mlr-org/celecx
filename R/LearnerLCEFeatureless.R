#' @title Featureless LCE Learner
#'
#' @name mlr_learners_lce.featureless
#'
#' @include LearnerLCE.R
#' @include utils_lce.R
#'
#' @description
#' Baseline [LearnerLCE] which ignores the `batch_nr` value and predicts a
#' constant performance for every future batch. The constant is one of three
#' summaries of the per-batch training performances, selected via `type`:
#'
#' * `"average"`: the mean (or median, when `robust = TRUE`) of the considered
#'   training batches. The "no learning-curve information used at all"
#'   sanity-check baseline.
#' * `"best"`: the best performance observed among the considered training
#'   batches. The "no progress beyond the best we have already seen" baseline.
#'   The optimization direction (whether higher or lower is better) is taken
#'   from the task's `measure`, so the task must carry one for this `type`.
#' * `"last"`: the performance of the most recent batch. The "no progress from
#'   here" baseline.
#'
#' The `window` parameter restricts "the considered batches" to the most recent
#' `window` of them (all of them by default). Location and dispersion are
#' computed on the task's [lce_link] scale. When `predict_type = "se"`,
#' `se_epistemic` is the (robust) link-scale dispersion of the considered
#' per-batch performances divided by the square root of their count (the standard
#' error of the constant), and the total predictive `se` adds that dispersion
#' back as the aleatoric spread (`se = sqrt(dispersion^2 + se_epistemic^2)`), both
#' replicated for every prediction. For `type = "average"` this is the textbook
#' predictive standard deviation of a new observation; for `type = "best"` /
#' `"last"` the constant is not the window mean, so the symmetric interval it
#' implies is only a coarse heuristic. The uncertainty reflects nothing beyond
#' the spread of the observed curve.
#'
#' @section Parameters:
#' * `type` :: `character(1)`\cr
#'   `"average"`, `"best"`, or `"last"`. Initialized to `"best"`.
#' * `robust` :: `logical(1)`\cr
#'   When `TRUE`, use median + MAD instead of mean + SD. Affects the `"average"`
#'   location and the dispersion of all types. Initialized to `FALSE`.
#' * `window` :: `integer(1)`\cr
#'   Number of most recent batches considered, clamped to the number of training
#'   batches. `Inf` (the default) uses all batches. For `type = "last"` only the
#'   `se` is affected, since the location is always the most recent batch.
#'
#' @export
LearnerLCEFeatureless <- R6Class("LearnerLCEFeatureless",
  inherit = LearnerLCE,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    initialize = function() {
      param_set <- ps(
        type = p_fct(c("average", "best", "last"), init = "best",
          tags = c("train", "required")),
        robust = p_lgl(init = FALSE, tags = c("train", "required")),
        window = p_int(lower = 1L, special_vals = list(Inf), init = Inf,
          tags = c("train", "required"))
      )

      super$initialize(
        id = "lce.featureless",
        param_set = param_set,
        predict_types = c("response", "se", "target_reached"),
        feature_types = "integer",
        properties = "featureless",
        label = "Featureless LCE",
        man = "celecx::mlr_learners_lce.featureless"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      link <- lce_link(task$link)
      minimize <- lce_model_minimize(task)
      pb <- lce_train_per_batch(task, link)
      n <- pb$n_batches
      idx <- seq.int(max(1L, n - pv$window + 1L), n)
      window_link <- link$transform(pb$value[idx])

      mu <- switch(pv$type,
        average = if (pv$robust) stats::median(window_link) else mean(window_link),
        last = window_link[length(window_link)],
        best = {
          if (is.na(minimize)) {
            stopf("'%s' with type = 'best' needs the task to carry a measure so the optimization direction is known", self$id)
          }
          if (minimize) min(window_link) else max(window_link)
        }
      )

      n_window <- length(window_link)
      dispersion <- if (n_window >= 2L) {
        if (pv$robust) stats::mad(window_link) else stats::sd(window_link)
      } else {
        0
      }
      se_epi <- if (n_window >= 2L) dispersion / sqrt(n_window) else 0
      # Total predictive SD of a new observation around the constant: the
      # aleatoric spread plus the standard error of the constant itself.
      se_total <- sqrt(dispersion^2 + se_epi^2)

      list(location = link$inverse(mu), mu = mu, dispersion = dispersion,
        se_total = se_total, se_epi = se_epi, n_batches = n, link = task$link,
        minimize = minimize)
    },

    .predict = function(task) {
      m <- self$model
      n <- task$nrow
      if (self$predict_type == "response") {
        return(list(response = rep(m$location, n)))
      }
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, rep(m$mu, n), rep(m$se_total, n),
        rep(m$se_epi, n), lce_link(m$link),
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)

#' @include aaa.R
learners[["lce.featureless"]] <- LearnerLCEFeatureless
