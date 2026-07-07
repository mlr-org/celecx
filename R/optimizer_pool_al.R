#' @title Convenience Constructor for Pool-Based Active Learning Optimizers
#'
#' @description
#' Creates an [OptimizerAL] with appropriate components for a specific active
#' learning method.
#'
#' @param method (`character(1)`)\cr
#'   One of `"gsx"`, `"gsy"`, `"igs"`, `"qbc"`, `"random"`, `"ideal"`.
#' @param learner ([mlr3::LearnerRegr] | `NULL`)\cr
#'   Regression learner. Required for `"gsy"`, `"igs"`, `"qbc"`, `"ideal"`.
#' @param delta (`numeric(1)`)\cr
#'   Exploration weight for IDEAL (default 1).
#' @param n_init (`integer(1)` | `NULL`)\cr
#'   Number of initial samples. `NULL` uses [OptimizerAL]'s default
#'   initialization policy.
#' @param init_method (`character(1)` | `NULL`)\cr
#'   Optional initialization override: `"gsx"`, `"random"`, or `"kmeans"`.
#'   `NULL` keeps the method-specific default (`"kmeans"` for IDEAL,
#'   `"gsx"` for GSx/GSy/iGS, `"random"` for random/QBC).
#' @param k_qbc (`integer(1)`)\cr
#'   Number of QBC committee members (default 5).
#' @param batch_size (`integer(1)`)\cr
#'   Points per iteration (default 1).
#' @param n_candidates (`NULL` | `integer(1)`)\cr
#'   Optional number of candidate points to subsample uniformly before scoring.
#'   `NULL` keeps exhaustive pool scoring and does not enable continuous-space
#'   use.
#' @param distance (`character(1)` | [ALDistance] | `NULL`)\cr
#'   Distance used by every distance-based component (the GSx / iGS / IDEAL
#'   acquisition functions and the `"gsx"` / `"kmeans"` initializations).
#'   `NULL` (default) keeps the papers' method-specific scalings
#'   (standardization; per-dimension affine for IDEAL), which support numeric
#'   search spaces only. For mixed-type pools pass `"gower"` (a
#'   [mlr_al_distances] key) or an [ALDistance] object. A `"kmeans"`
#'   initialization combined with a non-geometry distance (such as Gower) uses
#'   the medoid-based [SpaceSamplerKMedoids] instead, its mixed-type analogue.
#'   Note that on mixed-type pools the surrogate `learner` must support
#'   `character` features (e.g. `lrn("regr.ranger")`), since archives store
#'   [paradox::ParamSet] factor parameters as character columns.
#'
#' @return A configured [OptimizerAL].
#'
#' @export
optimizer_pool_al <- function(
    method = c("gsx", "gsy", "igs", "qbc", "random", "ideal"),
    learner = NULL, delta = 1, n_init = NULL, init_method = NULL,
    k_qbc = 5L, batch_size = 1L, n_candidates = NULL, distance = NULL) {

  method <- match.arg(method)
  assert_choice(init_method, c("gsx", "random", "kmeans"), null.ok = TRUE)
  assert_number(delta, lower = 0)
  assert_int(k_qbc, lower = 2L)
  assert_int(batch_size, lower = 1L)
  assert_int(n_candidates, lower = 1L, null.ok = TRUE)
  if (!is.null(distance) && !inherits(distance, "ALDistance")) {
    assert_choice(distance, mlr_al_distances$keys())
  }

  if (!is.null(n_candidates) && n_candidates < batch_size && method != "random") {
    stopf("n_candidates (%i) must be >= batch_size (%i)", n_candidates, batch_size)
  }

  if (!is.null(n_init)) {
    assert_int(n_init, lower = 1L)
  }

  if (method %in% c("gsy", "igs", "qbc", "ideal")) {
    assert_r6(learner, "LearnerRegr")
  }

  init_sampler <- optimizer_pool_al_init_sampler(method, init_method, distance)

  if (method == "random") {
    optimizer <- OptimizerAL$new(
      proposer = ALProposerScore$new(
        acq_id = "gsx",
        surrogate_id = "archive",
        candidate_sampler = SpaceSamplerUniform$new(),
        n_candidates = batch_size
      ),
      acq_functions = list(
        gsx = AcqFunctionDistGSx$new(
          al_distance = optimizer_pool_al_distance(distance, "standardize"))
      ),
      init_sampler = init_sampler,
      result_assigner = ResultAssignerNull$new()
    )
    optimizer_values <- list(batch_size = batch_size)
    if (!is.null(n_init)) {
      optimizer_values$n_init <- n_init
    }
    optimizer$param_set$set_values(.values = optimizer_values)
    return(optimizer)
  }

  candidate_sampler <- if (is.null(n_candidates)) {
    NULL
  } else {
    SpaceSamplerUniform$new()
  }
  n_candidates <- n_candidates %??% Inf

  optimizer <- switch(method,
    gsx = {
      OptimizerAL$new(
        proposer = ALProposerSequentialReference$new(
          acq_id = "gsx",
          surrogate_id = "archive",
          candidate_sampler = candidate_sampler,
          n_candidates = n_candidates
        ),
        acq_functions = list(
          gsx = AcqFunctionDistGSx$new(
            al_distance = optimizer_pool_al_distance(distance, "standardize"))
        ),
        init_sampler = init_sampler,
        result_assigner = ResultAssignerNull$new()
      )
    },
    gsy = {
      OptimizerAL$new(
        proposer = ALProposerPseudoLabel$new(
          acq_id = "gsy",
          surrogate_id = "model",
          label_surrogate_id = "model",
          candidate_sampler = candidate_sampler,
          n_candidates = n_candidates
        ),
        surrogates = list(
          model = SurrogateLearner$new(
            learner = optimizer_pool_al_response_learner(learner),
            archive = NULL
          )
        ),
        acq_functions = list(
          gsy = AcqFunctionGSy$new()
        ),
        init_sampler = init_sampler,
        result_assigner = ResultAssignerNull$new()
      )
    },
    igs = {
      OptimizerAL$new(
        proposer = ALProposerPseudoLabel$new(
          acq_id = "igs",
          surrogate_id = "model",
          label_surrogate_id = "model",
          candidate_sampler = candidate_sampler,
          n_candidates = n_candidates
        ),
        surrogates = list(
          model = SurrogateLearner$new(
            learner = optimizer_pool_al_response_learner(learner),
            archive = NULL
          )
        ),
        acq_functions = list(
          igs = AcqFunctionDistIGS$new(
            al_distance = optimizer_pool_al_distance(distance, "standardize"))
        ),
        init_sampler = init_sampler,
        result_assigner = ResultAssignerNull$new()
      )
    },
    qbc = {
      learner_qbc <- LearnerRegrBootstrapSE$new(
        optimizer_pool_al_response_learner(learner)
      )
      learner_qbc$param_set$set_values(n_bootstrap = k_qbc)
      learner_qbc$predict_type <- "se"

      OptimizerAL$new(
        proposer = ALProposerPseudoLabel$new(
          acq_id = "sd",
          surrogate_id = "model",
          label_surrogate_id = "model",
          candidate_sampler = candidate_sampler,
          n_candidates = n_candidates
        ),
        surrogates = list(
          model = SurrogateLearner$new(learner = learner_qbc, archive = NULL)
        ),
        acq_functions = list(
          sd = acqf("sd")
        ),
        init_sampler = init_sampler,
        result_assigner = ResultAssignerNull$new()
      )
    },
    ideal = {
      OptimizerAL$new(
        proposer = ALProposerPseudoLabel$new(
          acq_id = "ideal",
          surrogate_id = "model",
          label_surrogate_id = "model",
          candidate_sampler = candidate_sampler,
          n_candidates = n_candidates
        ),
        surrogates = list(
          model = SurrogateLearner$new(
            learner = optimizer_pool_al_response_learner(learner),
            archive = NULL
          )
        ),
        acq_functions = list(
          ideal = AcqFunctionDistIDEAL$new(
            delta = delta,
            al_distance = optimizer_pool_al_distance(distance, "affine")
          )
        ),
        init_sampler = init_sampler,
        result_assigner = ResultAssignerNull$new()
      )
    }
  )

  optimizer_values <- list(batch_size = batch_size)
  if (!is.null(n_init)) {
    optimizer_values$n_init <- n_init
  }
  optimizer$param_set$set_values(.values = optimizer_values)

  optimizer
}


optimizer_pool_al_response_learner <- function(learner) {
  assert_r6(learner, "LearnerRegr")
  learner_clone <- learner$clone(deep = TRUE)
  learner_clone$predict_type <- "response"
  learner_clone
}

optimizer_pool_al_default_init_method <- function(method) {
  switch(method,
    random = "random",
    qbc = "random",
    ideal = "kmeans",
    "gsx"
  )
}

# Resolve the user-facing `distance` argument to a fresh ALDistance: NULL falls back to the
# method's paper default (`default_key`), a string is a mlr_al_distances key, an ALDistance
# prototype is cloned.
optimizer_pool_al_distance <- function(distance, default_key) {
  if (is.null(distance)) {
    clx_ald(default_key)
  } else if (inherits(distance, "ALDistance")) {
    distance$clone(deep = TRUE)
  } else {
    clx_ald(distance)
  }
}

optimizer_pool_al_init_sampler <- function(method, init_method = NULL, distance = NULL) {
  init_method <- init_method %??% optimizer_pool_al_default_init_method(method)

  distance_init <- optimizer_pool_al_distance(distance,
    if (identical(method, "ideal")) "affine" else "standardize")

  on_discrete <- switch(init_method,
    gsx = SpaceSamplerGSx$new(distance = distance_init),
    random = SpaceSamplerUniform$new(),
    kmeans = if (inherits(distance_init, "ALDistanceGeometry")) {
      SpaceSamplerKMeans$new(distance = distance_init)
    } else {
      # k-means needs an embedding; medoids are its analogue under a plain
      # dissimilarity such as Gower
      SpaceSamplerKMedoids$new(distance = distance_init)
    }
  )

  on_continuous <- if (identical(method, "ideal")) {
    SpaceSamplerLhs$new()
  } else {
    SpaceSamplerUniform$new()
  }

  SpaceSamplerConditional$new(
    on_discrete = on_discrete,
    on_continuous = on_continuous
  )
}
