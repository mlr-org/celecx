ui_active_learning_prepare_se_learner <- function(learner,
    se_method = c("auto", "bootstrap", "quantile"),
    n_bootstrap = 30L) {
  assert_r6(learner, "LearnerRegr")
  se_method <- match.arg(se_method)
  assert_int(n_bootstrap, lower = 2L)

  learner_clone <- learner$clone(deep = TRUE)
  use_bootstrap <- se_method == "bootstrap" ||
    (se_method == "auto" && !"se" %in% learner_clone$predict_types)

  if (use_bootstrap) {
    learner_se <- LearnerRegrBootstrapSE$new(learner_clone)
    learner_se$param_set$set_values(n_bootstrap = n_bootstrap)
    learner_se$predict_type <- "se"
    return(learner_se)
  }

  if (se_method == "quantile") {
    learner_se <- LearnerRegrQuantileSE$new(learner_clone)
    learner_se$predict_type <- "se"
    return(learner_se)
  }

  learner_clone$predict_type <- "se"
  learner_clone
}


ui_active_learning_prepare_response_learner <- function(learner) {
  assert_r6(learner, "LearnerRegr")
  learner_clone <- learner$clone(deep = TRUE)
  learner_clone$predict_type <- "response"
  learner_clone
}


ui_active_learning_space_sampler_from_optimizer <- function(acq_optimizer) {
  if (inherits(acq_optimizer, "AcqOptimizer")) {
    acq_optimizer <- acq_optimizer$optimizer
  }

  if (inherits(acq_optimizer, "SpaceSampler")) {
    return(acq_optimizer$clone(deep = TRUE))
  }

  assert_r6(acq_optimizer, "Optimizer")

  optimizer_id <- tolower(acq_optimizer$id %??% "")

  # TODO: Map more acquisition optimizers to dedicated samplers when needed.
  if (grepl("sobol", optimizer_id, fixed = TRUE)) {
    return(SpaceSamplerSobol$new())
  }
  if (grepl("lhs", optimizer_id, fixed = TRUE)) {
    return(SpaceSamplerLhs$new())
  }

  SpaceSamplerUniform$new()
}


ui_optimizer_pool_al_default_init_method <- function(method) {
  switch(method,
    random = "random",
    qbc = "random",
    ideal = "kmeans",
    "gsx"
  )
}


ui_optimizer_pool_al_init_sampler <- function(method, init_method = NULL) {
  init_method <- init_method %??% ui_optimizer_pool_al_default_init_method(method)

  distance_init <- if (identical(method, "ideal")) {
    clx_ald("affine")
  } else {
    clx_ald("standardize")
  }

  on_discrete <- switch(init_method,
    gsx = SpaceSamplerGSx$new(distance = distance_init),
    random = SpaceSamplerUniform$new(),
    kmeans = SpaceSamplerKMeans$new(distance = distance_init)
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
