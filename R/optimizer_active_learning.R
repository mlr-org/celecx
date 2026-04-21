#' @title Active Learning Optimizer Factory
#'
#' @description
#' Convenience constructor that wires together an [OptimizerAL] for
#' uncertainty-based active learning with optional multipoint proposal
#' heuristics.
#'
#' @details
#' This helper builds an active-learning optimizer around:
#' - an uncertainty acquisition function (`"sd"`)
#' - a surrogate that can provide standard errors (either native `"se"`,
#'   [LearnerRegrBootstrapSE], or [LearnerRegrQuantileSE])
#' - proposer-based batch construction via [ALProposerScore],
#'   [ALProposerSequentialScore], or [ALProposerPseudoLabel]
#'
#' `acq_evals` controls the size of the candidate pool scored in each
#' proposal round. For continuous search spaces, candidates are sampled from the
#' search space using a coarse translation of `acq_optimizer` to a
#' [SpaceSampler]. For finite pools, the same sampler is applied to the
#' remaining pool.
#'
#' @param learner ([mlr3::LearnerRegr])\cr
#'   Base regression learner used as the surrogate.
#' @param se_method (`character(1)`)\cr
#'   How to obtain standard errors:
#'   - `"auto"`: use native `"se"` if supported by `learner`, otherwise `"bootstrap"`.
#'   - `"bootstrap"`: wrap via [LearnerRegrBootstrapSE].
#'   - `"quantile"`: wrap via [LearnerRegrQuantileSE] (requires `"quantiles"` support).
#' @param n_bootstrap (`integer(1)`)\cr
#'   Number of bootstrap replicates for `"bootstrap"`. Ignored otherwise.
#' @param batch_size (`integer(1)`)\cr
#'   Number of points proposed per active-learning iteration.
#' @param multipoint_method (`character(1)`)\cr
#'   Batch selection strategy:
#'   - `"greedy"`: top-k by acquisition score
#'   - `"local_penalization"`: sequential local-penalization heuristic
#'   - `"diversity"`: sequential score/diversity trade-off
#'   - `"constant_liar"`: sequential pseudo-label batching
#' @param acq_optimizer ([bbotk::Optimizer] | [mlr3mbo::AcqOptimizer])\cr
#'   Optimizer used to choose the candidate-generation strategy for acquisition
#'   scoring. The current implementation translates common optimizers to a
#'   [SpaceSampler] and ignores optimizer-specific search logic.
#' @param acq_evals (`integer(1)`)\cr
#'   Number of candidate points scored per proposal round.
#'
#' @return Configured [OptimizerAL].
#'
#' @export
optimizer_active_learning <- function(learner,
    se_method = c("auto", "bootstrap", "quantile"),
    n_bootstrap = 30L,
    batch_size = 1L,
    multipoint_method = c("greedy", "local_penalization", "diversity", "constant_liar"),
    acq_optimizer = opt("random_search"),
    acq_evals = 100L) {

  assert_r6(learner, "LearnerRegr")
  se_method <- match.arg(se_method)
  multipoint_method <- match.arg(multipoint_method)
  assert_int(n_bootstrap, lower = 2L)
  assert_int(batch_size, lower = 1L)
  assert_int(acq_evals, lower = 1L)

  if (batch_size > acq_evals) {
    stopf("batch_size (%i) must be <= acq_evals (%i)", batch_size, acq_evals)
  }
  if (multipoint_method == "constant_liar" && batch_size < 2L) {
    stopf("multipoint_method = 'constant_liar' requires batch_size >= 2")
  }

  learner_se <- ui_active_learning_prepare_se_learner(
    learner = learner,
    se_method = se_method,
    n_bootstrap = n_bootstrap
  )

  candidate_sampler_continuous <- ui_active_learning_space_sampler_from_optimizer(acq_optimizer)
  init_sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerUniform$new(),
    on_continuous = candidate_sampler_continuous$clone(deep = TRUE)
  )
  candidate_sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerUniform$new(),
    on_continuous = candidate_sampler_continuous
  )

  proposer <- switch(multipoint_method,
    greedy = ALProposerScore$new(
      acq_id = "sd",
      surrogate_id = "uncertainty",
      candidate_sampler = candidate_sampler,
      n_candidates = acq_evals
    ),
    local_penalization = ALProposerSequentialScore$new(
      acq_id = "sd",
      surrogate_id = "uncertainty",
      score_modifier = ALScoreModifierLocalPenalization$new(),
      candidate_sampler = candidate_sampler,
      n_candidates = acq_evals
    ),
    diversity = ALProposerSequentialScore$new(
      acq_id = "sd",
      surrogate_id = "uncertainty",
      score_modifier = ALScoreModifierDiversity$new(),
      candidate_sampler = candidate_sampler,
      n_candidates = acq_evals
    ),
    constant_liar = ALProposerPseudoLabel$new(
      acq_id = "sd",
      surrogate_id = "uncertainty",
      label_surrogate_id = "uncertainty",
      candidate_sampler = candidate_sampler,
      n_candidates = acq_evals
    )
  )

  optimizer <- OptimizerAL$new(
    proposer = proposer,
    surrogates = list(
      uncertainty = SurrogateLearner$new(learner = learner_se, archive = NULL)
    ),
    acq_functions = list(
      sd = acqf("sd")
    ),
    init_sampler = init_sampler,
    result_assigner = ResultAssignerNull$new()
  )
  optimizer$param_set$set_values(
    n_init = max(2L, batch_size),
    batch_size = batch_size
  )

  optimizer
}


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
