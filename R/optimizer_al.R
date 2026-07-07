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
#' - a surrogate (registered under id `"model"`) that can provide standard
#'   errors (either native `"se"`, [LearnerRegrBootstrapSE], or
#'   [LearnerRegrQuantileSE])
#' - proposer-based batch construction via [ALProposerScore],
#'   [ALProposerSequentialScore], or [ALProposerPseudoLabel]
#'
#' `n_candidates` controls the size of the candidate pool scored in each
#' proposal round. For continuous search spaces, candidates are drawn from the
#' search space by `candidate_sampler`; for finite pools, candidates are
#' subsampled uniformly from the remaining pool.
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
#' @param candidate_sampler (`NULL` | [SpaceSampler])\cr
#'   Sampler that draws the scored candidates from a continuous search space.
#'   `NULL` uses [SpaceSamplerUniform].
#' @param n_candidates (`integer(1)`)\cr
#'   Number of candidate points scored per proposal round.
#' @param n_init (`NULL` | `integer(1)`)\cr
#'   Number of initial evaluations. `NULL` uses [OptimizerAL]'s default
#'   initialization policy (`4 * d` for fresh runs, none when the archive is
#'   already populated).
#'
#' @return Configured [OptimizerAL].
#'
#' @export
optimizer_al <- function(learner,
    se_method = c("auto", "bootstrap", "quantile"),
    n_bootstrap = 30L,
    batch_size = 1L,
    multipoint_method = c("greedy", "local_penalization", "diversity", "constant_liar"),
    candidate_sampler = NULL,
    n_candidates = 100L,
    n_init = NULL) {

  assert_r6(learner, "LearnerRegr")
  se_method <- match.arg(se_method)
  multipoint_method <- match.arg(multipoint_method)
  assert_int(n_bootstrap, lower = 2L)
  assert_int(batch_size, lower = 1L)
  assert_int(n_candidates, lower = 1L)
  assert_r6(candidate_sampler, "SpaceSampler", null.ok = TRUE)
  assert_int(n_init, lower = 1L, null.ok = TRUE)

  if (batch_size > n_candidates) {
    stopf("batch_size (%i) must be <= n_candidates (%i)", batch_size, n_candidates)
  }
  if (multipoint_method == "constant_liar" && batch_size < 2L) {
    stopf("multipoint_method = 'constant_liar' requires batch_size >= 2")
  }

  learner_se <- optimizer_al_se_learner(
    learner = learner,
    se_method = se_method,
    n_bootstrap = n_bootstrap
  )

  sampler_continuous <- if (is.null(candidate_sampler)) {
    SpaceSamplerUniform$new()
  } else {
    candidate_sampler$clone(deep = TRUE)
  }
  init_sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerUniform$new(),
    on_continuous = sampler_continuous$clone(deep = TRUE)
  )
  proposal_sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerUniform$new(),
    on_continuous = sampler_continuous
  )

  proposer <- switch(multipoint_method,
    greedy = ALProposerScore$new(
      acq_id = "sd",
      surrogate_id = "model",
      candidate_sampler = proposal_sampler,
      n_candidates = n_candidates
    ),
    local_penalization = ALProposerSequentialScore$new(
      acq_id = "sd",
      surrogate_id = "model",
      score_modifier = ALScoreModifierLocalPenalization$new(),
      candidate_sampler = proposal_sampler,
      n_candidates = n_candidates
    ),
    diversity = ALProposerSequentialScore$new(
      acq_id = "sd",
      surrogate_id = "model",
      score_modifier = ALScoreModifierDiversity$new(),
      candidate_sampler = proposal_sampler,
      n_candidates = n_candidates
    ),
    constant_liar = ALProposerPseudoLabel$new(
      acq_id = "sd",
      surrogate_id = "model",
      label_surrogate_id = "model",
      candidate_sampler = proposal_sampler,
      n_candidates = n_candidates
    )
  )

  optimizer <- OptimizerAL$new(
    proposer = proposer,
    surrogates = list(
      model = SurrogateLearner$new(learner = learner_se, archive = NULL)
    ),
    acq_functions = list(
      sd = acqf("sd")
    ),
    init_sampler = init_sampler,
    result_assigner = ResultAssignerNull$new()
  )
  optimizer_values <- list(batch_size = batch_size)
  if (!is.null(n_init)) {
    optimizer_values$n_init <- n_init
  }
  optimizer$param_set$set_values(.values = optimizer_values)

  optimizer
}


optimizer_al_se_learner <- function(learner,
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
