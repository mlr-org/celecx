#' @title Active Learning Optimizer Factory
#'
#' @description
#' Convenience constructor that wires together an [mlr3mbo::OptimizerMbo] for
#' active learning with an uncertainty-based acquisition function (SD) and
#' optional multipoint (batch) proposal via [BatchProposer] batch strategies.
#'
#' @details
#' This helper focuses on the common active learning setup in this project:
#' - uses an SD acquisition function ([mlr3mbo::AcqFunctionSD])
#' - uses a surrogate that can provide standard errors (either native `"se"`,
#'   [LearnerRegrBootstrapSE], or [LearnerRegrQuantileSE])
#' - disables assigning a "best" point via [ResultAssignerNull] (required for
#'   codomains tagged with `"learn"`)
#'
#' @param learner ([mlr3::LearnerRegr])\cr
#'   Base regression learner used as the surrogate.
#' @param se_method (`character(1)`)\cr
#'   How to obtain standard errors:
#'   - `"auto"`: use native `"se"` if supported by `learner`, otherwise `"bootstrap"`.
#'   - `"bootstrap"`: wrap via [LearnerRegrBootstrapSE].
#'   - `"quantile"`: wrap via [LearnerRegrQuantileSE] (requires `"quantiles"` support).
#' @param se_method_n_bootstrap (`integer(1)`)\cr
#'   Number of bootstrap replicates for `"bootstrap"`. Ignored otherwise.
#' @param batch_size (`integer(1)`)\cr
#'   Number of points proposed per MBO iteration.
#' @param multipoint_method (`character(1)`)\cr
#'   Batch selection strategy:
#'   - `"greedy"`: top-k by acquisition score (equivalent to `n_candidates = batch_size`)
#'   - `"local_penalization"`: local penalization based diversity
#'   - `"diversity"`: score/diversity trade-off
#'   - `"constant_liar"`: uses [mlr3mbo::bayesopt_mpcl] (constant liar loop)
#' @param aqf_optimizer ([bbotk::Optimizer] | [mlr3mbo::AcqOptimizer])\cr
#'   Acquisition optimization backend. If an [bbotk::Optimizer] is provided, it
#'   is wrapped into a [BatchProposer]. Default is `opt("pool")`.
#' @param aqf_evals (`integer(1)`)\cr
#'   Evaluation budget for acquisition optimization (`TerminatorEvals$n_evals`).
#'
#' @return Configured [mlr3mbo::OptimizerMbo].
#'
#' @export
optimizer_active_learning <- function(learner,
    se_method = c("auto", "bootstrap", "quantile"),
    se_method_n_bootstrap = 30L,
    batch_size = 1L,
    multipoint_method = c("greedy", "local_penalization", "diversity", "constant_liar"),
    aqf_optimizer = opt("pool"),
    aqf_evals = 100L) {

  assert_r6(learner, "LearnerRegr")
  se_method <- match.arg(se_method)
  multipoint_method <- match.arg(multipoint_method)
  assert_int(se_method_n_bootstrap, lower = 2L)
  assert_int(batch_size, lower = 1L)
  assert_int(aqf_evals, lower = 1L)

  if (batch_size > aqf_evals) {
    stopf("batch_size (%i) must be <= aqf_evals (%i)", batch_size, aqf_evals)
  }
  if (multipoint_method == "constant_liar" && batch_size < 2L) {
    stopf("multipoint_method = 'constant_liar' requires batch_size >= 2")
  }

  # --------------------------------------------------------------------------
  # Surrogate learner with SE support
  # --------------------------------------------------------------------------
  use_bootstrap <- se_method == "bootstrap" ||
    (se_method == "auto" && !"se" %in% learner$predict_types)

  learner_se <- if (use_bootstrap) {
    lrn("regr.bootstrap_se",
      learner = learner,
      predict_type = "se",
      n_bootstrap = se_method_n_bootstrap
    )
  } else if (se_method == "quantile") {
    lrn("regr.quantile_se", learner = learner, predict_type = "se")
  } else {
    learner <- learner$clone(deep = TRUE)
    learner$predict_type <- "se"
    learner
  }

  surrogate <- SurrogateLearner$new(learner = learner_se, archive = NULL)
  acq_function <- AcqFunctionSD$new(surrogate = NULL)

  # --------------------------------------------------------------------------
  # Acquisition optimizer (inner optimization)
  # --------------------------------------------------------------------------
  acq_terminator <- TerminatorEvals$new()
  acq_terminator$param_set$set_values(n_evals = aqf_evals, k = 0L)

  acq_optimizer <- if (inherits(aqf_optimizer, "AcqOptimizer")) {
    aqf_optimizer
  } else {
    assert_r6(aqf_optimizer, "OptimizerBatch")
    BatchProposer$new(
      optimizer = aqf_optimizer,
      terminator = acq_terminator,
      acq_function = NULL
    )
  }

  acq_optimizer$terminator <- acq_terminator
  acq_optimizer$acq_function <- acq_function

  # --------------------------------------------------------------------------
  # Multipoint configuration
  # --------------------------------------------------------------------------
  is_batch_proposer <- inherits(acq_optimizer, "BatchProposer")
  needs_batch_proposer <- multipoint_method %in% c("local_penalization", "diversity")

  if (needs_batch_proposer && !is_batch_proposer) {
    stopf(
      "multipoint_method = '%s' requires aqf_optimizer to be a BatchProposer",
      multipoint_method
    )
  }

  loop_function <- bayesopt_ego
  loop_args <- list()

  if (multipoint_method == "constant_liar") {
    loop_function <- bayesopt_mpcl
    loop_args <- list(q = batch_size)
    acq_optimizer$param_set$set_values(n_candidates = 1L)
    if (is_batch_proposer) {
      acq_optimizer$pool_factor <- 1L
      acq_optimizer$batch_strategy <- batch_strategy_greedy()
    }
  } else {
    acq_optimizer$param_set$set_values(n_candidates = batch_size)
    if (is_batch_proposer) {
      batch_strategy <- switch(multipoint_method,
        greedy = batch_strategy_greedy(),
        local_penalization = batch_strategy_local_penalization(),
        diversity = batch_strategy_diversity()
      )
      acq_optimizer$batch_strategy <- batch_strategy
      if (multipoint_method == "greedy") {
        acq_optimizer$pool_factor <- 1L
      }
    }
  }

  OptimizerMbo$new(
    loop_function = loop_function,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    args = loop_args,
    result_assigner = ResultAssignerNull$new()
  )
}
