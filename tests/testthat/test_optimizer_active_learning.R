# =============================================================================
# UI Tests for optimizer_active_learning() / optimizer_pool_al() / optimize_active()
# =============================================================================

create_ui_test_objective <- function() {
  ObjectiveRFun$new(
    fun = function(xs) list(y = (xs$x - 1)^2 + 0.1 * sin(3 * xs$x)),
    domain = ps(x = p_dbl(lower = -2, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )
}


create_ui_test_pool_objective <- function(n = 20L, d = 2L, seed = 1L) {
  set.seed(seed)
  x <- matrix(runif(n * d), nrow = n, ncol = d)
  colnames(x) <- paste0("x", seq_len(d))
  dt <- as.data.table(x)
  dt[, y := rowSums(.SD^2), .SDcols = paste0("x", seq_len(d))]

  domain <- do.call(ps, set_names(
    lapply(seq_len(d), function(i) p_dbl(lower = 0, upper = 1)),
    paste0("x", seq_len(d))
  ))

  ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = ps(y = p_dbl(tags = "learn"))
  )
}


make_small_tree <- function() {
  learner <- lrn("regr.rpart")
  learner$param_set$set_values(minsplit = 1L, cp = 0)
  learner
}


test_that("optimizer_active_learning returns configured OptimizerAL", {
  optimizer <- optimizer_active_learning(
    learner = lrn("regr.rpart"),
    se_method = "auto",
    batch_size = 2L,
    multipoint_method = "greedy",
    acq_evals = 20L
  )

  expect_r6(optimizer, "OptimizerAL")
  expect_r6(optimizer$proposer, "ALProposerScore")
  expect_r6(optimizer$surrogates$uncertainty, "SurrogateLearner")
  expect_true(inherits(optimizer$surrogates$uncertainty$learner, "LearnerRegrBootstrapSE"))
  expect_equal(optimizer$param_set$values$batch_size, 2L)
  expect_equal(optimizer$param_set$values$n_init, 2L)
  expect_equal(optimizer$param_set$values$proposer.n_candidates, 20L)
})


test_that("optimizer_active_learning uses native or wrapped SE learners as requested", {
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("ranger")

  optimizer_native <- optimizer_active_learning(
    learner = lrn("regr.km", covtype = "matern5_2"),
    se_method = "auto"
  )
  optimizer_bootstrap <- optimizer_active_learning(
    learner = lrn("regr.km", covtype = "matern5_2"),
    se_method = "bootstrap",
    n_bootstrap = 5L
  )
  optimizer_quantile <- optimizer_active_learning(
    learner = lrn("regr.ranger"),
    se_method = "quantile"
  )

  expect_false(inherits(optimizer_native$surrogates$uncertainty$learner, "LearnerRegrBootstrapSE"))
  expect_equal(optimizer_native$surrogates$uncertainty$learner$predict_type, "se")
  expect_true(inherits(optimizer_bootstrap$surrogates$uncertainty$learner, "LearnerRegrBootstrapSE"))
  expect_true(inherits(optimizer_quantile$surrogates$uncertainty$learner, "LearnerRegrQuantileSE"))
})


test_that("optimizer_active_learning maps multipoint methods to proposer types", {
  optimizer_lp <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    multipoint_method = "local_penalization",
    batch_size = 2L
  )
  optimizer_div <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    multipoint_method = "diversity",
    batch_size = 2L
  )
  optimizer_cl <- optimizer_active_learning(
    learner = lrn("regr.featureless"),
    multipoint_method = "constant_liar",
    batch_size = 2L
  )

  expect_r6(optimizer_lp$proposer, "ALProposerSequentialScore")
  expect_r6(optimizer_lp$proposer$score_modifier, "ALScoreModifierLocalPenalization")
  expect_r6(optimizer_div$proposer, "ALProposerSequentialScore")
  expect_r6(optimizer_div$proposer$score_modifier, "ALScoreModifierDiversity")
  expect_r6(optimizer_cl$proposer, "ALProposerPseudoLabel")
})


test_that("optimizer_active_learning validates batch controls", {
  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.featureless"),
      batch_size = 5L,
      acq_evals = 4L
    ),
    "batch_size.*must be <= acq_evals"
  )

  expect_error(
    optimizer_active_learning(
      learner = lrn("regr.featureless"),
      multipoint_method = "constant_liar",
      batch_size = 1L
    ),
    "requires batch_size >= 2"
  )
})


test_that("optimize_active runs optimizer_active_learning and logs metrics", {
  set.seed(1)
  tracker <- MetricsTracker$new(metrics = list(best_y = metric_best_y))

  result <- optimize_active(
    objective = create_ui_test_objective(),
    n_evals = 8L,
    metrics_tracker = tracker,
    learner = lrn("regr.featureless"),
    batch_size = 2L,
    acq_evals = 10L
  )

  expect_type(result, "list")
  expect_r6(result$instance, "SearchInstance")
  expect_r6(result$optimizer, "OptimizerAL")
  expect_identical(result$metrics_tracker, tracker)
  expect_true(result$instance$is_terminated)
  expect_equal(tracker$n_batches, result$instance$archive$n_batch)
})


test_that("optimize_active accepts explicit pool optimizer", {
  set.seed(2)
  result <- optimize_active(
    objective = create_ui_test_pool_objective(n = 18L, d = 2L),
    n_evals = 6L,
    optimizer = optimizer_pool_al("gsx", n_init = 3L, batch_size = 1L)
  )

  expect_r6(result$instance, "SearchInstance")
  expect_r6(result$optimizer, "OptimizerAL")
  expect_equal(result$instance$archive$n_evals, 6L)
})


test_that("optimize_active silently ignores forecast tracking when it cannot be wired", {
  ForecastTracker <- R6Class("ForecastTracker",
    public = list(
      update = function(metrics_data) invisible(NULL)
    )
  )

  result <- optimize_active(
    objective = create_ui_test_objective(),
    n_evals = 5L,
    forecast_tracker = ForecastTracker$new(),
    learner = lrn("regr.featureless")
  )

  expect_r6(result$instance, "SearchInstance")
  expect_true(result$instance$is_terminated)
})


test_that("optimizer_pool_al returns OptimizerAL objects with method-specific components", {
  opt_gsx <- optimizer_pool_al("gsx", batch_size = 2L)
  opt_qbc <- optimizer_pool_al("qbc", learner = make_small_tree(), k_qbc = 3L)
  opt_ideal <- optimizer_pool_al("ideal", learner = make_small_tree(), delta = 2)

  expect_r6(opt_gsx, "OptimizerAL")
  expect_r6(opt_gsx$proposer, "ALProposerSequentialReference")
  expect_match(opt_gsx$init_sampler$on_discrete$id, "^gsx")
  expect_null(opt_gsx$param_set$values$n_init)

  expect_r6(opt_qbc$proposer, "ALProposerPseudoLabel")
  expect_true(inherits(opt_qbc$surrogates$model$learner, "LearnerRegrBootstrapSE"))
  expect_equal(opt_qbc$surrogates$model$learner$param_set$values$n_bootstrap, 3L)
  expect_equal(opt_qbc$init_sampler$on_discrete$id, "uniform")

  expect_r6(opt_ideal$proposer, "ALProposerPseudoLabel")
  expect_equal(opt_ideal$acq_functions$ideal$constants$values$delta, 2)
  expect_match(opt_ideal$init_sampler$on_discrete$id, "^kmeans")
})


test_that("optimizer_pool_al respects n_init default and init_method overrides", {
  objective <- create_ui_test_pool_objective(n = 24L, d = 2L)
  optimizer_default <- optimizer_pool_al("gsx", batch_size = 1L)
  optimizer_override <- optimizer_pool_al("random", init_method = "gsx", n_init = 5L)

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 10L)
  )
  optimizer_default$optimize(instance)

  expect_equal(instance$archive$data[, .N, by = batch_nr]$N[[1L]], 8L)
  expect_match(optimizer_override$init_sampler$on_discrete$id, "^gsx")
  expect_equal(optimizer_override$param_set$values$n_init, 5L)
})


test_that("optimizer_pool_al requires learners for model-based methods", {
  expect_error(optimizer_pool_al("gsy"))
  expect_error(optimizer_pool_al("igs"))
  expect_error(optimizer_pool_al("qbc"))
  expect_error(optimizer_pool_al("ideal"))
})


test_that("optimizer_pool_al validates pool_size for scored batching", {
  expect_error(
    optimizer_pool_al("ideal",
      learner = make_small_tree(),
      batch_size = 3L,
      pool_size = 2L
    ),
    "pool_size.*must be >= batch_size"
  )
})


test_that("optimizer_pool_al works on representative pool methods", {
  set.seed(3)
  objective <- create_ui_test_pool_objective(n = 20L, d = 2L)

  methods <- list(
    gsx = optimizer_pool_al("gsx", n_init = 3L),
    gsy = optimizer_pool_al("gsy", learner = make_small_tree(), n_init = 3L),
    qbc = optimizer_pool_al("qbc", learner = make_small_tree(), n_init = 3L),
    ideal = optimizer_pool_al("ideal", learner = make_small_tree(), n_init = 3L, batch_size = 2L)
  )

  for (optimizer in methods) {
    instance <- SearchInstance$new(
      objective = objective,
      terminator = trm("evals", n_evals = 6L)
    )
    expect_no_error(optimizer$optimize(instance))
    expect_equal(instance$archive$n_evals, 6L)
  }
})


test_that("optimizer_pool_al supports continuous objectives when pool_size is set", {
  set.seed(4)
  objective <- create_ui_test_objective()
  optimizer <- optimizer_pool_al(
    "ideal",
    learner = make_small_tree(),
    batch_size = 2L,
    pool_size = 12L
  )

  instance <- SearchInstance$new(
    objective = objective,
    terminator = trm("evals", n_evals = 6L)
  )

  expect_no_error(optimizer$optimize(instance))
  expect_equal(instance$archive$n_evals, 6L)
})
