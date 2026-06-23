make_replay_setup <- function(n = 25L) {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = sin(pi * xs$x) + 0.1 * xs$x),
    domain = ps(x = p_dbl(lower = 0, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )
  dt <- data.table(x = seq(0, 2, length.out = n))
  dt[, y := sin(pi * x) + 0.1 * x]
  list(objective = objective, test_task = as_task_regr(dt, target = "y"))
}

run_replay_archive <- function(objective, n_evals = 8L) {
  result <- optimize_active(objective = objective, n_evals = n_evals,
    learner = lrn("regr.featureless"), n_bootstrap = 2L,
    batch_size = 2L, acq_evals = 6L)
  result$instance$archive
}

test_that("replay_surrogate_performance builds a TaskLCE matching the archive", {
  set.seed(1)
  setup <- make_replay_setup()
  archive <- run_replay_archive(setup$objective)

  task_lce <- replay_surrogate_performance(archive, lrn("regr.featureless"),
    setup$test_task, measures = list(mae = msr("regr.mae")))

  expect_r6(task_lce, "TaskLCE")
  expect_equal(task_lce$target_names, "mae")
  expect_equal(task_lce$feature_names, "batch_nr")
  expect_set_equal(task_lce$archive_x, archive$cols_x)
  expect_set_equal(task_lce$archive_y, archive$cols_y)
  expect_r6(task_lce$search_space, "ParamSet")
  expect_r6(task_lce$codomain, "Codomain")
  expect_r6(task_lce$measure, "Measure")
  expect_equal(task_lce$measure$id, "regr.mae")
  expect_equal(task_lce$nrow, archive$n_evals)
  expect_set_equal(unique(task_lce$batch_nrs), sort(unique(archive$data$batch_nr)))
  expect_true(all(is.finite(task_lce$truth())))
})

test_that("replay_surrogate_performance accepts a Surrogate and requires a measure choice", {
  set.seed(2)
  setup <- make_replay_setup()
  archive <- run_replay_archive(setup$objective)

  surrogate <- SurrogateLearner$new(lrn("regr.featureless"))
  task_lce <- replay_surrogate_performance(archive, surrogate, setup$test_task,
    measures = list(mae = msr("regr.mae")))
  expect_r6(task_lce, "TaskLCE")
  expect_equal(task_lce$target_names, "mae")

  # ambiguous measures require an explicit choice
  expect_error(
    replay_surrogate_performance(archive, lrn("regr.featureless"), setup$test_task,
      measures = list(rsq = msr("regr.rsq"), mae = msr("regr.mae"))),
    "supply 'measure'"
  )
  task_rsq <- replay_surrogate_performance(archive, lrn("regr.featureless"),
    setup$test_task,
    measures = list(rsq = msr("regr.rsq"), mae = msr("regr.mae")),
    measure = "rsq")
  expect_equal(task_rsq$target_names, "rsq")
})

test_that("replay_surrogate_performance rejects multi-target archives", {
  codomain <- Codomain$new(
    ps(y1 = p_dbl(tags = "learn"), y2 = p_dbl(tags = "learn"))$domains)
  archive <- ArchiveBatch$new(ps(x = p_dbl(0, 1)), codomain)
  archive$data <- data.table(x = c(0.2, 0.8), y1 = c(1, 2), y2 = c(3, 4),
    batch_nr = c(1L, 2L))
  task <- as_task_regr(data.table(x = c(0.1, 0.5), y1 = c(0, 1)), target = "y1")
  expect_error(
    replay_surrogate_performance(archive, lrn("regr.featureless"), task),
    "single-target"
  )
})

test_that("replay reproduces the callback's TaskLCE for a deterministic surrogate", {
  set.seed(4)
  setup <- make_replay_setup()
  # regr.lm has a native se and a deterministic, input-dependent response, so the
  # online callback (scoring the optimizer's surrogate after each batch) and the
  # offline replay (refitting on each batch prefix) must agree exactly. We avoid
  # regr.featureless (constant prediction -> degenerate uncertainty sampling) and
  # a bootstrap-SE wrapper (response is the mean over random bootstrap fits, so
  # it cannot match). batch_size = 3 keeps the initial lm fit non-degenerate
  # (>= 1 residual degree of freedom, hence a finite se for the acquisition).
  learner <- lrn("regr.lm")
  measures <- list(mae = msr("regr.mae"))

  perf <- CallbackSurrogatePerformance$new(surrogate_id = "uncertainty",
    task = setup$test_task, measures = measures)
  result <- optimize_active(objective = setup$objective, n_evals = 9L,
    callbacks = list(perf), learner = learner, batch_size = 3L, acq_evals = 6L)

  callback_task <- perf$task()
  replay_task <- replay_surrogate_performance(result$instance$archive,
    learner, setup$test_task, measures = measures)

  expect_equal(replay_task$batch_nrs, callback_task$batch_nrs)
  expect_equal(replay_task$truth(), callback_task$truth())
  expect_equal(replay_task$data(), callback_task$data())
})

test_that("score_surrogate_on_task inverts an output trafo the surrogate leaves transformed", {
  set.seed(3)
  setup <- make_replay_setup()
  archive <- run_replay_archive(setup$objective)
  base <- lrn("regr.featureless", predict_type = "se")

  surrogate_inv <- SurrogateLearner$new(base$clone(deep = TRUE),
    output_trafo = ot("log", invert_posterior = TRUE), archive = archive)
  surrogate_noinv <- SurrogateLearner$new(base$clone(deep = TRUE),
    output_trafo = ot("log", invert_posterior = FALSE), archive = archive)
  surrogate_inv$update()
  surrogate_noinv$update()

  measures <- list(mae = msr("regr.mae"))
  # invert_posterior = TRUE: the surrogate inverts in $predict().
  # invert_posterior = FALSE: score_surrogate_on_task must invert; same result.
  score_inv <- score_surrogate_on_task(surrogate_inv, setup$test_task, measures)
  score_noinv <- score_surrogate_on_task(surrogate_noinv, setup$test_task, measures)
  expect_equal(score_noinv[["mae"]], score_inv[["mae"]])

  # ...and inverting genuinely changes the result versus scoring on the
  # transformed scale (guards against the inversion being a no-op).
  pred_transformed <- surrogate_noinv$predict(
    setup$test_task$data(cols = surrogate_noinv$cols_x))
  raw <- PredictionRegr$new(task = setup$test_task,
    response = pred_transformed$mean)
  expect_false(isTRUE(all.equal(
    msr("regr.mae")$score(raw, task = setup$test_task), score_noinv[["mae"]])))
})
