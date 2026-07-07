make_surrogate_performance_objective <- function() {
  ObjectiveRFun$new(
    fun = function(xs) list(y = sin(pi * xs$x) + 0.1 * xs$x),
    domain = ps(x = p_dbl(lower = 0, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )
}

make_surrogate_performance_task <- function(n = 41L) {
  dt <- data.table(x = seq(0, 2, length.out = n))
  dt[, y := sin(pi * x) + 0.1 * x]
  as_task_regr(dt, target = "y")
}

make_surrogate_performance_pool_objective <- function(n = 25L) {
  dt <- data.table(x = seq(0, 2, length.out = n))
  dt[, y := sin(pi * x) + 0.1 * x]
  objective <- ObjectiveDataset$new(
    dataset = dt,
    domain = ps(x = p_dbl(lower = 0, upper = 2)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )
  list(objective = objective, task = as_task_regr(copy(dt), target = "y"))
}


test_that("CallbackSurrogatePerformance logs one row per batch", {
  set.seed(1)
  perf <- clbk(
    "celecx.surrogate_performance",
    surrogate_id = "model",
    task = make_surrogate_performance_task()
  )

  result <- optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 6L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 2L,
    n_candidates = 6L, n_init = 2L
  )

  perf_data <- perf$data
  expect_equal(nrow(perf_data), result$instance$archive$n_batch)
  expect_equal(perf_data$n_evals[[nrow(perf_data)]], 6L)
  expect_names(names(perf_data), must.include = c(
    "batch_nr", "n_evals", "timestamp", "surrogate_id",
    "regr.rsq", "regr.mae"
  ))
  expect_type(perf_data[["regr.rsq"]], "double")
  expect_type(perf_data[["regr.mae"]], "double")
  expect_equal(perf_data$surrogate_id, rep("model", nrow(perf_data)))
  expect_equal(perf_data$batch_nr, seq_len(nrow(perf_data)))
})


test_that("CallbackSurrogatePerformance honors named measures", {
  set.seed(5)
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = make_surrogate_performance_task(),
    measures = list(r2 = msr("regr.rsq"), mae = msr("regr.mae"))
  )

  optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 4L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 1L,
    n_candidates = 4L, n_init = 2L
  )

  expect_names(names(perf$data), must.include = c("r2", "mae"))
  expect_names(names(perf$data), disjunct.from = c("regr.rsq", "regr.mae"))
})


test_that("CallbackSurrogatePerformance requires a list of measures", {
  expect_error(
    CallbackSurrogatePerformance$new(
      surrogate_id = "model",
      task = make_surrogate_performance_task(),
      measures = c("regr.rsq", "regr.mae")
    ),
    "list"
  )

  expect_error(
    CallbackSurrogatePerformance$new(
      surrogate_id = "model",
      task = make_surrogate_performance_task(),
      measures = msr("regr.mae")
    ),
    "list"
  )

  expect_error(
    CallbackSurrogatePerformance$new(
      surrogate_id = "model",
      task = make_surrogate_performance_task(),
      measures = list("regr.mae")
    ),
    "R6 class"
  )

  expect_error(
    CallbackSurrogatePerformance$new(
      surrogate_id = "model",
      task = make_surrogate_performance_task(),
      measures = list(msr("classif.acc"))
    ),
    "must be a regression measure"
  )
})


test_that("CallbackSurrogatePerformance stage hooks work after cloning", {
  set.seed(4)
  original <- clbk(
    "celecx.surrogate_performance",
    surrogate_id = "model",
    task = make_surrogate_performance_task(),
    measures = list(mae = msr("regr.mae"))
  )
  perf <- original$clone(deep = TRUE)

  result <- optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 5L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 1L,
    n_candidates = 5L, n_init = 2L
  )

  expect_equal(nrow(perf$data), result$instance$archive$n_batch)
  expect_equal(perf$data$n_evals[[nrow(perf$data)]], 5L)
  expect_equal(nrow(original$data), 0L)
})


test_that("CallbackSurrogatePerformance works for pool active learning model surrogate", {
  set.seed(2)
  setup <- make_surrogate_performance_pool_objective()
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = setup$task,
    measures = list(mae = msr("regr.mae"))
  )

  result <- optimize_active(
    objective = setup$objective,
    n_evals = 7L,
    optimizer = optimizer_pool_al(
      "gsy",
      learner = lrn("regr.featureless"),
      n_init = 3L,
      batch_size = 1L
    ),
    callbacks = list(perf)
  )

  perf_data <- perf$data
  expect_equal(nrow(perf_data), result$instance$archive$n_batch)
  expect_equal(perf_data$n_evals[[nrow(perf_data)]], 7L)
  expect_type(perf_data$mae, "double")
  expect_equal(perf_data$surrogate_id, rep("model", nrow(perf_data)))
})


test_that("CallbackSurrogatePerformance rejects unknown surrogate ids", {
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "missing",
    task = make_surrogate_performance_task()
  )

  expect_error(
    optimize_active(
      objective = make_surrogate_performance_objective(),
      n_evals = 4L,
      callbacks = list(perf),
      learner = lrn("regr.featureless"),
      n_bootstrap = 2L,
      batch_size = 1L,
      n_candidates = 4L, n_init = 2L
    ),
    "Unknown surrogate id 'missing'"
  )
  expect_equal(nrow(perf$data), 0L)
})


test_that("CallbackSurrogatePerformance rejects non-predictive surrogates", {
  setup <- make_surrogate_performance_pool_objective()
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "archive",
    task = setup$task
  )

  expect_error(
    optimize_active(
      objective = setup$objective,
      n_evals = 4L,
      optimizer = optimizer_pool_al("gsx", n_init = 2L, batch_size = 1L),
      callbacks = list(perf)
    ),
    "SurrogateNull does not make predictions"
  )
})


test_that("CallbackSurrogatePerformance$task() builds a TaskLCE", {
  set.seed(11)
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = make_surrogate_performance_task(),
    measures = list(mae = msr("regr.mae"))
  )

  result <- optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 6L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 2L,
    n_candidates = 6L, n_init = 2L
  )

  task_lce <- perf$task()
  expect_r6(task_lce, "TaskLCE")
  expect_equal(task_lce$target_names, "mae")
  expect_equal(task_lce$feature_names, "batch_nr")
  expect_equal(task_lce$archive_x, "x")
  expect_equal(task_lce$archive_y, "y")

  # measure provenance is carried; a continuous run has no pool
  expect_r6(task_lce$measure, "Measure")
  expect_equal(task_lce$measure$id, "regr.mae")
  expect_null(task_lce$pool)

  # search space / codomain are carried over from the archive
  archive <- result$instance$archive
  expect_r6(task_lce$search_space, "ParamSet")
  expect_r6(task_lce$codomain, "Codomain")
  expect_set_equal(task_lce$search_space$ids(), archive$cols_x)
  expect_set_equal(task_lce$codomain$target_ids, archive$cols_y)
  expect_false(identical(task_lce$search_space, archive$search_space))
  expect_equal(task_lce$nrow, result$instance$archive$n_evals)
  expect_equal(sort(unique(task_lce$batch_nrs)), seq_len(result$instance$archive$n_batch))
  perf_dt <- perf$data
  expect_equal(task_lce$truth(),
    perf_dt[list(task_lce$batch_nrs), "mae", on = "batch_nr"][[1L]])
})


test_that("CallbackSurrogatePerformance$task() carries the pool for pool runs", {
  set.seed(14)
  setup <- make_surrogate_performance_pool_objective()
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = setup$task,
    measures = list(mae = msr("regr.mae"))
  )
  optimize_active(
    objective = setup$objective,
    n_evals = 6L,
    optimizer = optimizer_pool_al(
      "gsy", learner = lrn("regr.featureless"), n_init = 3L, batch_size = 1L),
    callbacks = list(perf)
  )

  task_lce <- perf$task()
  expect_data_table(task_lce$pool, min.rows = 1L)
  expect_set_equal(names(task_lce$pool), "x")
})


test_that("CallbackSurrogatePerformance$task() requires explicit measure when ambiguous", {
  set.seed(12)
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = make_surrogate_performance_task(),
    measures = list(r2 = msr("regr.rsq"), mae = msr("regr.mae"))
  )
  optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 4L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 1L,
    n_candidates = 4L, n_init = 2L
  )

  expect_error(perf$task(), "supply 'measure'")
  expect_r6(perf$task(measure = "mae"), "TaskLCE")
  expect_error(perf$task(measure = "unknown"), "'measure'")
})


test_that("CallbackSurrogatePerformance$task() fails before optimization", {
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = make_surrogate_performance_task()
  )
  expect_error(perf$task(), "no archive")
})


test_that("CallbackSurrogatePerformance$clear() drops the archive reference", {
  set.seed(13)
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = make_surrogate_performance_task(),
    measures = list(mae = msr("regr.mae"))
  )
  optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 4L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 1L,
    n_candidates = 4L, n_init = 2L
  )
  expect_r6(perf$task(), "TaskLCE")
  perf$clear()
  expect_null(perf$state$archive)
  expect_error(perf$task(), "no archive")
})


test_that("CallbackSurrogatePerformance clears data when reused", {
  set.seed(3)
  perf <- CallbackSurrogatePerformance$new(
    surrogate_id = "model",
    task = make_surrogate_performance_task(),
    measures = list(mae = msr("regr.mae"))
  )

  result1 <- optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 6L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 2L,
    n_candidates = 6L, n_init = 2L
  )
  expect_equal(nrow(perf$data), result1$instance$archive$n_batch)

  result2 <- optimize_active(
    objective = make_surrogate_performance_objective(),
    n_evals = 4L,
    callbacks = list(perf),
    learner = lrn("regr.featureless"),
    n_bootstrap = 2L,
    batch_size = 1L,
    n_candidates = 4L, n_init = 2L
  )

  expect_equal(nrow(perf$data), result2$instance$archive$n_batch)
  expect_equal(perf$data$n_evals[[nrow(perf$data)]], 4L)
})
