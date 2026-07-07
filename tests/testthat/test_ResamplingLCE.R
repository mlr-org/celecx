make_resampling_task <- function(n_batches = 6L, per_batch = 3L) {
  batches <- rep(seq_len(n_batches), each = per_batch)
  dt <- data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(batches),
    perf = rep(seq_len(n_batches) / n_batches, each = per_batch)
  )
  TaskLCE$new("rs", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
}

test_that("ResamplingLCE produces expanding windows", {
  set.seed(14)
  task <- make_resampling_task()
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 2L, step_size = 1L)
  rs$instantiate(task)

  expect_equal(rs$iters, 4L)
  for (i in seq_len(rs$iters)) {
    tr_batches <- sort(unique(task$data(rows = rs$train_set(i), cols = "batch_nr")[[1L]]))
    te_batches <- sort(unique(task$data(rows = rs$test_set(i), cols = "batch_nr")[[1L]]))
    expect_equal(tr_batches, seq_len(i + 1L))
    expect_equal(te_batches, i + 2L)
  }
})

test_that("ResamplingLCE respects horizon and step_size", {
  set.seed(15)
  task <- make_resampling_task(n_batches = 10L)
  rs <- rsmp("lce.expanding_cv", horizon = 2L, min_train_batches = 3L, step_size = 2L)
  rs$instantiate(task)

  # train ends at min_train_batches, +step_size, ..., max = n_batches - horizon = 8
  # train_end ∈ {3, 5, 7}; test batches are (3,4), (5,6), (7,8)
  expect_equal(rs$iters, 3L)
  tr3 <- sort(unique(task$data(rows = rs$train_set(3L), cols = "batch_nr")[[1L]]))
  te3 <- sort(unique(task$data(rows = rs$test_set(3L), cols = "batch_nr")[[1L]]))
  expect_equal(tr3, 1:7)
  expect_equal(te3, c(8L, 9L))
})

test_that("ResamplingLCE errors when too few batches", {
  set.seed(16)
  task <- make_resampling_task(n_batches = 3L)
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 3L, step_size = 1L)
  expect_error(rs$instantiate(task), "need at least")
})

test_that("ResamplingLCE requires min_train_batches (no default)", {
  set.seed(19)
  task <- make_resampling_task(n_batches = 6L)
  rs <- rsmp("lce.expanding_cv", horizon = 1L, step_size = 1L)
  expect_error(rs$instantiate(task), "min_train_batches")
})

test_that("ResamplingLCE rejects grouped and stratified tasks", {
  set.seed(20)
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 2L,
    step_size = 1L)

  task_group <- make_resampling_task(n_batches = 6L)
  task_group$cbind(data.table(group_id = rep(letters[1:6], each = 3L)))
  task_group$set_col_roles("group_id", roles = "group")
  expect_error(rs$clone(deep = TRUE)$instantiate(task_group), "does not support")

  task_stratum <- make_resampling_task(n_batches = 6L)
  task_stratum$cbind(data.table(stratum_id = rep(c("a", "b"), length.out = task_stratum$nrow)))
  task_stratum$set_col_roles("stratum_id", roles = "stratum")
  expect_error(rs$clone(deep = TRUE)$instantiate(task_stratum), "does not support")
})

test_that("ResamplingLCE works inside resample()", {
  set.seed(17)
  batches <- 1:6
  per_batch <- 3L
  perf <- 0.9 - 0.7 * exp(-0.4 * batches)
  dt <- data.table(
    x1 = rnorm(length(batches) * per_batch),
    x2 = rnorm(length(batches) * per_batch),
    y = rnorm(length(batches) * per_batch),
    batch_nr = as.integer(rep(batches, each = per_batch)),
    perf = rep(perf, each = per_batch)
  )
  task <- TaskLCE$new("t", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 2L, step_size = 1L)
  learner <- lrn("lce.parametric_exponential")
  rr <- suppressMessages(resample(task, learner, rs))
  expect_equal(rr$resampling$iters, 4L)
  agg <- rr$aggregate(msr("lce.rmse"))
  expect_true(is.finite(agg))
  expect_true(agg >= 0)

  # the registered default measure makes measure-less aggregation work
  expect_equal(mlr_reflections$default_measures$lce, "lce.mae")
  agg_default <- rr$aggregate()
  expect_named(agg_default, "lce.mae")
  expect_true(is.finite(agg_default))
})

test_that("ResamplingLCE folds parameter limits iterations", {
  set.seed(18)
  task <- make_resampling_task(n_batches = 8L)
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 1L,
    step_size = 1L, folds = 3L)
  rs$instantiate(task)
  expect_equal(rs$iters, 3L)
})
