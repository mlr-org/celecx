# Archive with three batches of two evaluations each; y per batch:
# (5, 3), (4, 6), (2, 7).
make_opt_archive <- function(direction = "minimize") {
  search_space <- ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 1))
  codomain <- Codomain$new(ps(y = p_dbl(tags = direction))$domains)
  archive <- ArchiveBatch$new(search_space = search_space, codomain = codomain,
    check_values = FALSE)
  for (y_batch in list(c(5, 3), c(4, 6), c(2, 7))) {
    n <- length(y_batch)
    archive$add_evals(
      xdt = data.table(x1 = runif(n), x2 = runif(n)),
      xss_trafoed = NULL,
      ydt = data.table(y = y_batch)
    )
  }
  archive
}

test_that("task_lce_best_so_far computes the running batch best (minimize)", {
  set.seed(1)
  archive <- make_opt_archive("minimize")
  task <- task_lce_best_so_far(archive)

  expect_r6(task, "TaskLCE")
  expect_equal(task$id, "best_so_far")
  expect_equal(task$target_names, "best_so_far")
  expect_equal(task$batch_nr, "batch_nr")
  expect_setequal(task$archive_x, c("x1", "x2"))
  expect_equal(task$archive_y, "y")
  expect_null(task$measure)
  expect_equal(task$link, "identity")

  # batch bests 3, 4, 2 -> running best 3, 3, 2, constant within batches
  expect_equal(task$truth(), c(3, 3, 3, 3, 2, 2))
  expect_equal(task$batch_nrs, rep(1:3, each = 2L))
})

test_that("task_lce_best_so_far respects a maximize codomain", {
  set.seed(2)
  archive <- make_opt_archive("maximize")
  task <- task_lce_best_so_far(archive, id = "npv_curve")

  expect_equal(task$id, "npv_curve")
  # batch bests 5, 6, 7 -> running best 5, 6, 7
  expect_equal(task$truth(), c(5, 5, 6, 6, 7, 7))
})

test_that("the direction travels with the task via the codomain", {
  set.seed(3)
  task_min <- task_lce_best_so_far(make_opt_archive("minimize"))
  task_max <- task_lce_best_so_far(make_opt_archive("maximize"))
  expect_true(lce_task_minimize(task_min))
  expect_false(lce_task_minimize(task_max))
  expect_true(lce_model_minimize(task_min))
  expect_false(lce_model_minimize(task_max))
})

test_that("direction-dependent learners work on a best-so-far task", {
  set.seed(4)
  task <- task_lce_best_so_far(make_opt_archive("minimize"))
  learner <- lrn("lce.featureless", type = "best")
  learner$train(task)
  pred <- learner$predict_newdata(data.table(batch_nr = 4:6L))
  expect_equal(pred$response, rep(2, 3L))
})

test_that("task_lce_best_so_far rejects undirected codomains", {
  set.seed(5)
  search_space <- ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 1))
  codomain <- Codomain$new(ps(y = p_dbl(tags = "learn"))$domains)
  archive <- ArchiveBatch$new(search_space = search_space, codomain = codomain,
    check_values = FALSE)
  archive$add_evals(xdt = data.table(x1 = 0.5, x2 = 0.5), xss_trafoed = NULL,
    ydt = data.table(y = 1))
  expect_error(task_lce_best_so_far(archive), "minimize")
})

test_that("task_lce_best_so_far rejects an empty archive", {
  search_space <- ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 1))
  codomain <- Codomain$new(ps(y = p_dbl(tags = "minimize"))$domains)
  archive <- ArchiveBatch$new(search_space = search_space, codomain = codomain,
    check_values = FALSE)
  expect_error(task_lce_best_so_far(archive), "no evaluations")
})
