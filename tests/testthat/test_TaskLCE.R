test_that("TaskLCE basic construction works", {
  dt <- data.table(n_evals = 1:10, metric = 1/sqrt(1:10))
  task <- TaskLCE$new(id = "test", backend = dt, target = "metric",
    order = "n_evals")

  expect_equal(task$task_type, "lce")
  expect_equal(task$direction, "minimize")
  expect_equal(task$col_roles$order, "n_evals")
  expect_equal(task$target_names, "metric")
  expect_true("n_evals" %in% task$feature_names)
  expect_true("ordered" %in% task$properties)
  expect_equal(task$nrow, 10L)
})

test_that("TaskLCE direction can be maximize", {
  dt <- data.table(n_evals = 1:10, metric = sqrt(1:10))
  task <- TaskLCE$new(id = "test", backend = dt, target = "metric",
    order = "n_evals", direction = "maximize")

  expect_equal(task$direction, "maximize")
})

test_that("TaskLCE direction must be valid", {
  dt <- data.table(n_evals = 1:5, metric = 1:5)
  expect_error(
    TaskLCE$new(id = "t", backend = dt, target = "metric",
      order = "n_evals", direction = "invalid"),
    "arg"
  )
})

test_that("TaskLCE order column stays as feature", {
  dt <- data.table(n_evals = 1:10, extra = rnorm(10), metric = 1:10)
  task <- TaskLCE$new(id = "test", backend = dt, target = "metric",
    order = "n_evals")

  expect_true("n_evals" %in% task$feature_names)
  expect_true("extra" %in% task$feature_names)
  expect_equal(task$col_roles$order, "n_evals")
})

test_that("TaskLCE order active binding works", {
  dt <- data.table(n_evals = c(5L, 10L, 15L), metric = c(0.5, 0.3, 0.2))
  task <- TaskLCE$new(id = "test", backend = dt, target = "metric",
    order = "n_evals")

  ord <- task$order
  expect_data_table(ord, nrows = 3L)
  expect_named(ord, c("row_id", "order"))
  expect_equal(sort(ord$order), c(5, 10, 15))
})

test_that("TaskLCE with key column", {
  dt <- data.table(
    n_evals = rep(1:5, 2),
    run = factor(rep(c("a", "b"), each = 5)),
    metric = rnorm(10)
  )
  task <- TaskLCE$new(id = "test", backend = dt, target = "metric",
    order = "n_evals", key = "run")

  expect_equal(task$col_roles$key, "run")
  expect_true("keys" %in% task$properties)

  key_dt <- task$key
  expect_data_table(key_dt, nrows = 10L)
  expect_named(key_dt, c("row_id", "key"))
})

test_that("TaskLCE without key returns NULL for key binding", {
  dt <- data.table(n_evals = 1:5, metric = 1:5)
  task <- TaskLCE$new(id = "test", backend = dt, target = "metric",
    order = "n_evals")

  expect_null(task$key)
  expect_false("keys" %in% task$properties)
})

test_that("task_check_col_roles validates order column type", {
  dt <- data.table(n_evals = letters[1:5], metric = 1:5)
  expect_error(
    TaskLCE$new(id = "test", backend = dt, target = "metric",
      order = "n_evals"),
    "integer or numeric"
  )
})

test_that("as_task_lce.data.frame works", {
  dt <- data.table(n_evals = 1:10, metric = 1:10)
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "asdf")

  expect_r6(task, "TaskLCE")
  expect_equal(task$id, "asdf")
  expect_equal(task$direction, "minimize")
})

test_that("as_task_lce.TaskLCE identity and clone", {
  dt <- data.table(n_evals = 1:5, metric = 1:5)
  task <- TaskLCE$new(id = "orig", backend = dt, target = "metric",
    order = "n_evals")

  same <- as_task_lce(task)
  expect_identical(same, task)

  cloned <- as_task_lce(task, clone = TRUE)
  expect_false(identical(cloned, task))
  expect_equal(cloned$id, task$id)
})

test_that("generate_newdata_lce creates proper data", {
  dt <- data.table(n_evals = 1:10, extra = rnorm(10), metric = 1:10)
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")

  newdata <- generate_newdata_lce(task, c(11L, 12L, 15L))
  expect_data_table(newdata, nrows = 3L)
  expect_true("n_evals" %in% names(newdata))
  expect_true("metric" %in% names(newdata))
  expect_true("extra" %in% names(newdata))
  expect_equal(newdata$n_evals, c(11, 12, 15))
  expect_true(all(is.na(newdata$metric)))
  expect_true(all(is.na(newdata$extra)))
})

test_that("TaskLCE print works", {
  dt <- data.table(n_evals = 1:5, metric = 1:5)
  task <- as_task_lce(dt, target = "metric", order = "n_evals", id = "t")
  expect_output(print(task), "minimize")
})
