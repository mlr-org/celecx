make_lce_dt <- function(n_batches = 5L, per_batch = 3L) {
  batches <- rep(seq_len(n_batches), each = per_batch)
  data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(batches),
    perf = rep(seq_len(n_batches) / n_batches, each = per_batch)
  )
}

test_that("TaskLCE constructor sets roles", {
  set.seed(1)
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())

  expect_equal(task$task_type, "lce")
  expect_equal(task$target_names, "perf")
  expect_equal(task$feature_names, "batch_nr")
  expect_equal(task$archive_x, c("x1", "x2"))
  expect_equal(task$archive_y, "y")
  expect_equal(task$batch_nrs, rep(1:5, each = 3L))
  expect_type(task$truth(), "double")
})

test_that("TaskLCE rejects non-integer batch_nr column", {
  set.seed(2)
  dt <- make_lce_dt()
  dt[, batch_nr := as.numeric(batch_nr)]
  expect_error(
    TaskLCE$new("t", dt, target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain()),
    "must be integer"
  )
})

test_that("TaskLCE rejects non-numeric target column", {
  set.seed(3)
  dt <- make_lce_dt()
  dt[, perf := as.character(perf)]
  expect_error(
    TaskLCE$new("t", dt, target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain()),
    "must be numeric or integer"
  )
})

test_that("TaskLCE rejects duplicate column assignments", {
  set.seed(4)
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "perf",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain()),
    "must be disjoint"
  )
})

test_that("TaskLCE filter preserves archive columns", {
  set.seed(5)
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  task$filter(1:6)
  expect_equal(task$nrow, 6L)
  expect_equal(task$archive_x, c("x1", "x2"))
  expect_equal(task$archive_y, "y")
  ax <- task$archive_x_data()
  expect_equal(nrow(ax), 6L)
  expect_named(ax, c("x1", "x2"))
})

test_that("TaskLCE stores and exposes search_space and codomain", {
  set.seed(8)
  search_space <- lce_search_space()
  codomain <- lce_codomain()
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = search_space, codomain = codomain)

  expect_r6(task$search_space, "ParamSet")
  expect_r6(task$codomain, "Codomain")
  expect_set_equal(task$search_space$ids(), c("x1", "x2"))
  expect_set_equal(task$codomain$target_ids, "y")

  # stored objects are clones, decoupled from the inputs
  expect_false(identical(task$search_space, search_space))
  expect_false(identical(task$codomain, codomain))
})

test_that("TaskLCE rejects mismatched search_space / codomain ids", {
  set.seed(9)
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
      archive_x = "x1", archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain()),
    "search_space parameter ids"
  )
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "perf2",
      search_space = lce_search_space(), codomain = lce_codomain()),
    "codomain target ids"
  )
})

test_that("TaskLCE deep clone copies search_space and codomain", {
  set.seed(10)
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  clone <- task$clone(deep = TRUE)

  expect_false(identical(task$search_space, clone$search_space))
  expect_false(identical(task$codomain, clone$codomain))
  expect_set_equal(clone$search_space$ids(), c("x1", "x2"))
  expect_set_equal(clone$codomain$target_ids, "y")
})

test_that("TaskLCE measure and pool default to NULL", {
  set.seed(20)
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  expect_null(task$measure)
  expect_null(task$pool)
})

test_that("TaskLCE stores, validates, and deep-clones measure and pool", {
  set.seed(21)
  pool <- data.table(x1 = c(0.1, 0.2, 0.3), x2 = c(0.9, 0.8, 0.7))
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = msr("regr.mae"), pool = pool)

  expect_r6(task$measure, "Measure")
  expect_equal(task$measure$id, "regr.mae")
  expect_data_table(task$pool, nrows = 3L)
  expect_set_equal(names(task$pool), c("x1", "x2"))

  # measure is an R6 object (reference semantics), so a deep clone yields a
  # distinct object; this also confirms deep_clone descends into extra_args
  # (where the pool is copied in the same branch). The pool is a data.table
  # (value semantics), so only its contents are checked.
  clone <- task$clone(deep = TRUE)
  expect_false(identical(task$measure, clone$measure))
  expect_equal(clone$measure$id, "regr.mae")
  expect_equal(clone$pool, task$pool)
})

test_that("TaskLCE rejects a non-regression measure and mismatched pool columns", {
  set.seed(22)
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain(),
      measure = msr("classif.acc")),
    "regression measure"
  )
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain(),
      pool = data.table(x1 = 0.1, x3 = 0.2)),
    "pool columns"
  )
})

test_that("search_space and codomain are optional provenance", {
  set.seed(11)
  task <- TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y")

  expect_null(task$search_space)
  expect_null(task$codomain)
  expect_null(task$measure)
  expect_null(task$pool)

  clone <- task$clone(deep = TRUE)
  expect_null(clone$search_space)
  expect_null(clone$codomain)

  # cross-checks still apply when the provenance is given
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = ps(other = p_dbl())),
    "search_space parameter ids"
  )
  expect_error(
    TaskLCE$new("t", make_lce_dt(), target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      codomain = Codomain$new(ps(other = p_dbl(tags = "learn"))$domains)),
    "codomain target ids"
  )
})
