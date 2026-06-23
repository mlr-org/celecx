test_that("lce_link returns the built-in links with correct round-trips", {
  for (name in c("identity", "log", "logit")) {
    link <- lce_link(name)
    expect_equal(link$name, name)
    expect_function(link$transform)
    expect_function(link$inverse)
  }
  # round trips on valid points
  expect_equal(lce_link("identity")$inverse(lce_link("identity")$transform(c(-3, 0, 5))),
    c(-3, 0, 5))
  x_pos <- c(0.1, 1, 10)
  expect_equal(lce_link("log")$inverse(lce_link("log")$transform(x_pos)), x_pos)
  x_unit <- c(0.01, 0.5, 0.99)
  expect_equal(lce_link("logit")$inverse(lce_link("logit")$transform(x_unit)), x_unit,
    tolerance = 1e-12)
})

test_that("the built-in links are monotone increasing", {
  grid_for <- list(identity = seq(-5, 5, 0.5), log = seq(0.01, 10, 0.5),
    logit = seq(0.01, 0.99, 0.05))
  for (name in names(grid_for)) {
    z <- lce_link(name)$transform(grid_for[[name]])
    expect_true(all(diff(z) > 0))
  }
})

test_that("lce_link rejects unknown links", {
  expect_error(lce_link("probit"), "probit")
})

test_that("lce_link_from_range maps metric ranges to sensible links", {
  expect_equal(lce_link_from_range(c(0, Inf)), "log")
  expect_equal(lce_link_from_range(c(0, 1)), "logit")
  expect_equal(lce_link_from_range(c(-Inf, 1)), "identity")
  expect_equal(lce_link_from_range(c(-Inf, Inf)), "identity")
  expect_equal(lce_link_from_range(msr("regr.mae")$range), "log")
})

test_that("TaskLCE stores the link, defaulting to identity", {
  set.seed(1)
  dt <- data.table(x1 = rnorm(6), x2 = rnorm(6), y = rnorm(6),
    batch_nr = as.integer(rep(1:3, each = 2)), perf = rep(c(0.3, 0.2, 0.1), each = 2))
  task_default <- TaskLCE$new("t", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  expect_equal(task_default$link, "identity")

  task_log <- TaskLCE$new("t", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(), link = "log")
  expect_equal(task_log$link, "log")
  # survives deep clone
  expect_equal(task_log$clone(deep = TRUE)$link, "log")
})

test_that("TaskLCE rejects an unknown link", {
  set.seed(2)
  dt <- data.table(x1 = rnorm(6), x2 = rnorm(6), y = rnorm(6),
    batch_nr = as.integer(rep(1:3, each = 2)), perf = rep(c(0.3, 0.2, 0.1), each = 2))
  expect_error(
    TaskLCE$new("t", dt, target = "perf", batch_nr = "batch_nr",
      archive_x = c("x1", "x2"), archive_y = "y",
      search_space = lce_search_space(), codomain = lce_codomain(), link = "sqrt"),
    "link"
  )
})
