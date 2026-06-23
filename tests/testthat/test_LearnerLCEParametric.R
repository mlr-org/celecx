make_curve <- function(formula, n_batches = 10L, per_batch = 3L, noise = 0.0) {
  batches <- seq_len(n_batches)
  perf <- formula(batches) + rnorm(n_batches, 0, noise)
  dt <- data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(rep(batches, each = per_batch)),
    perf = rep(perf, each = per_batch)
  )
  TaskLCE$new("curve", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
}

test_that("parametric exponential SE is finite and non-negative", {
  set.seed(31)
  task <- make_curve(function(b) 0.9 - 0.7 * exp(-0.3 * b), noise = 0.005)
  l <- lrn("lce.parametric_exponential")
  l$predict_type <- "se"
  l$train(task)
  pred <- l$predict(task)
  expect_true(all(pred$se >= 0))
  expect_true(all(is.finite(pred$se)))
})

test_that("parametric exponential SE is NA when df <= 0", {
  set.seed(37)
  # Two batches, three parameters -> df = -1, Sigma cannot be computed.
  task <- make_curve(function(b) 0.5 + b * 0, n_batches = 2L)
  l <- lrn("lce.parametric_exponential")
  l$predict_type <- "se"
  l$train(task)
  pred <- l$predict(task)
  expect_true(all(is.na(pred$se)))
})

test_that("parametric power law recovers curve", {
  set.seed(32)
  task <- make_curve(function(b) 0.5 + 1.5 * b^(-0.6))
  l <- lrn("lce.parametric_power_law")
  l$predict_type <- "se"
  l$train(task)
  expect_equal(l$model$coefficients[["asymptote"]], 0.5, tolerance = 1e-3)
  expect_equal(l$model$coefficients[["amplitude"]], 1.5, tolerance = 1e-3)
  expect_equal(l$model$coefficients[["rate"]], 0.6, tolerance = 1e-3)
  pred <- l$predict(task)
  expect_true(all(pred$se >= 0))
})

test_that("parametric power law rejects non-positive batch numbers", {
  set.seed(33)
  task <- make_curve(function(b) 1 / b)
  dt <- task$data(cols = c(task$target_names, task$col_roles$feature,
    task$col_roles$archive_x, task$col_roles$archive_y))
  dt[, batch_nr := as.integer(batch_nr - 1L)]  # introduces a 0
  bad <- TaskLCE$new("bad", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  expect_error(lrn("lce.parametric_power_law")$train(bad), "positive batch numbers")
})

test_that("parametric logistic recovers a sigmoid curve", {
  set.seed(34)
  task <- make_curve(
    function(b) 0.1 + (0.9 - 0.1) / (1 + exp(-0.5 * (b - 5))),
    n_batches = 14L, noise = 0.0
  )
  l <- lrn("lce.parametric_logistic")
  l$train(task)
  expect_equal(l$model$coefficients[["lower"]], 0.1, tolerance = 5e-3)
  expect_equal(l$model$coefficients[["upper"]], 0.9, tolerance = 5e-3)
  expect_equal(l$model$coefficients[["midpoint"]], 5, tolerance = 0.2)
  expect_equal(l$model$coefficients[["rate"]], 0.5, tolerance = 0.05)
})

test_that("parametric log fits via OLS", {
  set.seed(35)
  task <- make_curve(function(b) 0.3 + 0.4 * log(b))
  l <- lrn("lce.parametric_log")
  l$predict_type <- "se"
  l$train(task)
  expect_equal(l$model$coefficients[["intercept"]], 0.3, tolerance = 1e-6)
  expect_equal(l$model$coefficients[["slope"]], 0.4, tolerance = 1e-6)
  pred <- l$predict(task)
  expect_true(all(is.finite(pred$se)))
})

test_that("lce_param_cov rejects an indefinite (non-PD) Hessian", {
  # a positive-definite Hessian yields a covariance; an indefinite but invertible
  # one must degrade to NULL (-> NA se) rather than a negative-variance matrix.
  pd <- lce_param_cov(matrix(c(2, 0, 0, 4), 2L), sse = 1, n_batches = 5L, n_pars = 2L)
  expect_false(is.null(pd$Sigma))
  expect_true(all(diag(pd$Sigma) > 0))
  indef <- lce_param_cov(matrix(c(1, 0, 0, -1), 2L), sse = 1, n_batches = 5L, n_pars = 2L)
  expect_null(indef$Sigma)
})

test_that("all parametric learners support predict_newdata", {
  set.seed(36)
  task <- make_curve(function(b) 0.9 - 0.7 * exp(-0.3 * b))
  newdata <- data.table(batch_nr = c(15L, 20L, 25L))
  for (id in c("lce.parametric_exponential", "lce.parametric_power_law",
      "lce.parametric_logistic", "lce.parametric_log")) {
    l <- lrn(id)
    l$train(task)
    pred <- l$predict_newdata(newdata)
    expect_length(pred$response, 3L)
    expect_true(all(is.finite(pred$response)))
  }
})
