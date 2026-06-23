make_baseline_task <- function(n_batches = 6L, per_batch = 3L, values = NULL,
    measure = NULL) {
  batches <- rep(seq_len(n_batches), each = per_batch)
  if (is.null(values)) values <- seq_len(n_batches) / n_batches
  dt <- data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(batches),
    perf = rep(values, each = per_batch)
  )
  TaskLCE$new("base", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure)
}

test_that("lce.featureless type = average predicts mean and sd", {
  set.seed(21)
  task <- make_baseline_task(n_batches = 5L, values = c(0.1, 0.2, 0.3, 0.4, 0.5))
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "average")
  l$predict_type <- "se"
  l$train(task)
  expect_equal(l$model$location, mean(c(0.1, 0.2, 0.3, 0.4, 0.5)))
  expect_equal(l$model$dispersion, sd(c(0.1, 0.2, 0.3, 0.4, 0.5)))
  pred <- l$predict(task)
  expect_true(all(pred$response == l$model$location))
  # total predictive se = sqrt(dispersion^2 + se_epistemic^2)
  expect_equal(l$model$se_total, sqrt(l$model$dispersion^2 + l$model$se_epi^2))
  expect_true(all(pred$se == l$model$se_total))
})

test_that("lce.featureless robust average uses median + MAD", {
  set.seed(22)
  task <- make_baseline_task(n_batches = 5L, values = c(0.1, 0.2, 0.3, 0.4, 5))
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "average", robust = TRUE)
  l$predict_type <- "se"
  l$train(task)
  expect_equal(l$model$location, median(c(0.1, 0.2, 0.3, 0.4, 5)))
  expect_equal(l$model$dispersion, mad(c(0.1, 0.2, 0.3, 0.4, 5)))
})

test_that("lce.featureless type = last predicts last training batch", {
  set.seed(23)
  task <- make_baseline_task(n_batches = 4L, values = c(0.1, 0.2, 0.3, 0.5))
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "last")
  l$predict_type <- "se"
  l$train(task)
  pred <- l$predict(task)
  expect_true(all(pred$response == 0.5))
  # total predictive se = dispersion of per-batch performances plus the standard
  # error of the constant (coarse but defined for all types)
  disp <- sd(c(0.1, 0.2, 0.3, 0.5))
  expect_equal(unique(pred$se), sqrt(disp^2 + (disp / sqrt(4))^2))
})

test_that("lce.featureless type = best uses max for maximize measures", {
  set.seed(26)
  # non-monotone curve: the global best (0.9) is not the most recent value
  task <- make_baseline_task(n_batches = 6L, values = c(0.1, 0.2, 0.9, 0.4, 0.3, 0.2),
    measure = msr("regr.rsq"))  # maximize
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "best")
  l$train(task)
  expect_equal(l$model$location, 0.9)  # max over ALL batches
})

test_that("lce.featureless type = best uses min for minimize measures", {
  set.seed(27)
  task <- make_baseline_task(n_batches = 6L, values = c(0.9, 0.8, 0.2, 0.6, 0.5, 0.4),
    measure = msr("regr.mae"))  # minimize
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "best")
  l$train(task)
  expect_equal(l$model$location, 0.2)  # min over ALL batches
})

test_that("lce.featureless type = best reads direction from the task measure", {
  set.seed(28)
  vals <- c(0.1, 0.2, 0.9, 0.4, 0.3, 0.2)
  task_max <- make_baseline_task(values = vals, measure = msr("regr.rsq"))
  task_min <- make_baseline_task(values = vals, measure = msr("regr.mae"))
  l <- lrn("lce.featureless")  # default type = "best"
  l$train(task_max)
  expect_equal(l$model$location, max(vals))
  l$train(task_min)
  expect_equal(l$model$location, min(vals))
})

test_that("lce.featureless type = best needs a task measure", {
  set.seed(29)
  task <- make_baseline_task(values = c(0.1, 0.2, 0.3))  # no measure
  l <- lrn("lce.featureless")  # default type = "best"
  expect_error(l$train(task), "needs the task to carry a measure")
})

test_that("link support is enforced at train time", {
  # the log link cannot represent a per-batch performance of 0; training must
  # error clearly rather than silently producing NaN standard errors.
  set.seed(33)
  dt <- data.table(x1 = rnorm(6), x2 = rnorm(6), y = rnorm(6),
    batch_nr = as.integer(1:6), perf = c(0.5, 0.4, 0, 0.2, 0.1, 0.05))
  task <- TaskLCE$new("supp", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(), link = "log")
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "average")
  expect_error(l$train(task), "support")
})

test_that("lce.featureless window restricts averaging to recent batches", {
  set.seed(30)
  task <- make_baseline_task(n_batches = 6L, values = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5))
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "average", window = 3L)
  l$predict_type <- "se"
  l$train(task)
  expect_equal(l$model$location, mean(c(0.3, 0.4, 0.5)))  # last 3 batches only
  expect_equal(l$model$dispersion, sd(c(0.3, 0.4, 0.5)))
})

test_that("lce.featureless window restricts the best search", {
  set.seed(31)
  # global best is the early 0.9; within the last 3 batches the best is 0.3
  task <- make_baseline_task(n_batches = 6L, values = c(0.1, 0.9, 0.2, 0.3, 0.3, 0.2),
    measure = msr("regr.rsq"))  # maximize
  l <- lrn("lce.featureless")
  l$param_set$set_values(type = "best", window = 3L)
  l$train(task)
  expect_equal(l$model$location, 0.3)
})

test_that("lce.featureless default window = Inf uses all batches", {
  set.seed(32)
  l <- lrn("lce.featureless")
  expect_equal(l$param_set$values$window, Inf)
})

test_that("lce.rolling_slope linearly extrapolates", {
  set.seed(24)
  task <- make_baseline_task(n_batches = 6L, values = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5))
  l <- lrn("lce.rolling_slope")
  l$param_set$set_values(window = 3L)
  l$train(task)
  expect_equal(l$model$slope, 0.1, tolerance = 1e-10)
  expect_equal(l$model$intercept, -0.1, tolerance = 1e-10)
  newdata <- data.table(batch_nr = c(7L, 8L, 9L))
  pred <- l$predict_newdata(newdata)
  expect_equal(pred$response, c(0.6, 0.7, 0.8), tolerance = 1e-10)
})

test_that("lce.rolling_slope window clamps to available batches", {
  set.seed(25)
  task <- make_baseline_task(n_batches = 3L, values = c(0.0, 0.2, 0.4))
  l <- lrn("lce.rolling_slope")
  l$param_set$set_values(window = 100L)
  l$train(task)
  expect_equal(l$model$window_used, 3L)
})
