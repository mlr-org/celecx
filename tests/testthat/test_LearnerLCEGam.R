make_gam_task <- function(values, per_batch = 2L, measure = msr("regr.mae"),
    link = "identity") {
  n_batches <- length(values)
  n <- n_batches * per_batch
  dt <- data.table(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = rnorm(n),
    batch_nr = as.integer(rep(seq_len(n_batches), each = per_batch)),
    perf = rep(values, each = per_batch)
  )
  TaskLCE$new("gam", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure, link = link)
}

test_that("lce.gam is registered and fits a smooth curve", {
  set.seed(11)
  values <- 0.05 + 0.9 * exp(-0.25 * (1:10))
  task <- make_gam_task(values)
  learner <- lrn("lce.gam")
  expect_r6(learner, "LearnerLCEGam")

  learner$train(task)
  pred_in <- learner$predict(task)
  expect_lt(max(abs(pred_in$response - rep(values, each = 2L))), 0.05)

  pred_out <- learner$predict_newdata(data.table(batch_nr = 11:20L))
  expect_true(all(is.finite(pred_out$response)))
})

test_that("lce.gam se predictions carry epistemic and total components", {
  set.seed(12)
  values <- 0.05 + 0.9 * exp(-0.25 * (1:12))
  task <- make_gam_task(values)
  learner <- lrn("lce.gam", predict_type = "se")
  learner$train(task)

  pred <- learner$predict_newdata(data.table(batch_nr = c(13L, 30L)))
  expect_true(all(is.finite(pred$se)))
  expect_true(all(pred$se >= pred$se_epistemic))
  # extrapolation uncertainty grows with distance from the data
  expect_gt(pred$se_epistemic[[2L]], pred$se_epistemic[[1L]])
})

test_that("lce.gam respects the log link", {
  set.seed(13)
  values <- 0.05 + 0.9 * exp(-0.25 * (1:10))
  task <- make_gam_task(values, link = "log")
  learner <- lrn("lce.gam", predict_type = "se")
  learner$train(task)
  pred <- learner$predict_newdata(data.table(batch_nr = 11:40L))
  expect_true(all(pred$response > 0))
})

test_that("lce.gam needs at least four distinct batches", {
  set.seed(14)
  task <- make_gam_task(c(0.5, 0.4, 0.3))
  expect_error(lrn("lce.gam")$train(task), "four")
})

test_that("lce.gam clamps k to the number of batches", {
  set.seed(15)
  task <- make_gam_task(c(0.5, 0.4, 0.35, 0.32, 0.3))
  learner <- lrn("lce.gam", k = 10L)
  learner$train(task)
  pred <- learner$predict(task)
  expect_true(all(is.finite(pred$response)))
})

test_that("lce.gam quantiles use the configured probabilities", {
  set.seed(16)
  values <- 0.05 + 0.9 * exp(-0.25 * (1:10))
  task <- make_gam_task(values)
  learner <- lrn("lce.gam", predict_type = "quantiles")
  learner$param_set$set_values(quantile_probs = c(0.1, 0.5, 0.9))
  learner$train(task)
  pred <- learner$predict_newdata(data.table(batch_nr = 11:15L))
  expect_equal(dim(pred$quantiles), c(5L, 3L))
  expect_equal(attr(pred$quantiles, "probs"), c(0.1, 0.5, 0.9))
  expect_true(all(diff(t(pred$quantiles)) >= 0))
})
