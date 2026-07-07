# Curve task with a measure (so target_reached has a direction) and a link.
make_pt_task <- function(f = function(b) 0.05 + 0.5 * exp(-0.3 * b), n = 12L,
    per = 3L, link = "log", measure = msr("regr.mae")) {
  b <- seq_len(n)
  dt <- data.table(x1 = rnorm(n * per), x2 = rnorm(n * per), y = rnorm(n * per),
    batch_nr = as.integer(rep(b, each = per)), perf = rep(f(b), each = per))
  TaskLCE$new("pt", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure, link = link)
}

gaussian_ids <- c("lce.parametric_exponential", "lce.parametric_power_law",
  "lce.parametric_logistic", "lce.parametric_log", "lce.featureless",
  "lce.rolling_slope", "lce.isotonic")

test_that("Gaussian LCE learners declare the response/se/quantiles/target_reached types", {
  for (id in gaussian_ids) {
    expect_setequal(lrn(id)$predict_types,
      c("response", "se", "quantiles", "target_reached"))
  }
})

test_that("se predict type yields total se >= epistemic se across learners", {
  set.seed(1)
  task <- make_pt_task()
  for (id in gaussian_ids) {
    l <- lrn(id)
    l$predict_type <- "se"
    l$train(task)
    pred <- l$predict(task)
    ok <- is.finite(pred$se) & is.finite(pred$se_epistemic)
    expect_true(all(pred$se[ok] >= pred$se_epistemic[ok] - 1e-9), info = id)
    expect_true(all(pred$se[ok] >= 0), info = id)
    # se columns are reported as predict types
    expect_true(all(c("se", "se_epistemic") %in% pred$predict_types), info = id)
  }
})

test_that("rolling_slope epistemic se grows with extrapolation distance", {
  set.seed(2)
  task <- make_pt_task(f = function(b) 0.5 + 0.3 * b, link = "identity",
    measure = msr("regr.rsq"))
  l <- lrn("lce.rolling_slope", window = 6L)
  l$predict_type <- "se"
  l$train(task)
  pred <- l$predict_newdata(data.table(batch_nr = c(13L, 30L, 60L)))
  expect_true(all(diff(pred$se_epistemic) > 0))
})

test_that("target_reached returns per-target reach probabilities in [0, 1]", {
  set.seed(3)
  task <- make_pt_task()
  l <- lrn("lce.parametric_exponential", reach_target = c(0.1, 0.2))
  l$predict_type <- "target_reached"
  l$train(task)
  pred <- l$predict_newdata(data.table(batch_nr = 13:20))
  tr <- pred$target_reached
  expect_equal(dim(tr), c(8L, 2L))
  expect_equal(colnames(tr), c("reached0.1", "reached0.2"))
  expect_true(all(tr >= 0 & tr <= 1))
  # later batches are at least as likely to have reached a fixed (loss) target
  expect_true(all(diff(tr[, 1L]) >= -1e-9))
})

test_that("target_reached handles deterministic zero-SE forecasts", {
  set.seed(31)
  task <- make_pt_task(f = function(b) rep(0.2, length(b)), n = 1L,
    link = "identity", measure = msr("regr.mae"))
  l <- lrn("lce.featureless", type = "average", reach_target = 0.25)
  l$predict_type <- "target_reached"
  l$train(task)
  pred <- l$predict_newdata(data.table(batch_nr = 2:5))
  expect_false(anyNA(pred$target_reached))
  expect_equal(as.numeric(pred$target_reached), rep(1, 4L))
})

test_that("target_reached errors without a target or a direction", {
  set.seed(4)
  l <- lrn("lce.parametric_exponential")
  l$predict_type <- "target_reached"
  task_no_target <- make_pt_task()
  l$train(task_no_target)
  expect_error(l$predict(task_no_target), "reach_target")

  l2 <- lrn("lce.parametric_exponential", reach_target = 0.1)
  l2$predict_type <- "target_reached"
  task_no_measure <- make_pt_task(measure = NULL)
  l2$train(task_no_measure)
  expect_error(l2$predict(task_no_measure), "direction")
})

test_that("sample-based learners produce quantiles and samples", {
  set.seed(5)
  task <- make_pt_task()
  boot <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  boot$param_set$set_values(n_bootstrap = 40L, seed = 1L)
  expect_true(all(c("quantiles", "samples", "target_reached") %in% boot$predict_types))

  boot$predict_type <- "samples"
  boot$train(task)
  ps <- boot$predict(task)
  expect_equal(ncol(ps$samples), 40L)
  expect_equal(nrow(ps$samples), task$nrow)

  boot$predict_type <- "quantiles"
  boot$param_set$set_values(quantile_probs = c(0.1, 0.5, 0.9))
  pq <- boot$predict(task)
  expect_equal(attr(pq$quantiles, "probs"), c(0.1, 0.5, 0.9))
  # quantiles are sorted within each row
  expect_true(all(pq$quantiles[, 1L] <= pq$quantiles[, 2L] + 1e-9))
  expect_true(all(pq$quantiles[, 2L] <= pq$quantiles[, 3L] + 1e-9))
})

test_that("predicting se is independent of the train-time predict type", {
  # The covariance is always stored, so se is available even when trained with
  # the default response predict type and switched afterwards.
  set.seed(6)
  task <- make_pt_task()
  l <- lrn("lce.parametric_exponential")  # default predict_type "response"
  l$train(task)
  l$predict_type <- "se"
  pred <- l$predict(task)
  expect_true(all(is.finite(pred$se)))
  expect_true(all(is.finite(pred$se_epistemic)))
})
