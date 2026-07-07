make_wrapper_task <- function(n_batches = 10L, per_batch = 3L, noise = 0.01) {
  batches <- seq_len(n_batches)
  perf <- 0.9 - 0.7 * exp(-0.3 * batches) + rnorm(n_batches, 0, noise)
  dt <- data.table(
    x1 = rnorm(n_batches * per_batch),
    x2 = rnorm(n_batches * per_batch),
    y = rnorm(n_batches * per_batch),
    batch_nr = as.integer(rep(batches, each = per_batch)),
    perf = rep(perf, each = per_batch)
  )
  TaskLCE$new("w", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
}

test_that("lce.bootstrap merges base learner's param_set", {
  base <- lrn("lce.parametric_exponential")
  wrapper <- lrn("lce.bootstrap", learner = base)
  ids <- wrapper$param_set$ids()
  expect_true(all(c("n_bootstrap", "seed") %in% ids))
  expect_true(any(grepl("rate_lower", ids)))
})

test_that("lce.bootstrap with parametric_exponential reproduces the curve", {
  set.seed(51)
  task <- make_wrapper_task()
  w <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  w$param_set$set_values(n_bootstrap = 30L, seed = 1L)
  w$predict_type <- "se"
  w$train(task)
  expect_length(w$model$bootstrap_states, 30L)
  pred <- w$predict(task)
  expect_true(all(is.finite(pred$response)))
  expect_true(all(pred$se >= 0))
  expect_true(all(is.finite(pred$se)))
  # Bootstrap-averaged response should be close to the base learner's fit.
  base <- lrn("lce.parametric_exponential")
  base$train(task)
  expect_lt(mean(abs(pred$response - base$predict(task)$response)), 0.05)
})

test_that("lce.bootstrap is reproducible with a fixed seed", {
  set.seed(52)
  task <- make_wrapper_task()
  w1 <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  w1$param_set$set_values(n_bootstrap = 10L, seed = 42L)
  w1$predict_type <- "se"
  w1$train(task)
  p1 <- w1$predict(task)

  w2 <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  w2$param_set$set_values(n_bootstrap = 10L, seed = 42L)
  w2$predict_type <- "se"
  w2$train(task)
  p2 <- w2$predict(task)

  expect_equal(p1$response, p2$response)
  expect_equal(p1$se, p2$se)
})

test_that("lce.bootstrap rejects non-LCE base learners", {
  expect_error(LearnerLCEBootstrap$new(lrn("regr.featureless")), "LearnerLCE")
})

test_that("lce.conformal trains, predicts, and exposes alpha", {
  set.seed(53)
  task <- make_wrapper_task(n_batches = 12L)
  w <- lrn("lce.conformal", learner = lrn("lce.parametric_exponential"))
  w$param_set$set_values(n_calibration_batches = 3L, alpha = 0.1)
  w$predict_type <- "se"
  w$train(task)
  expect_equal(w$model$alpha, 0.1)
  expect_equal(w$model$n_cal, 3L)
  expect_true(w$model$q >= 0)
  pred <- w$predict(task)
  expect_true(all(is.finite(pred$response)))
  expect_true(all(pred$se == pred$se[1L]))  # constant per row
})

test_that("lce.conformal errors when calibration size too large", {
  set.seed(54)
  task <- make_wrapper_task(n_batches = 4L)
  w <- lrn("lce.conformal", learner = lrn("lce.parametric_exponential"))
  w$param_set$set_values(n_calibration_batches = 5L)
  expect_error(w$train(task), "at least")
})

test_that("lce.conformal merges base learner's param_set", {
  base <- lrn("lce.parametric_exponential")
  wrapper <- lrn("lce.conformal", learner = base)
  ids <- wrapper$param_set$ids()
  expect_true(all(c("n_calibration_batches", "alpha") %in% ids))
  expect_true(any(grepl("rate_lower", ids)))
})

test_that("lce.bootstrap forwards the task measure to its replicate tasks", {
  # regression: replicate tasks used to drop `measure`, breaking base learners
  # that need the optimization direction (e.g. featureless type = "best").
  set.seed(60)
  batches <- seq_len(10L)
  perf <- 0.9 - 0.7 * exp(-0.3 * batches)
  dt <- data.table(x1 = rnorm(30), x2 = rnorm(30), y = rnorm(30),
    batch_nr = as.integer(rep(batches, each = 3L)), perf = rep(perf, each = 3L))
  task <- TaskLCE$new("wm", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = msr("regr.mae"))
  w <- lrn("lce.bootstrap", learner = lrn("lce.featureless"))  # default type = "best"
  w$param_set$set_values(n_bootstrap = 10L, seed = 1L)
  expect_silent(w$train(task))
  expect_length(w$model$bootstrap_states, 10L)
})

test_that("lce.bootstrap se is the total predictive SD (epistemic + aleatoric)", {
  set.seed(61)
  batches <- seq_len(12L)
  perf <- 0.05 + 0.6 * exp(-0.25 * batches) + rnorm(12L, 0, 0.05)
  dt <- data.table(x1 = rnorm(36), x2 = rnorm(36), y = rnorm(36),
    batch_nr = as.integer(rep(batches, each = 3L)), perf = rep(perf, each = 3L))
  task <- TaskLCE$new("bse", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain())
  w <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  w$param_set$set_values(n_bootstrap = 200L, seed = 1L)
  w$predict_type <- "se"
  w$train(task)
  pred <- w$predict(task)
  # total se strictly exceeds the epistemic se because the aleatoric residual
  # variance is added back; both are finite and non-negative.
  expect_true(all(pred$se > pred$se_epistemic + 1e-9))
  expect_true(all(is.finite(pred$se)) && all(pred$se_epistemic >= 0))
  expect_equal(pred$se, sqrt(pred$se_epistemic^2 + w$model$sigma2), tolerance = 1e-9)
})

test_that("lce.bootstrap on a log link stays in support (no NaN)", {
  # regression: natural-scale residual resampling could push synthetic targets
  # non-positive and break log(.).
  set.seed(62)
  batches <- seq_len(12L)
  perf <- pmax(0.05 + 0.6 * exp(-0.25 * batches) + rnorm(12L, 0, 0.05), 1e-3)
  dt <- data.table(x1 = rnorm(36), x2 = rnorm(36), y = rnorm(36),
    batch_nr = as.integer(rep(batches, each = 3L)), perf = rep(perf, each = 3L))
  task <- TaskLCE$new("blog", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(), link = "log")
  w <- lrn("lce.bootstrap", learner = lrn("lce.rolling_slope"))
  w$param_set$set_values(n_bootstrap = 50L, seed = 1L)
  w$predict_type <- "se"
  w$train(task)
  pred <- w$predict_newdata(data.table(batch_nr = 13:25L))
  expect_true(all(is.finite(pred$response)))
  expect_true(all(is.finite(pred$se)))
})

test_that("lce.conformal supports the target_reached predict type", {
  set.seed(63)
  batches <- seq_len(12L)
  perf <- 0.05 + 0.6 * exp(-0.25 * batches)
  dt <- data.table(x1 = rnorm(36), x2 = rnorm(36), y = rnorm(36),
    batch_nr = as.integer(rep(batches, each = 3L)), perf = rep(perf, each = 3L))
  task <- TaskLCE$new("ctr", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = msr("regr.mae"))
  w <- lrn("lce.conformal", learner = lrn("lce.parametric_exponential"))
  w$param_set$set_values(n_calibration_batches = 3L, alpha = 0.1,
    reach_target = c(0.1, 0.3))
  expect_true("target_reached" %in% w$predict_types)
  w$predict_type <- "target_reached"
  w$train(task)
  pred <- w$predict_newdata(data.table(batch_nr = 13:20L))
  tr <- pred$target_reached
  expect_equal(dim(tr), c(8L, 2L))
  expect_true(all(tr >= 0 & tr <= 1))
})

test_that("wrappers work inside resample()", {
  set.seed(55)
  task <- make_wrapper_task(n_batches = 10L)
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 6L,
    step_size = 1L)

  for (wrap_id in c("lce.bootstrap", "lce.conformal")) {
    wrapper <- lrn(wrap_id, learner = lrn("lce.parametric_exponential"))
    if (wrap_id == "lce.bootstrap") {
      wrapper$param_set$set_values(n_bootstrap = 10L, seed = 1L)
    }
    if (wrap_id == "lce.conformal") {
      wrapper$param_set$set_values(n_calibration_batches = 2L, alpha = 0.1)
    }
    rr <- suppressMessages(resample(task, wrapper, rs))
    agg <- rr$aggregate(msr("lce.rmse"))
    expect_true(is.finite(agg))
    expect_true(agg >= 0)
  }
})

test_that("deep-cloning a trained wrapper does not alias state or the original param_set", {
  set.seed(51)
  n <- 15L
  dt <- data.table(x1 = runif(n), x2 = runif(n), y = rnorm(n),
    batch_nr = rep(1:5, each = 3L),
    perf = rep(c(0.5, 0.4, 0.33, 0.28, 0.25), each = 3L))
  task <- TaskLCE$new("w", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y", measure = msr("regr.mae"))

  l <- LearnerLCEBootstrap$new(lrn("lce.parametric_exponential"))
  l$param_set$set_values(n_bootstrap = 5L, seed = 1L)
  ps_ref <- l$param_set
  l$train(task)

  clone <- l$clone(deep = TRUE)
  # mlr3 Learner deep_clone contract: train_task and log must not be aliased
  expect_false(identical(clone$state$train_task, l$state$train_task))
  # member states are plain immutable data and may be shared between clones;
  # both objects must remain fully functional and agree
  expect_equal(clone$predict(task)$response, l$predict(task)$response)
  # cloning must not detach a previously obtained param_set of the ORIGINAL
  expect_identical(l$param_set, ps_ref)
  # base params stay reachable under the base. prefix on both objects
  expect_true("base.rate_lower" %in% clone$param_set$ids())
})

test_that("LCE wrappers store bare learner states and reset the shared base learner", {
  set.seed(56)
  task <- make_wrapper_task(n_batches = 12L)

  w <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  w$param_set$set_values(n_bootstrap = 5L, seed = 1L)
  w$predict_type <- "se"
  w$train(task)
  expect_true(every(w$model$bootstrap_states, inherits, "learner_state"))
  expect_false(some(w$model$bootstrap_states, inherits, "R6"))
  expect_null(w$wrapped$state)
  w$predict(task)
  expect_null(w$wrapped$state)
  expect_equal(w$wrapped$predict_type, "response")

  wc <- lrn("lce.conformal", learner = lrn("lce.parametric_exponential"))
  wc$param_set$set_values(n_calibration_batches = 3L)
  wc$train(task)
  expect_s3_class(wc$model$base_state, "learner_state")
  expect_false(inherits(wc$model$base_state, "R6"))
  expect_null(wc$wrapped$state)
  wc$predict(task)
  expect_null(wc$wrapped$state)
})
