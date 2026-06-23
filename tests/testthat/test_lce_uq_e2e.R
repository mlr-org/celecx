# End-to-end exercises of the uncertainty / distributional stack: real predict
# -> per-batch aggregation -> distributional scoring, including resampling
# (which row-binds per-fold predictions, the matrix-column code path).

make_e2e_task <- function(n = 14L, per = 3L, link = "log", measure = msr("regr.mae"),
    noise = 0.01) {
  b <- seq_len(n)
  perf <- 0.05 + 0.6 * exp(-0.25 * b)
  set.seed(0)
  perf <- pmax(perf + stats::rnorm(n, 0, noise), 1e-3)
  dt <- data.table(x1 = rnorm(n * per), x2 = rnorm(n * per), y = rnorm(n * per),
    batch_nr = as.integer(rep(b, each = per)), perf = rep(perf, each = per))
  TaskLCE$new("e2e", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure, link = link)
}

test_that("se-based measures aggregate over a resampling (log link)", {
  set.seed(1)
  task <- make_e2e_task(link = "log")
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 8L, step_size = 1L)
  measures <- list(msr("lce.crps"), msr("lce.coverage"), msr("lce.interval_score"),
    msr("lce.rmse"))
  for (id in c("lce.parametric_exponential", "lce.rolling_slope", "lce.featureless")) {
    l <- lrn(id)
    l$predict_type <- "se"
    rr <- suppressMessages(resample(task, l, rs))
    agg <- rr$aggregate(measures)
    expect_true(all(is.finite(agg)), info = id)
    expect_true(agg[["lce.crps"]] >= 0, info = id)
    expect_true(agg[["lce.coverage"]] >= 0 && agg[["lce.coverage"]] <= 1, info = id)
  }
})

test_that("reach_brier scores a full resampling and lies in [0, 1]", {
  set.seed(2)
  task <- make_e2e_task()
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 8L, step_size = 1L)
  l <- lrn("lce.parametric_exponential")
  l$predict_type <- "se"
  rr <- suppressMessages(resample(task, l, rs))
  m <- msr("lce.reach_brier")
  m$param_set$set_values(target = 0.1)
  agg <- rr$aggregate(m)
  expect_true(is.finite(agg))
  expect_true(agg >= 0 && agg <= 1)
})

test_that("pinball scores a quantiles learner across resampling folds", {
  # exercises the matrix-column rbind across folds (c.PredictionDataLCE)
  set.seed(3)
  task <- make_e2e_task()
  rs <- rsmp("lce.expanding_cv", horizon = 2L, min_train_batches = 8L, step_size = 2L)
  boot <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  boot$param_set$set_values(n_bootstrap = 30L, seed = 1L,
    quantile_probs = c(0.1, 0.5, 0.9))
  boot$predict_type <- "quantiles"
  rr <- suppressMessages(resample(task, boot, rs))
  m <- msr("lce.pinball")
  m$param_set$set_values(alpha = 0.5)
  agg <- rr$aggregate(m)
  expect_true(is.finite(agg))
  expect_true(agg >= 0)
})

test_that("a benchmark across learners and distributional measures runs", {
  set.seed(4)
  task <- make_e2e_task()
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 9L, step_size = 1L)
  learners <- lapply(c("lce.parametric_exponential", "lce.parametric_power_law",
    "lce.rolling_slope"), function(id) {
    l <- lrn(id)
    l$predict_type <- "se"
    l
  })
  design <- benchmark_grid(task, learners, rs)
  bmr <- suppressMessages(benchmark(design))
  scores <- bmr$aggregate(list(msr("lce.crps"), msr("lce.mae")))
  expect_equal(nrow(scores), 3L)
  expect_true(all(is.finite(scores$lce.crps)))
})

test_that("the full stack runs end-to-end from a replayed run", {
  skip_if_not_installed("mlr3learners")
  set.seed(5)
  # build an archive via a small active-learning run, then replay it to a TaskLCE
  ss <- ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 1))
  codomain <- Codomain$new(ps(y = p_dbl(tags = "minimize"))$domains)
  archive <- ArchiveBatch$new(search_space = ss, codomain = codomain)
  n <- 30L
  xdt <- data.table(x1 = runif(n), x2 = runif(n))
  ydt <- data.table(y = (xdt$x1 - 0.5)^2 + (xdt$x2 - 0.5)^2 + rnorm(n, 0, 0.01))
  for (b in seq_len(10L)) {
    rows <- ((b - 1L) * 3L + 1L):(b * 3L)
    abatch <- ArchiveBatch$new(search_space = ss, codomain = codomain, check_values = FALSE)
    archive$add_evals(xdt = xdt[rows], xss_trafoed = NULL, ydt = ydt[rows])
  }
  test_dt <- data.table(x1 = runif(40L), x2 = runif(40L))
  test_dt[, y := (x1 - 0.5)^2 + (x2 - 0.5)^2]
  test_task <- TaskRegr$new("test", test_dt, target = "y")

  task <- replay_surrogate_performance(archive, lrn("regr.lm"), test_task,
    measures = list(mae = msr("regr.mae")), measure = "mae", link = "log")
  expect_r6(task, "TaskLCE")
  expect_equal(task$link, "log")

  l <- lrn("lce.parametric_exponential")
  l$predict_type <- "se"
  l$train(task)
  pred <- l$predict(task)
  expect_true(all(is.finite(pred$se)))
  expect_true(msr("lce.crps")$score(pred, task = task) >= 0)

  # decision-support forecast on the trained learner
  res <- lce_batches_to_target(l, batch_grid = 11:60,
    target = task$truth()[length(task$truth())] * 0.9, crossing = "expected")
  expect_true(res$p_never >= 0 && res$p_never <= 1)
})
