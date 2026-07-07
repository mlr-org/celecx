# Slowly-decaying loss curve so the target is reached partway through the grid.
make_b2t_task <- function(n = 12L, per = 3L, link = "log", measure = msr("regr.mae")) {
  b <- seq_len(n)
  perf <- 0.05 + 0.9 * exp(-0.15 * b)
  dt <- data.table(x1 = rnorm(n * per), x2 = rnorm(n * per), y = rnorm(n * per),
    batch_nr = as.integer(rep(b, each = per)), perf = rep(perf, each = per))
  TaskLCE$new("b2t", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure, link = link)
}

test_that("expected crossing returns a monotone CDF, quantiles and p_never", {
  set.seed(1)
  task <- make_b2t_task()
  l <- lrn("lce.parametric_exponential")
  l$train(task)
  res <- lce_batches_to_target(l, batch_grid = 13:80, target = 0.15,
    crossing = "expected", probs = c(0.1, 0.5, 0.9))
  expect_named(res, c("quantiles", "quantiles_remaining", "last_trained_batch",
    "p_never", "grid"))
  expect_true(all(diff(res$grid$cdf) >= -1e-12))  # non-decreasing
  expect_true(res$p_never >= 0 && res$p_never <= 1)
  # crossing quantiles are ordered and lie on the grid (or NA)
  qv <- res$quantiles
  finite <- qv[is.finite(qv)]
  expect_true(all(diff(finite) >= 0))
  expect_true(all(finite %in% 13:80))
  # remaining batches are the crossing quantiles counted from the last trained batch
  expect_equal(res$last_trained_batch, 12)
  expect_equal(res$quantiles_remaining, res$quantiles - 12)
})

test_that("observed crossing uses the joint sample paths", {
  set.seed(2)
  task <- make_b2t_task()
  boot <- lrn("lce.bootstrap", learner = lrn("lce.parametric_exponential"))
  boot$param_set$set_values(n_bootstrap = 80L, seed = 1L)
  boot$train(task)
  obs_res <- lce_batches_to_target(boot, batch_grid = 13:80, target = 0.15,
    crossing = "observed")
  expect_named(obs_res, c("quantiles", "quantiles_remaining", "last_trained_batch",
    "p_never", "grid"))
  expect_true(all(diff(obs_res$grid$cdf) >= -1e-12))  # cumulative reach fraction
  expect_true(obs_res$p_never >= 0 && obs_res$p_never <= 1)
  qv <- obs_res$quantiles
  expect_true(all(qv[is.finite(qv)] %in% 13:80))
  # the learner's predict type is restored after the transform
  expect_equal(boot$predict_type, "response")
})

test_that("observed crossing errors for a learner without samples", {
  set.seed(3)
  task <- make_b2t_task()
  l <- lrn("lce.parametric_exponential")
  l$train(task)
  expect_error(
    lce_batches_to_target(l, batch_grid = 13:40, target = 0.15, crossing = "observed"),
    "samples"
  )
})

test_that("lce_batches_to_target needs a trained learner and a direction", {
  set.seed(4)
  l <- lrn("lce.parametric_exponential")
  expect_error(lce_batches_to_target(l, 13:40, target = 0.15), "trained")

  task_no_measure <- make_b2t_task(measure = NULL)
  l$train(task_no_measure)
  expect_error(lce_batches_to_target(l, 13:40, target = 0.15), "direction")
})

test_that("p_never is high when the target is unreachable within the grid", {
  set.seed(5)
  task <- make_b2t_task()
  l <- lrn("lce.parametric_exponential")
  l$train(task)
  # asymptote is ~0.05, so a target below it is essentially never reached
  res <- lce_batches_to_target(l, batch_grid = 13:80, target = 0.001,
    crossing = "expected", probs = 0.5)
  expect_gt(res$p_never, 0.5)
  expect_true(is.na(res$quantiles[["q0.5"]]))
})
