# Build a TaskLCE with one row per batch and a known per-batch performance, plus
# an optional measure (for direction) and link.
make_distr_task <- function(truth, link = "identity", measure = msr("regr.mae"),
    per_batch = 1L) {
  n_batches <- length(truth)
  n <- n_batches * per_batch
  dt <- data.table(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n),
    batch_nr = as.integer(rep(seq_len(n_batches), each = per_batch)),
    perf = rep(truth, each = per_batch))
  TaskLCE$new("d", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(),
    measure = measure, link = link)
}

test_that("lce_gaussian_crps matches a numerical CRPS integral", {
  num_crps <- function(y, mu, sigma) {
    grid <- seq(mu - 12 * sigma, mu + 12 * sigma, length.out = 20001L)
    integrand <- (pnorm(grid, mu, sigma) - (grid >= y))^2
    sum((integrand[-1L] + integrand[-length(integrand)]) / 2 * diff(grid))
  }
  expect_equal(lce_gaussian_crps(0, 0, 1), num_crps(0, 0, 1), tolerance = 1e-3)
  expect_equal(lce_gaussian_crps(1.3, 0.5, 0.8), num_crps(1.3, 0.5, 0.8), tolerance = 1e-3)
  # degenerate predictive collapses to absolute error
  expect_equal(lce_gaussian_crps(c(1, 2), c(0.5, 2), c(0, 0)), c(0.5, 0))
})

test_that("lce_reach_prob handles scalar targets with zero sigma", {
  expect_equal(
    lce_reach_prob(0.5, mu = c(0.4, 0.6, 0.3), sigma = c(0, 0, 0),
      minimize = TRUE),
    c(1, 0, 1)
  )
  expect_equal(
    lce_reach_prob(0.5, mu = c(0.4, 0.6, 0.3), sigma = c(0, 0, 0),
      minimize = FALSE),
    c(0, 1, 0)
  )
})

test_that("lce.crps scores the Normal-on-link predictive (identity link)", {
  set.seed(1)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth)
  p <- PredictionLCE$new(task = task, response = truth, se = rep(0.1, 3L),
    se_epistemic = rep(0.05, 3L))
  const <- 2 * dnorm(0) - 1 / sqrt(pi)
  expect_equal(msr("lce.crps")$score(p, task = task), 0.1 * const, tolerance = 1e-10)
})

test_that("lce.crps is computed on the link scale", {
  set.seed(2)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth, link = "log")
  response <- c(0.28, 0.22, 0.12)
  se <- c(0.2, 0.15, 0.1)
  p <- PredictionLCE$new(task = task, response = response, se = se,
    se_epistemic = se / 2)
  expected <- mean(lce_gaussian_crps(log(truth), log(response), se))
  expect_equal(msr("lce.crps")$score(p, task = task), expected, tolerance = 1e-10)
})

test_that("lce.reach_brier scores the reach probability (minimize direction)", {
  set.seed(3)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth, measure = msr("regr.mae"))
  se <- rep(0.1, 3L)
  p <- PredictionLCE$new(task = task, response = truth, se = se, se_epistemic = se / 2)
  m <- msr("lce.reach_brier"); m$param_set$set_values(target = 0.15)
  reach <- pnorm((0.15 - truth) / se)        # minimize: reach when y <= tau
  reached <- as.numeric(truth <= 0.15)
  expect_equal(m$score(p, task = task), mean((reach - reached)^2), tolerance = 1e-10)
})

test_that("lce.reach_brier flips direction for a maximize measure", {
  set.seed(4)
  truth <- c(0.1, 0.4, 0.8)
  task <- make_distr_task(truth, measure = msr("regr.rsq"))  # maximize
  se <- rep(0.1, 3L)
  p <- PredictionLCE$new(task = task, response = truth, se = se, se_epistemic = se / 2)
  m <- msr("lce.reach_brier"); m$param_set$set_values(target = 0.5)
  reach <- pnorm((truth - 0.5) / se)         # maximize: reach when y >= tau
  reached <- as.numeric(truth >= 0.5)
  expect_equal(m$score(p, task = task), mean((reach - reached)^2), tolerance = 1e-10)
})

test_that("lce.reach_brier needs a target and a direction", {
  set.seed(5)
  truth <- c(0.3, 0.2, 0.1)
  task_m <- make_distr_task(truth, measure = msr("regr.mae"))
  p_m <- PredictionLCE$new(task = task_m, response = truth, se = rep(0.1, 3L),
    se_epistemic = rep(0.05, 3L))
  expect_error(msr("lce.reach_brier")$score(p_m, task = task_m), "target")

  task_nodir <- make_distr_task(truth, measure = NULL)
  p_nodir <- PredictionLCE$new(task = task_nodir, response = truth, se = rep(0.1, 3L),
    se_epistemic = rep(0.05, 3L))
  m <- msr("lce.reach_brier"); m$param_set$set_values(target = 0.15)
  expect_error(m$score(p_nodir, task = task_nodir), "measure")
})

test_that("lce.coverage and lce.interval_score score the central interval", {
  set.seed(6)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth)
  se <- rep(0.1, 3L)
  p <- PredictionLCE$new(task = task, response = truth, se = se, se_epistemic = se / 2)
  # truth == response, so it is always inside the interval
  expect_equal(msr("lce.coverage")$score(p, task = task), 1)
  z <- qnorm(0.95)
  expect_equal(msr("lce.interval_score")$score(p, task = task), 2 * z * 0.1,
    tolerance = 1e-10)
  # a far-off response leaves the truth outside, lowering coverage
  p_off <- PredictionLCE$new(task = task, response = truth + 1, se = se,
    se_epistemic = se / 2)
  expect_equal(msr("lce.coverage")$score(p_off, task = task), 0)
})

test_that("lce.interval_score is computed on the link scale", {
  set.seed(61)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth, link = "log")
  se <- rep(0.2, 3L)
  # truth == response, so every point is inside: the link-scale interval width is
  # 2 * z * se regardless of the link (the natural-scale width would differ).
  p <- PredictionLCE$new(task = task, response = truth, se = se, se_epistemic = se / 2)
  z <- qnorm(0.95)
  expect_equal(msr("lce.interval_score")$score(p, task = task), 2 * z * 0.2,
    tolerance = 1e-10)
})

test_that("lce.interval_score diverges to Inf (not NaN) at a boundary truth", {
  # the log link maps a per-batch performance of 0 to -Inf; the Winkler score
  # must be +Inf there (consistent with lce.crps), not NaN.
  set.seed(63)
  truth <- c(0.3, 0.2, 0)
  task <- make_distr_task(truth, link = "log")
  p <- PredictionLCE$new(task = task, response = c(0.3, 0.2, 0.1),
    se = rep(0.2, 3L), se_epistemic = rep(0.1, 3L))
  sc <- msr("lce.interval_score")$score(p, task = task)
  expect_true(is.infinite(sc) && sc > 0)
  expect_false(is.nan(sc))
  expect_true(is.infinite(msr("lce.crps")$score(p, task = task)))
})

test_that("se-based measures return NA (not an error) when se is NA", {
  set.seed(62)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth)
  se <- c(0.1, NA, 0.1)
  p <- PredictionLCE$new(task = task, response = truth, se = se, se_epistemic = se)
  # lce_gaussian_crps tolerates NA sigma elementwise
  expect_equal(lce_gaussian_crps(c(1, 2), c(0.5, 1), c(0.1, NA))[2L], NA_real_)
  # the measure aggregates to NA rather than crashing
  expect_true(is.na(msr("lce.crps")$score(p, task = task)))
})

test_that("lce.pinball scores the requested quantile column", {
  set.seed(7)
  truth <- c(0.3, 0.2, 0.1)
  task <- make_distr_task(truth)
  probs <- c(0.1, 0.5, 0.9)
  q <- cbind(truth - 0.1, c(0.25, 0.25, 0.05), truth + 0.1)
  setattr(q, "probs", probs)
  p <- PredictionLCE$new(task = task, response = c(0.25, 0.25, 0.05), quantiles = q)
  m <- msr("lce.pinball"); m$param_set$set_values(alpha = 0.5)
  d <- truth - c(0.25, 0.25, 0.05)
  expect_equal(m$score(p, task = task), mean(pmax(0.5 * d, -0.5 * d)), tolerance = 1e-10)
  # alpha must be among the predicted probabilities
  m2 <- msr("lce.pinball"); m2$param_set$set_values(alpha = 0.3)
  expect_error(m2$score(p, task = task), "alpha")
})

test_that("distributional measures aggregate per batch and honour weights", {
  set.seed(8)
  # two rows per batch, identical within batch -> per-batch aggregation
  task <- make_distr_task(c(0.3, 0.2, 0.1), per_batch = 2L)
  response <- task$truth()
  const <- 2 * dnorm(0) - 1 / sqrt(pi)
  # batch-varying se so the weighted and unweighted means differ
  se <- rep(c(0.1, 0.2, 0.3), each = 2L)
  p <- PredictionLCE$new(task = task, response = response, se = se,
    se_epistemic = se / 2)
  expect_equal(msr("lce.crps")$score(p, task = task), mean(c(0.1, 0.2, 0.3)) * const,
    tolerance = 1e-10)

  # weighted: per-batch weight is the sum of its row weights -> c(2, 4, 10)
  w <- c(1, 1, 2, 2, 5, 5)
  pw <- PredictionLCE$new(task = task, response = response, se = se,
    se_epistemic = se / 2, weights = w)
  batch_w <- c(2, 4, 10)
  expect_equal(msr("lce.crps")$score(pw, task = task),
    sum(batch_w * c(0.1, 0.2, 0.3) * const) / sum(batch_w), tolerance = 1e-10)
})
