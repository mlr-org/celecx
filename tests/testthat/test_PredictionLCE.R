make_pred_task <- function(n_batches = 4L, per_batch = 2L, link = "identity") {
  n <- n_batches * per_batch
  dt <- data.table(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n),
    batch_nr = as.integer(rep(seq_len(n_batches), each = per_batch)),
    perf = rep(seq_len(n_batches) / n_batches, each = per_batch))
  TaskLCE$new("p", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = lce_search_space(), codomain = lce_codomain(), link = link)
}

mk_quantiles <- function(n, probs) {
  q <- t(replicate(n, sort(stats::rnorm(length(probs)))))
  q <- matrix(q, nrow = n, ncol = length(probs))
  setattr(q, "probs", probs)
  q
}

test_that("PredictionLCE carries se, se_epistemic and reports predict types", {
  set.seed(1)
  task <- make_pred_task()
  n <- task$nrow
  p <- PredictionLCE$new(task = task, response = rep(0.3, n), se = rep(0.2, n),
    se_epistemic = rep(0.1, n))
  expect_setequal(p$predict_types, c("response", "se", "se_epistemic"))
  expect_equal(p$se, rep(0.2, n))
  expect_equal(p$se_epistemic, rep(0.1, n))
  # absent columns surface as NA via the active bindings
  p_resp <- PredictionLCE$new(task = task, response = rep(0.3, n))
  expect_true(all(is.na(p_resp$se)))
  expect_true(all(is.na(p_resp$se_epistemic)))
})

test_that("PredictionLCE carries quantiles / samples / target_reached matrices", {
  set.seed(2)
  task <- make_pred_task()
  n <- task$nrow
  q <- mk_quantiles(n, c(0.1, 0.5, 0.9))
  s <- matrix(rnorm(n * 5), ncol = 5)
  tr <- matrix(runif(n * 2), ncol = 2)
  setattr(tr, "target", c(0.2, 0.4))
  p <- PredictionLCE$new(task = task, response = rep(0.3, n), quantiles = q,
    samples = s, target_reached = tr)
  expect_setequal(p$predict_types, c("response", "quantiles", "samples", "target_reached"))
  expect_equal(colnames(p$quantiles), c("q0.1", "q0.5", "q0.9"))
  expect_equal(colnames(p$samples), sprintf("s%i", 1:5))
  expect_equal(colnames(p$target_reached), c("reached0.2", "reached0.4"))
  tab <- as.data.table(p)
  expect_true(all(c("q0.1", "q0.5", "q0.9", "s1", "reached0.2") %in% names(tab)))
})

test_that("check rejects quantiles without a probs attribute or wrong length", {
  set.seed(3)
  task <- make_pred_task()
  n <- task$nrow
  bad <- matrix(rnorm(n * 3), ncol = 3)  # no probs attr
  expect_error(PredictionLCE$new(task = task, response = rep(0.3, n), quantiles = bad),
    "probs attribute")
  short <- mk_quantiles(n - 1L, c(0.1, 0.5, 0.9))
  expect_error(PredictionLCE$new(task = task, response = rep(0.3, n), quantiles = short),
    "quantiles")
})

test_that("is_missing detects NA in any payload including matrices", {
  set.seed(4)
  task <- make_pred_task()
  n <- task$nrow
  s <- matrix(rnorm(n * 3), ncol = 3)
  s[2L, 1L] <- NA_real_
  p <- PredictionLCE$new(task = task, response = rep(0.3, n), samples = s)
  miss <- is_missing_prediction_data(p$data)
  expect_true(p$row_ids[2L] %in% miss)
  expect_false(p$row_ids[1L] %in% miss)
})

test_that("filter preserves rows and matrix attributes", {
  set.seed(5)
  task <- make_pred_task()
  n <- task$nrow
  q <- mk_quantiles(n, c(0.25, 0.75))
  p <- PredictionLCE$new(task = task, response = rep(0.3, n), se = rep(0.1, n), quantiles = q)
  keep_ids <- p$row_ids[c(1L, 3L)]
  filtered <- filter_prediction_data(p$data, keep_ids)
  expect_equal(length(filtered$row_ids), 2L)
  expect_equal(nrow(filtered$quantiles), 2L)
  expect_equal(attr(filtered$quantiles, "probs"), c(0.25, 0.75))
  # the filtered data still passes the checker (attributes intact)
  expect_silent(check_prediction_data(filtered))
})

test_that("c() row-binds predictions and preserves matrix attributes", {
  set.seed(6)
  task <- make_pred_task()
  n <- task$nrow
  half <- n %/% 2L
  q1 <- mk_quantiles(half, c(0.1, 0.9))
  q2 <- mk_quantiles(n - half, c(0.1, 0.9))
  p1 <- PredictionLCE$new(task = task, row_ids = task$row_ids[seq_len(half)],
    truth = task$truth(task$row_ids[seq_len(half)]),
    response = rep(0.3, half), quantiles = q1)
  p2 <- PredictionLCE$new(task = task, row_ids = task$row_ids[(half + 1L):n],
    truth = task$truth(task$row_ids[(half + 1L):n]),
    response = rep(0.3, n - half), quantiles = q2)
  comb <- c(p1$data, p2$data)
  expect_equal(length(comb$row_ids), n)
  expect_equal(nrow(comb$quantiles), n)
  expect_equal(attr(comb$quantiles, "probs"), c(0.1, 0.9))
})

test_that("c() row-binds samples predictions (no probs/target attribute)", {
  # regression: bind_matrix read attr(x, NULL) unconditionally and crashed for
  # the samples matrix, which carries no attribute.
  set.seed(8)
  task <- make_pred_task()
  n <- task$nrow
  half <- n %/% 2L
  s1 <- matrix(rnorm(half * 4L), ncol = 4L)
  s2 <- matrix(rnorm((n - half) * 4L), ncol = 4L)
  p1 <- PredictionLCE$new(task = task, row_ids = task$row_ids[seq_len(half)],
    truth = task$truth(task$row_ids[seq_len(half)]),
    response = rep(0.3, half), samples = s1)
  p2 <- PredictionLCE$new(task = task, row_ids = task$row_ids[(half + 1L):n],
    truth = task$truth(task$row_ids[(half + 1L):n]),
    response = rep(0.3, n - half), samples = s2)
  comb <- c(p1$data, p2$data)
  expect_equal(length(comb$row_ids), n)
  expect_equal(dim(comb$samples), c(n, 4L))
})

test_that("c() rejects predictions with differing predict types", {
  set.seed(7)
  task <- make_pred_task()
  n <- task$nrow
  p_resp <- PredictionLCE$new(task = task, response = rep(0.3, n))
  p_se <- PredictionLCE$new(task = task, response = rep(0.3, n), se = rep(0.1, n))
  expect_error(c(p_resp$data, p_se$data), "differing predict types")
})
