# Build a TaskLCE whose archive features lie in [0, 1]^2 so the (bounded or
# unbounded) search space and the oracle agree. The performance target is an
# exponential-ish curve; the simulate learner ignores it except for the
# in-sample recorded curve, and uses the stored `measure` to score the surrogate.
make_sim_task <- function(n_batches = 8L, per_batch = 2L,
    search_space = lce_search_space_bounded(), pool = NULL) {
  batches <- seq_len(n_batches)
  perf <- 0.9 - 0.6 * exp(-0.3 * batches)
  n <- n_batches * per_batch
  dt <- data.table(
    x1 = runif(n),
    x2 = runif(n),
    y = NA_real_,
    batch_nr = as.integer(rep(batches, each = per_batch)),
    perf = rep(perf, each = per_batch)
  )
  dt[, y := sin(3 * x1) + x2^2]
  TaskLCE$new("sim", dt, target = "perf", batch_nr = "batch_nr",
    archive_x = c("x1", "x2"), archive_y = "y",
    search_space = search_space, codomain = lce_codomain(),
    measure = msr("regr.mae"), pool = pool)
}

# A small, fast active-learning optimizer with a "uncertainty" surrogate that has
# native se (regr.lm), avoiding the slower bootstrap-SE wrapper.
make_sim_optimizer <- function(batch_size = 1L) {
  optimizer_active_learning(lrn("regr.lm"), batch_size = batch_size, acq_evals = 8L)
}

test_that("lce.simulate constructs and exposes its configuration", {
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  expect_r6(l, "LearnerLCE")
  expect_equal(l$id, "lce.simulate")
  expect_setequal(l$predict_types,
    c("response", "se", "quantiles", "samples", "target_reached"))
  expect_true(all(c("n_eval_points", "n_restarts", "extrapolation", "seed") %in%
    l$param_set$ids()))
  expect_equal(l$surrogate_id, "uncertainty")
  expect_r6(l$optimizer, "OptimizerAL")
  expect_r6(l$oracle_learner, "LearnerRegr")
})

test_that("lce.simulate is registered in the dictionary", {
  l <- lrn("lce.simulate", optimizer = make_sim_optimizer(),
    oracle_learner = lrn("regr.rpart"))
  expect_r6(l, "LearnerLCESimulate")
})

test_that("lce.simulate train builds a valid model", {
  set.seed(1)
  task <- make_sim_task()
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(n_eval_points = 15L)
  l$train(task)

  expect_r6(l$model$objective, "ObjectiveLearner")
  expect_r6(l$model$eval_task, "TaskRegr")
  expect_equal(l$model$eval_task$nrow, 15L)
  expect_equal(l$model$eval_task$target_names, "y")
  expect_r6(l$model$measure, "Measure")
  expect_equal(l$model$measure$task_type, "regr")
  expect_equal(l$model$last_prefix_batch, 8L)
})

test_that("lce.simulate predicts a finite curve at the requested batches", {
  set.seed(2)
  task <- make_sim_task()
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(n_eval_points = 15L, seed = 1L)
  l$train(task)

  newdata <- data.table(batch_nr = 9:12)
  pred <- l$predict_newdata(newdata)
  expect_equal(length(pred$response), 4L)
  expect_true(all(is.finite(pred$response)))
  # the tracked measure is regr.mae, which is non-negative
  expect_true(all(pred$response >= 0))
})

test_that("lce.simulate scores exactly at requested batches (no interpolation)", {
  set.seed(3)
  task <- make_sim_task()
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(n_eval_points = 12L, seed = 7L)
  l$train(task)

  # Non-uniform spacing forces multiple optimize() calls with adjusted batch
  # sizes; we still get one value per requested point.
  pred_gap <- l$predict_newdata(data.table(batch_nr = c(9L, 11L, 14L)))
  expect_equal(length(pred_gap$response), 3L)
  expect_true(all(is.finite(pred_gap$response)))

  # Repeated batch_nrs map to identical per-row predictions.
  pred_dup <- l$predict_newdata(data.table(batch_nr = c(9L, 9L, 10L)))
  expect_equal(pred_dup$response[[1L]], pred_dup$response[[2L]])
})

test_that("lce.simulate is reproducible with a fixed seed", {
  set.seed(4)
  task <- make_sim_task()
  newdata <- data.table(batch_nr = 9:11)

  l1 <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l1$param_set$set_values(n_eval_points = 12L, seed = 123L)
  l1$train(task)
  p1 <- l1$predict_newdata(newdata)

  l2 <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l2$param_set$set_values(n_eval_points = 12L, seed = 123L)
  l2$train(task)
  p2 <- l2$predict_newdata(newdata)

  expect_equal(p1$response, p2$response)
})

test_that("lce.simulate returns the recorded curve in-sample", {
  set.seed(5)
  task <- make_sim_task()
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(n_eval_points = 12L, seed = 1L)
  l$train(task)

  recorded <- lce_train_per_batch(task)
  pred <- l$predict_newdata(data.table(batch_nr = c(3L, 5L)))
  expect_equal(pred$response,
    recorded$value[match(c(3L, 5L), recorded$batch)])
})

test_that("lce.simulate honors max_batch_cap with hold and linear extrapolation", {
  set.seed(6)
  task <- make_sim_task()
  newdata <- data.table(batch_nr = c(9L, 10L, 13L))

  l_hold <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l_hold$param_set$set_values(n_eval_points = 12L, seed = 2L,
    max_batch_cap = 2L, extrapolation = "hold")
  l_hold$train(task)
  p_hold <- l_hold$predict_newdata(newdata)
  # only the first two future points are simulated; batch 13 holds batch 10.
  expect_equal(p_hold$response[[3L]], p_hold$response[[2L]])

  l_lin <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l_lin$param_set$set_values(n_eval_points = 12L, seed = 2L,
    max_batch_cap = 2L, extrapolation = "linear")
  l_lin$train(task)
  p_lin <- l_lin$predict_newdata(newdata)
  expect_true(all(is.finite(p_lin$response)))
})

test_that("lce.simulate se is 0 with one restart and >= 0 with several", {
  set.seed(7)
  task <- make_sim_task()
  newdata <- data.table(batch_nr = 9:11)

  l1 <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l1$predict_type <- "se"
  l1$param_set$set_values(n_eval_points = 12L, n_restarts = 1L, seed = 3L)
  l1$train(task)
  p1 <- l1$predict_newdata(newdata)
  expect_true(all(p1$se == 0))

  l5 <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l5$predict_type <- "se"
  l5$param_set$set_values(n_eval_points = 12L, n_restarts = 5L, seed = 3L)
  l5$train(task)
  p5 <- l5$predict_newdata(newdata)
  expect_true(all(p5$se >= 0))
  expect_true(all(is.finite(p5$se)))
})

test_that("lce.simulate errors on an oracle that cannot fit", {
  set.seed(8)
  task <- make_sim_task()
  # regr.debug with error_train = 1 always errors during train.
  bad_oracle <- lrn("regr.debug", error_train = 1)
  l <- LearnerLCESimulate$new(make_sim_optimizer(), bad_oracle)
  expect_error(l$train(task), "oracle")
})

test_that("lce.simulate rejects an unknown surrogate_id", {
  set.seed(9)
  task <- make_sim_task()
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"),
    surrogate_id = "does_not_exist")
  expect_error(l$train(task), "surrogate_id")
})

test_that("lce.simulate imputes finite bounds for an unbounded search space", {
  set.seed(10)
  task <- make_sim_task(search_space = lce_search_space())
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(n_eval_points = 12L, seed = 1L)
  expect_silent(l$train(task))
  # the normalized search space has finite bounds
  ss <- l$model$search_space
  expect_true(all(is.finite(ss$lower)))
  expect_true(all(is.finite(ss$upper)))
  pred <- l$predict_newdata(data.table(batch_nr = 9:10))
  expect_true(all(is.finite(pred$response)))
})

test_that("lce.simulate runs pool-restricted when the task carries a pool", {
  set.seed(11)
  pool <- data.table(x1 = seq(0, 1, length.out = 40L),
    x2 = seq(1, 0, length.out = 40L))
  task <- make_sim_task(per_batch = 1L, pool = pool)
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(seed = 1L)
  l$train(task)
  expect_false(is.null(l$model$pool))
  pred <- l$predict_newdata(data.table(batch_nr = 9:10))
  expect_true(all(is.finite(pred$response)))
})

test_that("lce.simulate works inside resample()", {
  set.seed(12)
  task <- make_sim_task(n_batches = 10L)
  rs <- rsmp("lce.expanding_cv", horizon = 1L, min_train_batches = 6L,
    step_size = 1L)
  l <- LearnerLCESimulate$new(make_sim_optimizer(), lrn("regr.rpart"))
  l$param_set$set_values(n_eval_points = 10L, seed = 1L)
  rr <- suppressMessages(resample(task, l, rs))
  agg <- rr$aggregate(msr("lce.rmse"))
  expect_true(is.finite(agg))
  expect_true(agg >= 0)
})
