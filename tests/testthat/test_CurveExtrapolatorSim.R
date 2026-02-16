# =============================================================================
# Tests for CurveExtrapolatorSim
# =============================================================================

create_replay_archive <- function() {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  archive <- ArchiveBatch$new(
    search_space = search_space,
    codomain = codomain
  )

  xdt <- data.table(x = seq(0, 1, length.out = 8L))
  ydt <- data.table(y = (xdt$x - 0.25)^2)
  archive$add_evals(xdt, xss_trafoed = NULL, ydt)

  archive
}


test_that("CurveExtrapolatorSim trains and predicts with replay context", {
  archive <- create_replay_archive()
  history <- data.table(
    n_evals = seq_len(archive$n_evals),
    metric = cummin(archive$data$y)
  )

  extrapolator <- CurveExtrapolatorSim$new(n_sim = 10L, seed = 42L)
  extrapolator$train(
    history,
    archive = archive,
    optimizer = opt("random_search", batch_size = 1L)
  )

  prediction1 <- extrapolator$predict(6:12)
  prediction2 <- extrapolator$predict(6:12)

  expect_data_table(prediction1, nrows = 7)
  expect_equal(prediction1, prediction2)
  expect_true(all(prediction1$q05 <= prediction1$q50))
  expect_true(all(prediction1$q50 <= prediction1$q95))
})


test_that("CurveExtrapolatorSim requires replay context", {
  history <- data.table(n_evals = 1:6, metric = rev(seq_len(6)))
  extrapolator <- CurveExtrapolatorSim$new(n_sim = 3L)

  expect_error(
    extrapolator$train(history),
    "archive"
  )
})


test_that("curve_objective_builder_default returns ObjectiveLearner", {
  archive <- create_replay_archive()
  objective <- curve_objective_builder_default(archive)

  expect_r6(objective, "ObjectiveLearner")
})
