# =============================================================================
# Tests for CurveExtrapolator base class
# =============================================================================

DummyCurveExtrapolator <- R6Class("DummyCurveExtrapolator",
  inherit = CurveExtrapolator,
  private = list(
    .train = function(history, ...) {
      list(history_n = nrow(history))
    },
    .predict = function(n_evals_future, ...) {
      data.table(
        n_evals = as.integer(n_evals_future),
        mean = as.numeric(n_evals_future),
        q05 = as.numeric(n_evals_future) - 1,
        q50 = as.numeric(n_evals_future),
        q95 = as.numeric(n_evals_future) + 1
      )
    }
  )
)


test_that("CurveExtrapolator trains and predicts with dummy subclass", {
  extrapolator <- DummyCurveExtrapolator$new()
  history <- data.table(n_evals = 1:5, metric = c(5, 4, 3, 2, 1))

  expect_false(extrapolator$is_trained)
  extrapolator$train(history)
  expect_true(extrapolator$is_trained)

  prediction <- extrapolator$predict(c(6L, 7L, 8L))
  expect_data_table(prediction, nrows = 3)
  expect_names(names(prediction), must.include = c("n_evals", "mean", "q05", "q50", "q95"))
})


test_that("CurveExtrapolator enforces strictly increasing x column", {
  extrapolator <- DummyCurveExtrapolator$new()

  bad_history <- data.table(n_evals = c(1L, 2L, 2L, 3L), metric = c(3, 2, 2, 1))
  expect_error(
    extrapolator$train(bad_history),
    "strictly increasing"
  )
})


test_that("CurveExtrapolator checks optional packages at train time", {
  extrapolator <- DummyCurveExtrapolator$new(packages = "pkg_that_does_not_exist_celecx")
  history <- data.table(n_evals = 1:5, metric = c(5, 4, 3, 2, 1))

  expect_error(
    extrapolator$train(history),
    "could not be loaded"
  )
})


test_that("CurveExtrapolator predict_target returns expected names", {
  extrapolator <- DummyCurveExtrapolator$new(direction = "minimize")
  history <- data.table(n_evals = 1:5, metric = c(5, 4, 3, 2, 1))
  extrapolator$train(history)

  stats <- extrapolator$predict_target(target = 3, n_evals_budget = 8L)

  expect_type(stats, "list")
  expect_true(all(c("p_reach_budget", "n_evals_q50", "n_evals_q90") %in% names(stats)))
  expect_true(is.numeric(stats$p_reach_budget))
})
