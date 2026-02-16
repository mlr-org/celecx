# =============================================================================
# Tests for TerminatorForecastTarget
# =============================================================================

create_archive_for_terminator <- function() {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  codomain <- ps(y = p_dbl(tags = "minimize"))
  archive <- ArchiveBatch$new(search_space = search_space, codomain = codomain)
  archive$add_evals(data.table(x = 0.5), xss_trafoed = NULL, data.table(y = 0.25))
  archive
}

create_forecast_tracker_for_terminator <- function(target = 0.1) {
  ForecastTracker$new(
    extrapolator = CurveExtrapolatorParametric$new(n_bootstrap = 30L),
    metric_name = "best_y",
    target = target,
    min_points = 2L
  )
}


test_that("TerminatorForecastTarget stops on low probability", {
  tracker <- create_forecast_tracker_for_terminator(target = 0.1)
  private <- get_private(tracker)
  private$.data <- data.table(
    update_nr = 1L,
    batch_nr = 1L,
    n_evals = 5L,
    metric_value = 0.5,
    budget_n_evals = 10L,
    p_reach_budget = 0.01,
    n_evals_q50 = NA_integer_,
    n_evals_q90 = NA_integer_,
    timestamp = Sys.time(),
    prediction = list(data.table())
  )

  terminator <- TerminatorForecastTarget$new(tracker)

  expect_true(terminator$is_terminated(create_archive_for_terminator()))
})


test_that("TerminatorForecastTarget stops when target is reached", {
  tracker <- create_forecast_tracker_for_terminator(target = 0.2)
  private <- get_private(tracker)
  private$.data <- data.table(
    update_nr = 1L,
    batch_nr = 1L,
    n_evals = 5L,
    metric_value = 0.1,
    budget_n_evals = 10L,
    p_reach_budget = 0.9,
    n_evals_q50 = 6L,
    n_evals_q90 = 7L,
    timestamp = Sys.time(),
    prediction = list(data.table())
  )

  terminator <- TerminatorForecastTarget$new(tracker)
  terminator$param_set$set_values(stop_on_unlikely = FALSE, stop_on_reached = TRUE)

  expect_true(terminator$is_terminated(create_archive_for_terminator()))
})


test_that("TerminatorForecastTarget does not stop without trigger", {
  tracker <- create_forecast_tracker_for_terminator(target = 0.05)
  private <- get_private(tracker)
  private$.data <- data.table(
    update_nr = 1L,
    batch_nr = 1L,
    n_evals = 5L,
    metric_value = 0.5,
    budget_n_evals = 10L,
    p_reach_budget = 0.8,
    n_evals_q50 = 9L,
    n_evals_q90 = NA_integer_,
    timestamp = Sys.time(),
    prediction = list(data.table())
  )

  terminator <- TerminatorForecastTarget$new(tracker)
  terminator$param_set$set_values(stop_on_unlikely = TRUE, p_reach_threshold = 0.05, stop_on_reached = TRUE)

  expect_false(terminator$is_terminated(create_archive_for_terminator()))
})
