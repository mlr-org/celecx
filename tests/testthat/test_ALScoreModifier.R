al_score_modifier_test_context <- function(search_space) {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = 0),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "learn")),
    check_values = FALSE
  )
  instance <- SearchInstance$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 1L),
    check_values = FALSE
  )
  ALContext$new(
    instance = instance,
    pool = NULL,
    surrogates = list(),
    acq_functions = list(),
    run_state = list(
      working_acqs = new.env(parent = emptyenv()),
      working_acqs_search_space = new.env(parent = emptyenv())
    )
  )
}

test_that("score modifiers use search-space-scaled ALDistanceGower", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 1),
    z = p_int(lower = 1L, upper = 11L),
    group = p_fct(levels = c("a", "b"))
  )
  candidates <- data.table(
    x = c(0, 0.2, 0.8, 1),
    z = c(1L, 1L, 11L, 6L),
    group = factor(c("a", "a", "b", "b"), levels = c("a", "b"))
  )
  selected <- data.table(
    x = c(0, 1),
    z = c(1L, 6L),
    group = factor(c("a", "b"), levels = c("a", "b"))
  )

  distance <- ALDistanceGower$new()
  distance$param_set$set_values(scale = "minmax_space")
  distance$fit_pool(NULL, search_space)
  distance$set_reference_points(selected)

  distances <- distance$distances(candidates)
  expect_equal(
    al_distance_gower_search_space_distances(candidates, selected, search_space),
    distances
  )
  expect_equal(compute_gower_distance(candidates, selected, search_space), distances)

  context <- al_score_modifier_test_context(search_space)
  utility <- c(1, 2, 4, 8)

  local_penalization <- ALScoreModifierLocalPenalization$new(
    bandwidth = 0.1,
    penalization = 10
  )
  expected_local <- utility
  nearby <- rowSums(distances <= 0.1) > 0
  expected_local[nearby] <- expected_local[nearby] - 10
  expect_equal(
    local_penalization$modify(candidates, utility, selected, search_space, context),
    expected_local
  )

  diversity_weight <- 0.4
  utility_range <- max(utility) - min(utility)
  utility_normalized <- (utility - min(utility)) / utility_range
  distance_min <- apply(distances, 1L, min)
  distance_range <- max(distance_min) - min(distance_min)
  distance_normalized <- (distance_min - min(distance_min)) / distance_range
  expected_diversity <- (1 - diversity_weight) * utility_normalized +
    diversity_weight * distance_normalized

  diversity <- ALScoreModifierDiversity$new(diversity_weight = diversity_weight)
  expect_equal(
    diversity$modify(candidates, utility, selected, search_space, context),
    expected_diversity
  )
})
