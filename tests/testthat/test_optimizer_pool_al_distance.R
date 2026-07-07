make_mixed_pool_objective <- function(n = 60L) {
  pool <- data.table(
    x1 = round(runif(n), 6),
    x2 = sample(c("a", "b", "c"), n, replace = TRUE)
  )
  pool <- unique(pool)
  pool[, y := x1 + 0.3 * as.integer(factor(x2))]
  ObjectiveDataset$new(pool,
    domain = ps(x1 = p_dbl(0, 1), x2 = p_fct(c("a", "b", "c"))),
    codomain = ps(y = p_dbl(tags = "learn")))
}

test_that("optimizer_pool_al runs distance-based methods on mixed-type pools with gower", {
  set.seed(1)
  objective <- make_mixed_pool_objective()
  # the surrogate learner must handle character features: archives store p_fct
  # columns as character, and the surrogate task is built from the archive
  for (method in c("gsx", "igs", "ideal")) {
    learner <- if (method == "gsx") NULL else lrn("regr.debug")
    optimizer <- optimizer_pool_al(method, learner = learner, n_init = 4L,
      batch_size = 2L, distance = "gower")
    instance <- SearchInstance$new(objective = objective,
      terminator = trm("evals", n_evals = 10L))
    optimizer$optimize(instance)
    expect_equal(nrow(instance$archive$data), 10L, info = method)
  }
})

test_that("the numeric method defaults still reject mixed-type pools", {
  set.seed(2)
  objective <- make_mixed_pool_objective()
  optimizer <- optimizer_pool_al("ideal", learner = lrn("regr.rpart"), n_init = 4L)
  instance <- SearchInstance$new(objective = objective,
    terminator = trm("evals", n_evals = 6L))
  expect_error(optimizer$optimize(instance))
})

test_that("distance accepts an ALDistance prototype and rejects unknown keys", {
  set.seed(3)
  objective <- make_mixed_pool_objective()
  optimizer <- optimizer_pool_al("gsx", n_init = 3L, distance = clx_ald("gower"))
  instance <- SearchInstance$new(objective = objective,
    terminator = trm("evals", n_evals = 6L))
  optimizer$optimize(instance)
  expect_equal(nrow(instance$archive$data), 6L)

  expect_error(optimizer_pool_al("gsx", distance = "nonexistent"), "nonexistent")
})
