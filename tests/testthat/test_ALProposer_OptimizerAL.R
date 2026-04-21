optimizer_al_test_instance <- function(n_pool = 8L, n_evals = 5L) {
  pool <- data.table(x = seq_len(n_pool), y = seq_len(n_pool)^2)
  objective <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xs) xs[, .(y)],
    domain = ps(x = p_int(lower = 1L, upper = n_pool)),
    codomain = ps(y = p_dbl(tags = "learn")),
    id = "optimizer_al_test"
  )

  SearchInstance$new(
    objective = objective,
    search_space = ps(x = p_int(lower = 1L, upper = n_pool)),
    terminator = trm("evals", n_evals = n_evals)
  )
}

test_that("OptimizerAL exposes owned component parameter sets and clones cleanly", {
  proposer <- ALProposerSequentialScore$new(
    acq_id = "gsx",
    surrogate_id = "archive",
    score_modifier = ALScoreModifierDiversity$new(),
    candidate_sampler = SpaceSamplerGSx$new(distance = clx_ald("gower"))
  )
  optimizer <- OptimizerAL$new(
    proposer = proposer,
    acq_functions = list(gsx = AcqFunctionDistGSx$new(al_distance = clx_ald("gower")))
  )

  expect_true(inherits(optimizer$param_set, "ParamSetCollection"))
  expect_setequal(
    optimizer$param_set$ids(),
    c(
      "batch_size", "replace_samples", "n_init",
      "proposer.n_candidates", "proposer.acq_fit_scope",
      "proposer.score_modifier.diversity_weight",
      "proposer.candidate_sampler.scale",
      "surrogate_archive.catch_errors",
      "acq_function_gsx.al_distance.scale"
    )
  )
  expect_setequal(
    optimizer$param_set$levels[["proposer.acq_fit_scope"]],
    c("global", "candidate", "search_space")
  )

  optimizer$param_set$values$proposer.score_modifier.diversity_weight <- 0.8
  optimizer$param_set$values$proposer.candidate_sampler.scale <- "off"
  optimizer$param_set$values$acq_function_gsx.al_distance.scale <- "standardize"

  expect_equal(optimizer$proposer$score_modifier$param_set$values$diversity_weight, 0.8)
  expect_equal(optimizer$proposer$candidate_sampler$distance$param_set$values$scale, "off")
  expect_equal(optimizer$acq_functions$gsx$al_distance$param_set$values$scale, "standardize")

  clone <- optimizer$clone(deep = TRUE)
  clone$param_set$values$proposer.score_modifier.diversity_weight <- 0.2
  clone$param_set$values$acq_function_gsx.al_distance.scale <- "off"

  expect_equal(optimizer$param_set$values$proposer.score_modifier.diversity_weight, 0.8)
  expect_equal(clone$param_set$values$proposer.score_modifier.diversity_weight, 0.2)
  expect_equal(optimizer$param_set$values$acq_function_gsx.al_distance.scale, "standardize")
  expect_equal(clone$param_set$values$acq_function_gsx.al_distance.scale, "off")
})

test_that("OptimizerAL stores acquisition functions as unwired prototypes", {
  instance <- optimizer_al_test_instance(n_evals = 1L)
  surrogate <- SurrogateNull$new(archive = instance$archive)
  acq_function <- AcqFunctionDistGSx$new(surrogate = surrogate)

  expect_warning(
    optimizer <- OptimizerAL$new(
      proposer = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive"),
      acq_functions = list(gsx = acq_function)
    ),
    "Ignoring pre-set surrogate"
  )

  expect_null(optimizer$acq_functions$gsx$surrogate)
  expect_equal(optimizer$acq_functions$gsx$domain$length, 0L)
  expect_equal(optimizer$acq_functions$gsx$codomain$length, 0L)
})

test_that("OptimizerAL runs GSx sequential-reference batches without duplicate pool points", {
  set.seed(1)
  instance <- optimizer_al_test_instance(n_pool = 8L, n_evals = 5L)
  optimizer <- OptimizerAL$new(
    proposer = ALProposerSequentialReference$new(acq_id = "gsx", surrogate_id = "archive"),
    init_sampler = SpaceSamplerGSx$new(),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(n_init = 1L, batch_size = 2L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 5L)
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
  expect_equal(instance$archive$n_batch, 3L)
})

test_that("ALProposerPortfolio queries children round-robin and respects pending points", {
  set.seed(2)
  instance <- optimizer_al_test_instance(n_pool = 8L, n_evals = 3L)
  optimizer <- OptimizerAL$new(
    proposer = ALProposerPortfolio$new(list(
      first = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive"),
      second = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive")
    )),
    init_sampler = SpaceSamplerUniform$new(),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(n_init = 1L, batch_size = 2L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 3L)
  expect_equal(instance$archive$n_batch, 2L)
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
})

test_that("ALProposerPseudoLabel uses temporary surrogate/acquisition state", {
  set.seed(3)
  instance <- optimizer_al_test_instance(n_pool = 10L, n_evals = 6L)
  optimizer <- OptimizerAL$new(
    proposer = ALProposerPseudoLabel$new(
      acq_id = "gsy",
      surrogate_id = "rpart",
      label_surrogate_id = "rpart"
    ),
    surrogates = list(rpart = SurrogateLearner$new(lrn("regr.rpart"))),
    acq_functions = list(gsy = AcqFunctionGSy$new())
  )
  optimizer$param_set$set_values(n_init = 2L, batch_size = 2L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 6L)
  expect_equal(instance$archive$n_batch, 3L)
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
})

test_that("OptimizerAL handles continuous search spaces with sampled candidate pools", {
  set.seed(4)
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  instance <- SearchInstance$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 4)
  )
  optimizer <- OptimizerAL$new(
    proposer = ALProposerScore$new(
      acq_id = "gsx",
      surrogate_id = "archive",
      candidate_sampler = SpaceSamplerUniform$new(),
      n_candidates = 8L,
      acq_fit_scope = "candidate"
    ),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(n_init = 1L, batch_size = 3L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 4L)
  expect_equal(instance$archive$n_batch, 2L)
})

test_that("OptimizerAL falls back to search-space acquisition fitting without a finite pool", {
  set.seed(41)
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x^2),
    domain = search_space,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  instance <- SearchInstance$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 4)
  )
  optimizer <- OptimizerAL$new(
    proposer = ALProposerScore$new(
      acq_id = "gsx",
      surrogate_id = "archive",
      candidate_sampler = SpaceSamplerUniform$new(),
      n_candidates = 8L
    ),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(n_init = 1L, batch_size = 3L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 4L)
  expect_equal(instance$archive$n_batch, 2L)
})

test_that("ALContext caches global and search-space fitted acquisitions separately", {
  instance <- optimizer_al_test_instance(n_pool = 8L, n_evals = 3L)
  instance$eval_batch(data.table(x = 1L))
  run_state <- list(
    working_acqs = new.env(parent = emptyenv()),
    working_acqs_search_space = new.env(parent = emptyenv())
  )
  context <- ALContext$new(
    instance = instance,
    pool = data.table(x = c(2L, 4L)),
    surrogates = list(archive = SurrogateNull$new()),
    acq_functions = list(gsx = AcqFunctionDistGSx$new(al_distance = clx_ald("affine"))),
    run_state = run_state
  )

  acq_global <- context$get_acq("gsx", "archive", fit_scope = "global")
  acq_search_space <- context$get_acq("gsx", "archive", fit_scope = "search_space")

  expect_false(identical(acq_global, acq_search_space))
  expect_true(exists("gsx::archive", envir = run_state$working_acqs, inherits = FALSE))
  expect_true(exists("gsx::archive", envir = run_state$working_acqs_search_space, inherits = FALSE))
  expect_equal(acq_global$al_distance$state$column_min, c(x = 2), tolerance = 1e-12)
  expect_equal(acq_search_space$al_distance$state$column_min, c(x = 1), tolerance = 1e-12)
})

test_that("OptimizerAL exhausts finite pools without replacement by default", {
  set.seed(5)
  instance <- optimizer_al_test_instance(n_pool = 2L, n_evals = 5L)
  optimizer <- OptimizerAL$new(
    proposer = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive"),
    init_sampler = SpaceSamplerUniform$new(),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(n_init = 1L, batch_size = 2L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 2L)
  expect_equal(anyDuplicated(instance$archive$data$x), 0L)
})

test_that("OptimizerAL allows finite-pool replacement between batches", {
  set.seed(6)
  instance <- optimizer_al_test_instance(n_pool = 2L, n_evals = 5L)
  optimizer <- OptimizerAL$new(
    proposer = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive"),
    init_sampler = SpaceSamplerUniform$new(),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(
    n_init = 1L,
    batch_size = 2L,
    replace_samples = "between_batches"
  )

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 5L)
  expect_gt(anyDuplicated(instance$archive$data$x), 0L)
  expect_true(all(instance$archive$data[, all(!duplicated(x)), by = batch_nr]$V1))
})

test_that("OptimizerAL counts distinct initial pool points when archive contains repeats", {
  set.seed(7)
  instance <- optimizer_al_test_instance(n_pool = 3L, n_evals = 4L)
  instance$eval_batch(data.table(x = 1L))
  instance$eval_batch(data.table(x = 1L))

  optimizer <- OptimizerAL$new(
    proposer = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive"),
    init_sampler = SpaceSamplerUniform$new(),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(n_init = 2L, batch_size = 1L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_evals, 4L)
  expect_equal(uniqueN(instance$archive$data, by = "x"), 3L)
})

test_that("OptimizerAL does not request default initial design when archive already contains points", {
  set.seed(8)
  instance <- optimizer_al_test_instance(n_pool = 8L, n_evals = 4L)
  instance$eval_batch(data.table(x = 1L))

  optimizer <- OptimizerAL$new(
    proposer = ALProposerScore$new(acq_id = "gsx", surrogate_id = "archive"),
    init_sampler = SpaceSamplerGSx$new(),
    acq_functions = list(gsx = AcqFunctionDistGSx$new())
  )
  optimizer$param_set$set_values(batch_size = 1L)

  expect_warning(optimizer$optimize(instance), NA)

  expect_equal(instance$archive$n_batch, 4L)
  expect_equal(instance$archive$n_evals, 4L)
})
