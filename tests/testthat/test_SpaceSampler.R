space_sampler_test_sort <- function(x) {
  x <- copy(x)
  setorderv(x, names(x))
  x
}

space_sampler_test_kmedoids_exact_indices <- function(distance_matrix, pool_idx, n, fixed_idx = integer(0)) {
  combinations <- combn(pool_idx, n, simplify = FALSE)
  objectives <- vapply(combinations, function(selected_idx) {
    medoid_idx <- c(fixed_idx, selected_idx)
    sum(apply(distance_matrix[, medoid_idx, drop = FALSE], 1L, min))
  }, numeric(1L))

  combinations[[which.min(objectives)]]
}

test_that("mlr_space_samplers exposes registered sampler classes", {
  dict_dt <- as.data.table(mlr_space_samplers)

  expect_setequal(dict_dt$key, c(
    "uniform",
    "lhs",
    "sobol",
    "gsx",
    "kmeans",
    "relational_kmeans",
    "kmedoids",
    "chain",
    "conditional"
  ))
  expect_true(all(c("label", "man", "deterministic") %in% names(dict_dt)))

  uniform <- clx_sps("uniform")
  lhs <- clx_sps("lhs")
  sobol <- clx_sps("sobol")
  gsx <- clx_sps("gsx")
  kmeans <- clx_sps("kmeans")
  relational_kmeans <- clx_sps("relational_kmeans")
  kmedoids <- clx_sps("kmedoids")
  chain <- clx_sps("chain")
  conditional <- clx_sps("conditional")
  multiple <- clx_spss(c("uniform", "gsx", "kmeans"))

  expect_r6(uniform, "SpaceSamplerUniform")
  expect_r6(lhs, "SpaceSamplerLhs")
  expect_r6(sobol, "SpaceSamplerSobol")
  expect_r6(gsx, "SpaceSamplerGSx")
  expect_r6(kmeans, "SpaceSamplerKMeans")
  expect_r6(relational_kmeans, "SpaceSamplerRelationalKMeans")
  expect_r6(kmedoids, "SpaceSamplerKMedoids")
  expect_r6(chain, "SpaceSamplerChain")
  expect_r6(conditional, "SpaceSamplerConditional")
  expect_identical(gsx$id, "gsx.standardize")
  expect_identical(kmeans$id, "kmeans.affine")
  expect_identical(relational_kmeans$id, "relational_kmeans.affine")
  expect_identical(kmedoids$id, "kmedoids.gower")
  expect_named(multiple, c("uniform", "gsx.standardize", "kmeans.affine"))
  expect_true(uniform$deterministic)
  expect_false(lhs$deterministic)
  expect_false(sobol$deterministic)
  expect_false(gsx$deterministic)
  expect_false(kmeans$deterministic)
  expect_false(relational_kmeans$deterministic)
  expect_true(kmedoids$deterministic)
  expect_true(conditional$deterministic)
})

test_that("SpaceSampler base returns the whole pool when n exceeds pool size", {
  TestSpaceSamplerNeverCalled <- R6Class("TestSpaceSamplerNeverCalled",
    inherit = SpaceSampler,
    public = list(
      initialize = function() {
        super$initialize(
          id = "never_called",
          deterministic = TRUE,
          label = "Never Called Sampler",
          man = "celecx::test_never_called_sampler"
        )
      }
    ),
    private = list(
      .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
        stop("should not be called")
      }
    )
  )

  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  pool <- data.table(x = c(0.1, 0.9))
  sampler <- TestSpaceSamplerNeverCalled$new()

  expect_equal(sampler$sample(2L, search_space = search_space, pool = pool), pool)
  expect_equal(sampler$sample(3L, search_space = search_space, pool = pool), pool)
})

test_that("SpaceSamplerUniform samples from a pool without replacement", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  pool <- data.table(x = c(0.1, 0.2, 0.3, 0.4))
  sampler <- clx_sps("uniform")

  set.seed(1)
  result <- sampler$sample(2L, search_space = search_space, pool = pool)

  expect_equal(result, pool[c(1L, 3L)])
})

test_that("SpaceSamplerUniform samples from the search space without a pool", {
  search_space <- ps(
    x = p_dbl(lower = -1, upper = 1),
    flag = p_lgl()
  )
  sampler <- clx_sps("uniform")

  set.seed(1)
  result <- sampler$sample(5L, search_space = search_space)

  expect_data_table(result, nrows = 5L)
  search_space$assert_dt(result)
})

test_that("SpaceSamplerLhs samples from the search space without a pool", {
  search_space <- ps(
    x = p_dbl(lower = -1, upper = 1),
    y = p_int(lower = 1L, upper = 5L),
    flag = p_lgl()
  )
  sampler <- clx_sps("lhs")

  set.seed(1)
  result <- sampler$sample(10L, search_space = search_space)

  expect_data_table(result, nrows = 10L)
  search_space$assert_dt(result)
})

test_that("SpaceSamplerSobol samples from the search space without a pool", {
  search_space <- ps(
    x = p_dbl(lower = -1, upper = 1),
    y = p_int(lower = 1L, upper = 5L),
    mode = p_fct(levels = c("a", "b", "c"))
  )
  sampler <- clx_sps("sobol")

  set.seed(1)
  result <- sampler$sample(10L, search_space = search_space)

  expect_data_table(result, nrows = 10L)
  search_space$assert_dt(result)
})

test_that("search-space-only samplers reject pools before returning them", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  pool <- data.table(x = c(0.1, 0.9))

  for (sampler in clx_spss(c("lhs", "sobol"))) {
    expect_error(
      sampler$sample(1L, search_space = search_space, pool = pool),
      "does not support 'pool'",
      info = sampler$id
    )
  }
})

test_that("SpaceSamplerDistance forwards the wrapped distance param_set by reference", {
  distance <- clx_ald("gower")
  sampler <- SpaceSamplerGSx$new(distance = distance)

  expect_identical(sampler$param_set, distance$param_set)

  sampler$param_set$values$scale <- "off"
  expect_identical(distance$param_set$values$scale, "off")
})

test_that("SpaceSamplerDistance keeps param_set forwarding after deep clone", {
  sampler <- SpaceSamplerGSx$new(distance = clx_ald("gower"))
  sampler$param_set$values$scale <- "off"

  clone <- sampler$clone(deep = TRUE)

  expect_identical(clone$param_set, clone$distance$param_set)
  expect_identical(clone$param_set$values$scale, "off")
  expect_identical(clone$distance$param_set$values$scale, "off")

  clone$param_set$values$scale <- "standardize"
  expect_identical(clone$distance$param_set$values$scale, "standardize")
  expect_identical(sampler$distance$param_set$values$scale, "off")
})

test_that("SpaceSamplerGSx matches centroid-nearest then farthest-first order", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 2, 5, 8, 10))
  sampler <- clx_sps("gsx", distance = clx_ald("standardize"))

  result <- sampler$sample(3L, search_space = search_space, pool = pool)

  expect_equal(result, pool[c(3L, 1L, 5L)])
})

test_that("SpaceSamplerGSx uses known_pool as the initial reference set", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 2, 8, 10))
  known_pool <- data.table(x = 5)
  sampler <- clx_sps("gsx", distance = clx_ald("standardize"))

  result <- sampler$sample(
    2L,
    search_space = search_space,
    pool = pool,
    known_pool = known_pool
  )

  expect_equal(result, pool[c(1L, 4L)])
})

test_that("SpaceSamplerGSx currently requires a pool", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  sampler <- clx_sps("gsx")

  expect_error(
    sampler$sample(2L, search_space = search_space),
    "requires a pool"
  )
})

test_that("SpaceSamplerKMeans returns centroid-nearest point for n = 1", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 5, 10))
  sampler <- clx_sps("kmeans")

  result <- sampler$sample(1L, search_space = search_space, pool = pool)

  expect_equal(result, pool[2L])
})

test_that("SpaceSamplerKMeans incorporates known_pool into the fitted geometry", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 10))
  known_pool <- data.table(x = 10)
  sampler <- clx_sps("kmeans")

  result <- sampler$sample(
    1L,
    search_space = search_space,
    pool = pool,
    known_pool = known_pool
  )

  expect_equal(result, pool[2L])
})

test_that("SpaceSamplerKMeans uses the fitted data as reference embedding", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 5, 10))
  sampler <- clx_sps("kmeans")

  sampler$distance$fit_pool(pool, search_space)
  on.exit(sampler$distance$clear(), add = TRUE)
  expect_null(sampler$distance$reference_embedding)

  sampler$distance$set_reference_points(pool)
  expect_equal(
    celecx:::space_sampler_distance_pool_matrix(sampler$distance),
    sampler$distance$reference_embedding,
    tolerance = 1e-12
  )
})

test_that("SpaceSamplerKMeans currently requires a pool", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  sampler <- clx_sps("kmeans")

  expect_error(
    sampler$sample(2L, search_space = search_space),
    "requires a pool"
  )
})

test_that("SpaceSamplerRelationalKMeans matches SpaceSamplerKMeans for affine distances", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    y = p_dbl(lower = 0, upper = 10)
  )
  pool <- data.table(
    x = c(0, 0, 1, 9, 10, 10),
    y = c(0, 1, 0, 10, 9, 10)
  )
  sampler_kmeans <- clx_sps("kmeans", distance = clx_ald("affine"))
  sampler_relational <- clx_sps("relational_kmeans", distance = clx_ald("affine"))

  set.seed(1)
  result_kmeans <- sampler_kmeans$sample(2L, search_space = search_space, pool = pool)
  set.seed(1)
  result_relational <- sampler_relational$sample(2L, search_space = search_space, pool = pool)

  expect_equal(
    space_sampler_test_sort(result_relational),
    space_sampler_test_sort(result_kmeans)
  )
})

test_that("SpaceSamplerRelationalKMeans currently requires a pool", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 1))
  sampler <- clx_sps("relational_kmeans")

  expect_error(
    sampler$sample(2L, search_space = search_space),
    "requires a pool"
  )
})

test_that("SpaceSamplerKMedoids matches the exact optimum on a small numeric pool", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 1, 2, 8, 9, 10))
  sampler <- SpaceSamplerKMedoids$new(distance = clx_ald("affine"))

  fit_data <- celecx:::space_sampler_distance_fit_data(pool, known_pool = NULL)
  sampler$distance$fit_pool(fit_data$xdt, search_space)
  sampler$distance$set_reference_points(fit_data$xdt)
  on.exit(sampler$distance$clear())
  distance_matrix <- sampler$distance$distances(fit_data$xdt)
  expected_idx <- space_sampler_test_kmedoids_exact_indices(
    distance_matrix = distance_matrix,
    pool_idx = fit_data$pool_idx,
    n = 2L
  )

  result <- sampler$sample(2L, search_space = search_space, pool = pool)

  expect_equal(result, pool[match(expected_idx, fit_data$pool_idx)])
})

test_that("SpaceSamplerKMedoids treats known_pool as fixed medoids", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(0, 1, 2, 8, 9, 10))
  known_pool <- data.table(x = 0.5)
  sampler <- SpaceSamplerKMedoids$new(distance = clx_ald("affine"))

  fit_data <- celecx:::space_sampler_distance_fit_data(pool, known_pool = known_pool)
  sampler$distance$fit_pool(fit_data$xdt, search_space)
  sampler$distance$set_reference_points(fit_data$xdt)
  on.exit(sampler$distance$clear(), add = TRUE)
  distance_matrix <- sampler$distance$distances(fit_data$xdt)
  expected_idx <- space_sampler_test_kmedoids_exact_indices(
    distance_matrix = distance_matrix,
    pool_idx = fit_data$pool_idx,
    n = 1L,
    fixed_idx = fit_data$known_idx
  )

  result <- sampler$sample(
    1L,
    search_space = search_space,
    pool = pool,
    known_pool = known_pool
  )

  expect_equal(result, pool[match(expected_idx, fit_data$pool_idx)])
})

test_that("SpaceSamplerKMedoids works with Gower distance on mixed search spaces", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    flag = p_lgl()
  )
  pool <- data.table(
    x = c(0, 1, 10),
    flag = c(FALSE, FALSE, TRUE)
  )
  sampler <- clx_sps("kmedoids")

  result <- sampler$sample(1L, search_space = search_space, pool = pool)

  expect_equal(result, pool[2L])
})

test_that("SpaceSamplerChain applies wrapped samplers in sequence", {
  TestSpaceSamplerHead <- R6Class("TestSpaceSamplerHead",
    inherit = SpaceSampler,
    public = list(
      initialize = function() {
        super$initialize(
          id = "head",
          deterministic = TRUE,
          label = "Head Sampler",
          man = "celecx::test_head_sampler"
        )
      }
    ),
    private = list(
      .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
        pool[seq_len(n)]
      }
    )
  )
  TestSpaceSamplerTail <- R6Class("TestSpaceSamplerTail",
    inherit = SpaceSampler,
    public = list(
      initialize = function() {
        super$initialize(
          id = "tail",
          deterministic = TRUE,
          label = "Tail Sampler",
          man = "celecx::test_tail_sampler"
        )
      }
    ),
    private = list(
      .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
        pool[(nrow(pool) - n + 1L):nrow(pool)]
      }
    )
  )

  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = 1:5)
  sampler <- SpaceSamplerChain$new(samplers = list(
    tail = TestSpaceSamplerTail$new(),
    head = TestSpaceSamplerHead$new()
  ))
  sampler$param_set$values$target_size_fn <- function(pool_size, n) c(4L, n)

  result <- sampler$sample(2L, search_space = search_space, pool = pool)

  expect_true(sampler$deterministic)
  expect_equal(result, data.table(x = 2:3))
})

test_that("SpaceSamplerChain exposes wrapped sampler parameters via prefixed ids", {
  sampler <- SpaceSamplerChain$new(samplers = list(
    gsx = SpaceSamplerGSx$new(distance = clx_ald("gower")),
    uniform = SpaceSamplerUniform$new()
  ))

  expect_setequal(sampler$param_set$ids(), c("target_size_fn", "gsx.scale"))
  expect_identical(sampler$param_set$values$gsx.scale, "minmax_auto")

  sampler$param_set$values$gsx.scale <- "off"
  expect_identical(sampler$samplers$gsx$param_set$values$scale, "off")

  sampler$samplers$gsx$param_set$values$scale <- "standardize"
  expect_identical(sampler$param_set$values$gsx.scale, "standardize")
})

test_that("SpaceSamplerChain keeps prefixed parameter forwarding after deep clone", {
  sampler <- SpaceSamplerChain$new(samplers = list(
    gsx = SpaceSamplerGSx$new(distance = clx_ald("gower")),
    uniform = SpaceSamplerUniform$new()
  ))
  sampler$param_set$values$gsx.scale <- "off"

  clone <- sampler$clone(deep = TRUE)

  expect_setequal(clone$param_set$ids(), c("target_size_fn", "gsx.scale"))
  expect_identical(clone$param_set$values$gsx.scale, "off")
  expect_identical(clone$samplers$gsx$param_set$values$scale, "off")

  clone$param_set$values$gsx.scale <- "standardize"
  expect_identical(clone$samplers$gsx$param_set$values$scale, "standardize")
  expect_identical(sampler$samplers$gsx$param_set$values$scale, "off")

  clone$samplers$gsx$param_set$values$scale <- "minmax_space"
  expect_identical(clone$param_set$values$gsx.scale, "minmax_space")
  expect_identical(sampler$param_set$values$gsx.scale, "off")
})

test_that("SpaceSamplerChain default target_size_fn respects finite and infinite pools", {
  sampler <- SpaceSamplerChain$new(samplers = list(
    first = SpaceSamplerUniform$new(),
    second = SpaceSamplerUniform$new(),
    third = SpaceSamplerUniform$new()
  ))
  target_size_fn <- sampler$param_set$values$target_size_fn

  expect_equal(target_size_fn(pool_size = 100L, n = 5L), c(37, 14, 5))
  expect_equal(target_size_fn(pool_size = Inf, n = 5L), c(20, 10, 5))
})

test_that("SpaceSamplerChain validates the target_size_fn output", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = 1:5)
  sampler <- SpaceSamplerChain$new(samplers = list(
    uniform = SpaceSamplerUniform$new(),
    tail = SpaceSamplerUniform$new()
  ))

  sampler$param_set$values$target_size_fn <- function(pool_size, n) c(2L, 3L)
  expect_error(
    sampler$sample(2L, search_space = search_space, pool = pool),
    "non-increasing"
  )

  sampler$param_set$values$target_size_fn <- function(pool_size, n) c(4L, 3L)
  expect_error(
    sampler$sample(2L, search_space = search_space, pool = pool),
    "final 'target_size_fn' entry"
  )
})

test_that("SpaceSamplerChain is non-deterministic if any wrapped sampler is non-deterministic", {
  sampler <- SpaceSamplerChain$new(samplers = list(
    uniform = SpaceSamplerUniform$new(),
    gsx = SpaceSamplerGSx$new(distance = clx_ald("standardize"))
  ))

  expect_false(sampler$deterministic)
})

test_that("SpaceSamplerConditional delegates based on pool availability", {
  TestSpaceSamplerConstant <- R6Class("TestSpaceSamplerConstant",
    inherit = SpaceSampler,
    public = list(
      initialize = function(id, value) {
        private$.value <- value
        super$initialize(
          id = id,
          deterministic = TRUE,
          label = sprintf("%s Sampler", id),
          man = "celecx::test_constant_sampler",
          additional_phash_input = ".value"
        )
      }
    ),
    private = list(
      .value = NULL,
      .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
        data.table(x = rep(private$.value, n))
      }
    )
  )

  search_space <- ps(x = p_dbl(lower = 0, upper = 10))
  pool <- data.table(x = c(2, 3, 4))
  sampler <- SpaceSamplerConditional$new(
    on_discrete = TestSpaceSamplerConstant$new("discrete", 1),
    on_continuous = TestSpaceSamplerConstant$new("continuous", 9)
  )

  expect_equal(
    sampler$sample(2L, search_space = search_space, pool = pool),
    data.table(x = c(1, 1))
  )
  expect_equal(
    sampler$sample(2L, search_space = search_space),
    data.table(x = c(9, 9))
  )
})

test_that("SpaceSamplerConditional exposes wrapped sampler parameters via prefixed ids", {
  sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerGSx$new(distance = clx_ald("gower")),
    on_continuous = SpaceSamplerKMedoids$new(distance = clx_ald("gower"))
  )

  expect_true(inherits(sampler$param_set, "ParamSetCollection"))
  expect_setequal(sampler$param_set$ids(), c("on_discrete.scale", "on_continuous.scale"))
  expect_identical(sampler$param_set$values$on_discrete.scale, "minmax_auto")
  expect_identical(sampler$param_set$values$on_continuous.scale, "minmax_auto")

  sampler$param_set$values$on_discrete.scale <- "off"
  expect_identical(sampler$on_discrete$param_set$values$scale, "off")

  sampler$on_continuous$param_set$values$scale <- "standardize"
  expect_identical(sampler$param_set$values$on_continuous.scale, "standardize")
})

test_that("SpaceSamplerConditional keeps prefixed parameter forwarding after deep clone", {
  sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerGSx$new(distance = clx_ald("gower")),
    on_continuous = SpaceSamplerKMedoids$new(distance = clx_ald("gower"))
  )
  sampler$param_set$values$on_discrete.scale <- "off"
  sampler$param_set$values$on_continuous.scale <- "standardize"

  clone <- sampler$clone(deep = TRUE)

  expect_true(inherits(clone$param_set, "ParamSetCollection"))
  expect_setequal(clone$param_set$ids(), c("on_discrete.scale", "on_continuous.scale"))
  expect_identical(clone$param_set$values$on_discrete.scale, "off")
  expect_identical(clone$param_set$values$on_continuous.scale, "standardize")
  expect_identical(clone$on_discrete$param_set$values$scale, "off")
  expect_identical(clone$on_continuous$param_set$values$scale, "standardize")

  clone$param_set$values$on_discrete.scale <- "minmax_space"
  expect_identical(clone$on_discrete$param_set$values$scale, "minmax_space")
  expect_identical(sampler$on_discrete$param_set$values$scale, "off")

  clone$on_continuous$param_set$values$scale <- "minmax_empirical"
  expect_identical(clone$param_set$values$on_continuous.scale, "minmax_empirical")
  expect_identical(sampler$param_set$values$on_continuous.scale, "standardize")
})

test_that("SpaceSamplerConditional is non-deterministic if any wrapped sampler is non-deterministic", {
  sampler <- SpaceSamplerConditional$new(
    on_discrete = SpaceSamplerUniform$new(),
    on_continuous = SpaceSamplerLhs$new()
  )

  expect_false(sampler$deterministic)
  expect_equal(sampler$packages, "lhs")
})
