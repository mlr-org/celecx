test_that("mlr_al_distances exposes registered distance classes", {
  dict_dt <- as.data.table(mlr_al_distances)

  expect_setequal(dict_dt$key, c("standardize", "affine", "gower"))
  expect_true(all(c("label", "man") %in% names(dict_dt)))

  standardize <- clx_ald("standardize")
  affine <- clx_ald("affine", id = "affine_custom")
  gower <- clx_ald("gower")
  multiple <- clx_alds(c("standardize", "affine", "gower"))

  expect_r6(standardize, "ALDistanceStandardize")
  expect_r6(standardize, "ALDistanceGeometry")
  expect_identical(standardize$id, "standardize")
  expect_r6(affine, "ALDistanceAffine")
  expect_r6(affine, "ALDistanceGeometry")
  expect_identical(affine$id, "affine_custom")
  expect_r6(gower, "ALDistanceGower")
  expect_false(inherits(gower, "ALDistanceGeometry"))
  expect_identical(gower$param_set$ids(tags = "required"), "scale")
  expect_identical(gower$param_set$values$scale, "minmax_auto")
  expect_named(multiple, c("standardize", "affine", "gower"))
  expect_r6(multiple$standardize, "ALDistanceStandardize")
  expect_r6(multiple$affine, "ALDistanceAffine")
  expect_r6(multiple$gower, "ALDistanceGower")
})

test_that("ALDistance base class stores arbitrary fitted state", {
  TestALDistanceAbsolute <- R6Class("TestALDistanceAbsolute",
    inherit = ALDistance,
    public = list(
      initialize = function() {
        super$initialize(
          id = "absolute",
          label = "Absolute AL Distance",
          man = "celecx::test_absolute"
        )
      }
    ),
    private = list(
      .fit_pool = function(xdt, search_space) {
        list(
          xdt_pool = copy(xdt)
        )
      },

      .distances = function(xdt, state, i = NULL) {
        ref_xdt <- state$xdt_reference
        if (!is.null(i)) {
          ref_xdt <- ref_xdt[i]
        }

        outer(xdt$x, ref_xdt$x, function(query_x, ref_x) abs(query_x - ref_x))
      }
    )
  )

  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10)
  )
  pool <- data.table(
    x = c(0, 3, 6)
  )

  distance <- TestALDistanceAbsolute$new()
  hash_before <- distance$hash

  expect_false(distance$is_fitted)
  expect_null(distance$state)
  expect_null(distance$search_space)
  expect_null(distance$n_reference_points)
  expect_error(distance$set_reference_points(pool), "not fitted")

  distance$fit_pool(pool, search_space)

  expect_true(distance$is_fitted)
  expect_identical(distance$n_pool, 3L)
  expect_null(distance$n_reference_points)
  expect_equal(distance$state$xdt_pool, pool)
  expect_identical(distance$search_space$ids(), "x")
  expect_error(distance$distances(pool[3, .(x = 5)]), "no reference points")

  distance$set_reference_points(pool)

  expect_identical(distance$n_reference_points, 3L)
  expect_equal(distance$state$xdt_reference, pool)

  dist_full <- distance$distances(pool[3, .(x = 5)])
  dist_subset <- distance$distances(pool[3, .(x = 5)], i = 2L)

  expect_equal(dist_full, matrix(c(5, 2, 1), nrow = 1), tolerance = 1e-12)
  expect_equal(dist_subset, matrix(2, nrow = 1), tolerance = 1e-12)
  expect_identical(distance$hash, hash_before)
  expect_error(distance$state <- list(), "read-only")

  distance$fit_pool(pool[1:2], search_space)
  expect_null(distance$n_reference_points)
  expect_error(distance$distances(pool[3, .(x = 5)]), "no reference points")

  distance$clear()

  expect_false(distance$is_fitted)
  expect_null(distance$n_pool)
  expect_null(distance$n_reference_points)
  expect_null(distance$state)
  expect_null(distance$search_space)
})

test_that("ALDistanceGeometry provides Euclidean geometry distances", {
  TestALDistanceGeometryIdentity <- R6Class("TestALDistanceGeometryIdentity",
    inherit = ALDistanceGeometry,
    public = list(
      initialize = function() {
        super$initialize(
          id = "identity_geometry",
          label = "Identity Geometry Distance",
          man = "celecx::test_identity_geometry"
        )
      }
    ),
    private = list(
      .fit_geometry = function(xdt, search_space) {
        list(
          dimension = ncol(xdt)
        )
      },

      .transform = function(xdt, state) {
        as.matrix(xdt)
      }
    )
  )

  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    y = p_dbl(lower = 0, upper = 10)
  )
  pool <- data.table(
    x = c(0, 3, 6),
    y = c(0, 4, 8)
  )

  distance <- TestALDistanceGeometryIdentity$new()
  distance$fit_pool(pool, search_space)

  expect_identical(distance$dimension, 2L)
  expect_null(distance$reference_embedding)

  distance$set_reference_points(pool)

  expect_identical(distance$dimension, 2L)
  expect_identical(distance$n_pool, 3L)
  expect_equal(distance$reference_embedding, as.matrix(pool), tolerance = 1e-12)
  expect_equal(
    distance$distances(pool[3, .(x = 6, y = 8)], i = 1:2),
    matrix(c(10, 5), nrow = 1),
    tolerance = 1e-12
  )
})

test_that("ALDistanceStandardize standardizes numeric columns and handles zero variance", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 2),
    y = p_dbl(lower = 10, upper = 10)
  )
  pool <- data.table(
    x = c(0, 1, 2),
    y = c(10, 10, 10)
  )

  distance <- clx_ald("standardize")
  distance$fit_pool(pool, search_space)
  distance$set_reference_points(pool)

  expect_identical(distance$dimension, 2L)
  expect_false("geometry_pool" %in% names(distance$state))
  expect_equal(dim(distance$reference_embedding), c(3L, 2L))
  expect_equal(
    distance$distances(pool[2], i = c(1L, 3L)),
    matrix(c(1, 1), nrow = 1),
    tolerance = 1e-12
  )
  expect_equal(distance$distances(pool[2], i = 3L), matrix(1, nrow = 1), tolerance = 1e-12)
})

test_that("ALDistanceStandardize one-hot encodes factor columns from the search space", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 3),
    color = p_fct(levels = c("red", "blue", "green"))
  )
  pool <- data.table(
    x = c(1, 2, 3, 2),
    color = factor(c("red", "blue", NA, "red"), levels = c("red", "blue", "green"))
  )

  distance <- clx_ald("standardize")
  distance$fit_pool(pool, search_space)
  distance$set_reference_points(pool)

  green_query <- data.table(
    x = 2,
    color = factor("green", levels = c("red", "blue", "green"))
  )

  expect_identical(distance$dimension, 4L)
  expect_false("geometry_pool" %in% names(distance$state))
  expect_equal(dim(distance$reference_embedding), c(4L, 4L))
  expect_equal(distance$distances(pool[1], i = 1L), matrix(0, nrow = 1), tolerance = 1e-12)
  expect_true(all(is.finite(distance$distances(pool[3], i = 1L))))
  expect_true(all(is.finite(distance$distances(green_query, i = 1L))))
})

test_that("ALDistanceStandardize can fit finite search-space bounds", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    color = p_fct(levels = c("red", "blue"))
  )
  reference <- data.table(
    x = c(0, 10),
    color = c("red", "blue")
  )

  distance <- clx_ald("standardize")
  distance$fit_pool(NULL, search_space)
  distance$set_reference_points(reference)

  expect_true(distance$is_fitted)
  expect_null(distance$n_pool)
  expect_identical(distance$dimension, 3L)
  expect_equal(distance$state$column_center[["x"]], 5, tolerance = 1e-12)
  expect_equal(distance$state$column_scale[["x"]], 10 / sqrt(12), tolerance = 1e-12)
  expect_equal(
    distance$distances(data.table(x = 5, color = "red"), i = 1L),
    matrix(sqrt(3), nrow = 1),
    tolerance = 1e-12
  )
})

test_that("ALDistanceAffine affine-scales numeric columns to compute Euclidean distances", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    y = p_int(lower = 5L, upper = 5L)
  )
  pool <- data.table(
    x = c(0, 5, 10),
    y = c(5L, 5L, 5L)
  )

  distance <- clx_ald("affine")
  distance$fit_pool(pool, search_space)
  distance$set_reference_points(pool)

  expect_identical(distance$dimension, 2L)
  expect_false("geometry_pool" %in% names(distance$state))
  expect_equal(dim(distance$reference_embedding), c(3L, 2L))
  expect_equal(
    distance$distances(pool[2], i = c(1L, 3L)),
    matrix(c(1, 1), nrow = 1),
    tolerance = 1e-12
  )
  expect_equal(distance$distances(pool[2], i = 3L), matrix(1, nrow = 1), tolerance = 1e-12)
})

test_that("ALDistanceAffine can fit finite search-space bounds", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    y = p_int(lower = 5L, upper = 5L)
  )
  reference <- data.table(
    x = c(0, 10),
    y = c(5L, 5L)
  )

  distance <- clx_ald("affine")
  distance$fit_pool(NULL, search_space)
  distance$set_reference_points(reference)

  expect_true(distance$is_fitted)
  expect_null(distance$n_pool)
  expect_equal(distance$state$column_min, c(x = 0, y = 5), tolerance = 1e-12)
  expect_equal(distance$state$column_max, c(x = 10, y = 5), tolerance = 1e-12)
  expect_equal(
    distance$distances(data.table(x = 5, y = 5L)),
    matrix(c(1, 1), nrow = 1),
    tolerance = 1e-12
  )
})

test_that("ALDistanceAffine rejects non-numeric search spaces", {
  search_space <- ps(color = p_fct(levels = c("red", "blue")))
  pool <- data.table(
    color = factor(c("red", "blue"), levels = c("red", "blue"))
  )

  distance <- clx_ald("affine")

  expect_error(
    distance$fit_pool(pool, search_space),
    "numeric"
  )
})

test_that("ALDistanceGower computes mixed-type distances", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    color = p_fct(levels = c("red", "blue"))
  )
  pool <- data.table(
    x = c(0, 10),
    color = factor(c("red", "blue"), levels = c("red", "blue"))
  )
  query <- data.table(
    x = 5,
    color = factor("red", levels = c("red", "blue"))
  )

  distance <- clx_ald("gower")
  distance$fit_pool(pool, search_space)
  distance$set_reference_points(pool)

  expect_equal(distance$state$xdt_pool$x, c(0, 1), tolerance = 1e-12)
  expect_equal(distance$state$xdt_reference$x, c(0, 1), tolerance = 1e-12)
  expect_identical(distance$state$xdt_pool$color, pool$color)
  expect_equal(
    distance$distances(query),
    matrix(c(0.25, 0.75), nrow = 1),
    tolerance = 1e-12
  )
})

test_that("ALDistanceGower scales internal copies without mutating caller data", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    color = p_fct(levels = c("red", "blue"))
  )
  pool <- data.table(
    x = c(0, 10),
    color = factor(c("red", "blue"), levels = c("red", "blue"))
  )
  query <- data.table(
    x = 5,
    color = factor("red", levels = c("red", "blue"))
  )
  pool_before <- copy(pool)
  query_before <- copy(query)

  distance <- clx_ald("gower")
  distance$fit_pool(pool, search_space)
  distance$set_reference_points(pool)
  distance$distances(query)

  expect_equal(pool, pool_before)
  expect_equal(query, query_before)
  expect_equal(distance$state$xdt_pool$x, c(0, 1), tolerance = 1e-12)
})

test_that("ALDistanceGower can fit search-space bounds for min-max scaling", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 20),
    color = p_fct(levels = c("red", "blue"))
  )
  reference <- data.table(
    x = c(0, 10),
    color = c("red", "blue")
  )
  query <- data.table(
    x = 5,
    color = "red"
  )

  distance <- clx_ald("gower")
  distance$fit_pool(NULL, search_space)
  distance$set_reference_points(reference)

  expect_true(distance$is_fitted)
  expect_null(distance$n_pool)
  expect_null(distance$state$xdt_pool)
  expect_equal(
    distance$distances(query),
    matrix(c(0.125, 0.625), nrow = 1),
    tolerance = 1e-12
  )

  expect_error(
    clx_ald("gower", scale = "minmax_empirical")$fit_pool(NULL, search_space),
    "finite pool"
  )
  expect_error(
    clx_ald("gower", scale = "standardize")$fit_pool(NULL, search_space),
    "finite pool"
  )
})

test_that("ALDistanceGower supports numeric scaling modes", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 20)
  )
  pool <- data.table(
    x = c(0, 10)
  )
  query <- data.table(
    x = 5
  )

  distance_empirical <- clx_ald("gower", scale = "minmax_empirical")
  distance_space <- clx_ald("gower", scale = "minmax_space")
  distance_standardize <- clx_ald("gower", scale = "standardize")
  distance_off <- clx_ald("gower", scale = "off")

  distance_empirical$fit_pool(pool, search_space)
  distance_space$fit_pool(pool, search_space)
  distance_standardize$fit_pool(pool, search_space)
  distance_off$fit_pool(pool, search_space)
  distance_empirical$set_reference_points(pool)
  distance_space$set_reference_points(pool)
  distance_standardize$set_reference_points(pool)
  distance_off$set_reference_points(pool)

  expect_equal(
    distance_empirical$distances(query),
    matrix(c(0.5, 0.5), nrow = 1),
    tolerance = 1e-12
  )
  expect_equal(
    distance_space$distances(query),
    matrix(c(0.25, 0.25), nrow = 1),
    tolerance = 1e-12
  )
  expect_equal(
    distance_standardize$distances(query),
    matrix(rep(5 / stats::sd(c(0, 10)), 2L), nrow = 1),
    tolerance = 1e-12
  )
  expect_equal(
    distance_off$distances(query),
    matrix(c(5, 5), nrow = 1),
    tolerance = 1e-12
  )
})

test_that("ALDistanceGower ignores missing values in pairwise averaging", {
  search_space <- ps(
    x = p_dbl(lower = 0, upper = 10),
    color = p_fct(levels = c("red", "blue"))
  )
  pool <- data.table(
    x = c(0, 10),
    color = factor(c(NA, "blue"), levels = c("red", "blue"))
  )
  query <- data.table(
    x = 0,
    color = factor("red", levels = c("red", "blue"))
  )

  distance <- clx_ald("gower")
  distance$fit_pool(pool, search_space)
  distance$set_reference_points(pool)

  expect_equal(
    distance$distances(query),
    matrix(c(0, 1), nrow = 1),
    tolerance = 1e-12
  )
})
