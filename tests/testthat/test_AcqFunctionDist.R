TestSurrogateMean <- R6Class("TestSurrogateMean",
  inherit = SurrogateLearner,
  public = list(
    mean_fun = NULL,

    initialize = function(archive, mean_fun) {
      self$mean_fun <- mean_fun
      self$learner <- NULL
      self$input_trafo <- NULL
      self$output_trafo <- NULL
      param_set <- ps(catch_errors = p_lgl())
      param_set$values$catch_errors <- FALSE
      private$.archive <- archive
      private$.cols_x <- archive$cols_x
      private$.cols_y <- archive$cols_y
      private$.param_set <- param_set
    },

    predict = function(xdt) {
      data.table(mean = self$mean_fun(xdt))
    }
  ),
  active = list(
    print_id = function(rhs) {
      if (!missing(rhs)) stop("print_id is read-only")
      "test"
    },
    n_learner = function(rhs) {
      if (!missing(rhs)) stop("n_learner is read-only")
      1L
    },
    packages = function(rhs) {
      if (!missing(rhs)) stop("packages is read-only")
      character()
    },
    feature_types = function(rhs) {
      if (!missing(rhs)) stop("feature_types is read-only")
      "numeric"
    },
    properties = function(rhs) {
      if (!missing(rhs)) stop("properties is read-only")
      character()
    },
    predict_type = function(rhs) {
      if (!missing(rhs)) stop("predict_type is read-only")
      "response"
    }
  ),
  private = list(
    .update = function() NULL,
    .update_async = function() NULL,
    .reset = function() NULL
  )
)

TestALDistanceScale <- R6Class("TestALDistanceScale",
  inherit = ALDistance,
  public = list(
    n_fit = 0L,

    initialize = function() {
      super$initialize(
        id = "scale",
        param_set = ps(
          scale = p_dbl(lower = 0, init = 1)
        ),
        label = "Scale Test Distance",
        man = "celecx::test_scale_distance"
      )
    }
  ),
  private = list(
    .fit_pool = function(xdt, search_space) {
      self$n_fit <- self$n_fit + 1L
      list(xdt_pool = copy(xdt))
    },

    .distances = function(xdt, state, i = NULL) {
      xdt_pool <- state$xdt_reference
      if (!is.null(i)) {
        xdt_pool <- xdt_pool[i]
      }
      abs(outer(xdt$x, xdt_pool$x, "-")) * self$param_set$values$scale
    }
  )
)

TestAcqFunctionDistPassthrough <- R6Class("TestAcqFunctionDistPassthrough",
  inherit = AcqFunctionDist,
  public = list(
    initialize = function(al_distance) {
      super$initialize(
        id = "acq_dist_test",
        al_distance = al_distance,
        requires_predict_type_se = FALSE,
        direction = "maximize"
      )
    }
  ),
  private = list(
    .fun_dist = function(xdt, distances) {
      data.table(acq_dist_test = apply(distances, 1L, min))
    }
  )
)

make_acq_function_test_archive <- function(x) {
  objective <- ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x),
    domain = ps(x = p_dbl(lower = -10, upper = 10)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  instance <- OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 20L)
  )
  instance$eval_batch(data.table(x = x))
  instance$archive
}

test_that("distance-aware acquisition functions use distinct classes and dictionary keys", {
  keys <- as.data.table(mlr_acqfunctions)$key

  expect_subset(c("dist_gsx", "dist_igs", "dist_ideal", "gsy"), keys)
  expect_r6(mlr_acqfunctions$get("dist_gsx"), "AcqFunctionDistGSx")
  expect_r6(mlr_acqfunctions$get("dist_igs"), "AcqFunctionDistIGS")
  expect_r6(mlr_acqfunctions$get("dist_ideal"), "AcqFunctionDistIDEAL")
  expect_r6(mlr_acqfunctions$get("gsy"), "AcqFunctionGSy")
  expect_true(inherits(AcqFunctionDistGSx$new(), "AcqFunctionDist"))
  expect_false(inherits(AcqFunctionGSy$new(), "AcqFunctionDist"))
  expect_true(inherits(AcqFunctionGSy$new(), "AcqFunction"))
})

test_that("prediction-based acquisition constructors require SurrogateLearner", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- SurrogateNull$new(archive = archive)

  expect_error(AcqFunctionGSy$new(surrogate = surrogate), "SurrogateLearner")
  expect_error(AcqFunctionDistIGS$new(surrogate = surrogate), "SurrogateLearner")
  expect_error(AcqFunctionDistIDEAL$new(surrogate = surrogate), "SurrogateLearner")
})

test_that("AcqFunctionDistIDEAL initializes constants from constructor arguments", {
  acq_function <- AcqFunctionDistIDEAL$new(
    al_distance = NULL,
    delta = 2,
    omega = 3,
    tolerance_equality = 0.5
  )

  expect_equal(acq_function$constants$values[c("delta", "omega", "tolerance_equality")], list(
    delta = 2,
    omega = 3,
    tolerance_equality = 0.5
  ))
  expect_false("rho" %in% acq_function$constants$ids())
  expect_error(AcqFunctionDistIDEAL$new(al_distance = NULL, delta = -1), "p_dbl")
})

test_that("AcqFunctionDist computes distances before calling subclasses", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 2))
  distance <- TestALDistanceScale$new()
  distance$fit_pool(data.table(x = c(0, 2)), search_space)
  distance$set_reference_points(data.table(x = c(0, 2)))
  acq_function <- TestAcqFunctionDistPassthrough$new(al_distance = distance)

  expect_equal(acq_function$eval_dt(data.table(x = 1))$acq_dist_test, 1)
})

test_that("AcqFunctionDist update sets archive rows as distance references in order", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistIGS$new(surrogate = surrogate, al_distance = distance)

  expect_false(distance$is_fitted)
  expect_error(acq_function$update(), "not fitted")

  acq_function$fit_pool(data.table(x = c(0, 2, 4, 10)), archive$search_space)
  acq_function$update()

  expect_true(distance$is_fitted)
  expect_identical(distance$n_reference_points, 2L)
  expect_identical(distance$n_fit, 1L)

  scores <- acq_function$eval_dt(data.table(x = c(1, 10)))

  expect_equal(scores$acq_dist_igs, c(1.5, 39), tolerance = 1e-12)
  expect_identical(distance$n_fit, 1L)
})

test_that("AcqFunctionDist update applies distance constants before setting references", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- SurrogateNull$new(archive = archive)
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistGSx$new(surrogate = surrogate, al_distance = distance)

  acq_function$constants$values$al_distance.scale <- 3
  acq_function$fit_pool(data.table(x = c(0, 4)), archive$search_space)
  acq_function$update()

  expect_equal(acq_function$eval_dt(data.table(x = 1))$acq_dist_gsx, 3)
})

test_that("AcqFunctionDist exposes distance parameters as detached constants", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 2))
  distance <- TestALDistanceScale$new()
  distance$fit_pool(data.table(x = c(0, 2)), search_space)
  distance$set_reference_points(data.table(x = c(0, 2)))
  acq_function <- AcqFunctionDistGSx$new(al_distance = distance)

  expect_true(inherits(acq_function$constants, "ParamSetCollection"))
  expect_setequal(acq_function$constants$ids(), "al_distance.scale")
  expect_identical(acq_function$constants$values$al_distance.scale, 1)

  acq_function$constants$values$al_distance.scale <- 2
  expect_identical(acq_function$al_distance$param_set$values$scale, 2)
  expect_equal(acq_function$eval_dt(data.table(x = 1))$acq_dist_gsx, 2)
  expect_identical(acq_function$al_distance$param_set$values$scale, 2)

  clone <- acq_function$clone(deep = TRUE)
  clone$constants$values$al_distance.scale <- 3
  expect_equal(clone$eval_dt(data.table(x = 1))$acq_dist_gsx, 3)
  expect_identical(clone$al_distance$param_set$values$scale, 3)
  expect_identical(acq_function$al_distance$param_set$values$scale, 2)
})

test_that("AcqFunctionDist rebuilds distance constants when the distance is set lazily", {
  search_space <- ps(x = p_dbl(lower = 0, upper = 2))
  distance <- TestALDistanceScale$new()
  distance$fit_pool(data.table(x = c(0, 2)), search_space)
  distance$set_reference_points(data.table(x = c(0, 2)))
  acq_function <- AcqFunctionDistGSx$new(al_distance = NULL)

  expect_setequal(acq_function$constants$ids(), character())

  acq_function$al_distance <- distance
  expect_setequal(acq_function$constants$ids(), "al_distance.scale")

  acq_function$constants$values$al_distance.scale <- 4
  expect_equal(acq_function$eval_dt(data.table(x = 1))$acq_dist_gsx, 4)
  expect_identical(acq_function$al_distance$param_set$values$scale, 4)
})

test_that("SurrogateNull provides AcqFunction context without predictions", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- SurrogateNull$new(archive = archive)
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistGSx$new(al_distance = distance)

  expect_setequal(acq_function$domain$ids(), character())
  expect_setequal(acq_function$codomain$ids(), character())

  acq_function$surrogate <- surrogate

  expect_identical(acq_function$archive, archive)
  expect_identical(acq_function$domain$ids(), "x")
  expect_identical(acq_function$codomain$ids(), "acq_dist_gsx")
  expect_true("maximize" %in% acq_function$codomain$tags$acq_dist_gsx)
  acq_function$fit_pool(data.table(x = c(0, 4)), archive$search_space)
  acq_function$update()
  expect_equal(acq_function$eval_dt(data.table(x = 1))$acq_dist_gsx, 1)
  expect_error(surrogate$predict(data.table(x = 1)), "does not make predictions")
})

test_that("AcqFunctionGSy scores distance to observed target values", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  acq_function <- AcqFunctionGSy$new(surrogate = surrogate)

  scores <- acq_function$eval_dt(data.table(x = c(1, 10)))

  expect_equal(scores$acq_gsy, c(1.5, 6.5), tolerance = 1e-12)
})

test_that("AcqFunctionDistIGS takes the minimum after multiplying input and output distances", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistIGS$new(surrogate = surrogate, al_distance = distance)

  acq_function$fit_pool(data.table(x = c(0, 4)), archive$search_space)
  acq_function$update()
  scores <- acq_function$eval_dt(data.table(x = c(1, 10)))

  expect_equal(scores$acq_dist_igs, c(1.5, 39), tolerance = 1e-12)
})

test_that("AcqFunctionDistIDEAL implements IDW residuals and exploration constant", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistIDEAL$new(
    surrogate = surrogate,
    al_distance = distance,
    delta = 0.5,
    omega = 0
  )

  acq_function$fit_pool(data.table(x = c(0, 4)), archive$search_space)
  acq_function$update()
  scores <- acq_function$eval_dt(data.table(x = c(0, 2)))

  weights_raw <- exp(-c(4, 4)) / c(4, 4)
  weight_sum <- sum(weights_raw)
  expected_mid <- sum((weights_raw / weight_sum) * c(2.5^2, 1.5^2)) +
    0.5 * (2 / pi) * atan(1 / weight_sum)

  expect_equal(scores$acq_dist_ideal, c(0.5^2, expected_mid), tolerance = 1e-12)
})

test_that("AcqFunctionDistIDEAL keeps residual weights stable under raw-weight underflow", {
  distances_sq <- matrix(c(1000, 1001), nrow = 1L)
  pred_mean <- 0
  y_obs <- c(2, 10)

  log_weights <- -distances_sq[1L, ] - log(distances_sq[1L, ])
  offset <- max(log_weights)
  log_weight_sum <- offset + log(sum(exp(log_weights - offset)))
  expected <- sum(exp(log_weights - log_weight_sum) * y_obs^2) +
    (2 / pi) * atan(exp(-log_weight_sum))

  expect_equal(sum(exp(-distances_sq[1L, ]) / distances_sq[1L, ]), 0)
  expect_equal(
    acq_function_ideal_score(
      distances_sq = distances_sq,
      pred_mean = pred_mean,
      y_obs = y_obs,
      delta = 1,
      tolerance_equality = 0
    ),
    expected,
    tolerance = 1e-12
  )
})

test_that("AcqFunctionDistIDEAL applies rho when omega is positive", {
  archive <- make_acq_function_test_archive(c(0, 10))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistIDEAL$new(
    surrogate = surrogate,
    al_distance = distance,
    delta = 0,
    omega = 2
  )

  acq_function$fit_pool(data.table(x = c(0, 4, 10)), archive$search_space)
  acq_function$update()
  scores <- acq_function$eval_dt(data.table(x = c(0, 10)))

  expect_equal(scores$acq_dist_ideal, c(0.5^2 * 3, 0.5^2 * (1 + 2 * 4 / 6)), tolerance = 1e-12)
})

test_that("AcqFunctionDistIDEAL uses unit rho for search-space fits", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistIDEAL$new(
    surrogate = surrogate,
    al_distance = distance,
    delta = 0,
    omega = 2
  )

  acq_function$fit_pool(NULL, archive$search_space)
  acq_function$update()
  scores <- acq_function$eval_dt(data.table(x = c(0, 2)))

  weights_raw <- exp(-c(4, 4)) / c(4, 4)
  weight_sum <- sum(weights_raw)
  expected_mid <- sum((weights_raw / weight_sum) * c(2.5^2, 1.5^2))

  expect_equal(scores$acq_dist_ideal, 3 * c(0.5^2, expected_mid), tolerance = 1e-12)
})

test_that("AcqFunctionDistIDEAL rho requires a fitted pool and pool candidates", {
  archive <- make_acq_function_test_archive(c(0, 4))
  surrogate <- TestSurrogateMean$new(
    archive = archive,
    mean_fun = function(xdt) xdt$x + 0.5
  )
  distance <- TestALDistanceScale$new()
  acq_function <- AcqFunctionDistIDEAL$new(
    surrogate = surrogate,
    al_distance = distance,
    omega = 2
  )

  acq_function$al_distance$fit_pool(data.table(x = c(0, 4, 8)), archive$search_space)
  acq_function$update()

  expect_error(
    acq_function$eval_dt(data.table(x = 0)),
    "call fit_pool"
  )

  acq_function$fit_pool(data.table(x = c(0, 4, 8)), archive$search_space)
  acq_function$update()

  expect_error(
    acq_function$eval_dt(data.table(x = 2)),
    "fitted pool"
  )
})
