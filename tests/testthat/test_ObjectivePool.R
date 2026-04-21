

# Tests for assert_data_table_param_set helper

test_that("assert_data_table_param_set validates correctly", {
  pool <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    candidate_id = c("a", "b", "c")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  result <- assert_data_table_param_set(copy(pool), domain)
  expect_data_table(result)
  expect_equal(result$x1, c(1L, 2L, 3L))
})

test_that("assert_data_table_param_set errors on missing domain columns", {
  pool <- data.table(
    x1 = c(1L, 2L),
    candidate_id = c("a", "b")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  expect_error(
    assert_data_table_param_set(copy(pool), domain),
    "not found in"
  )
})

test_that("assert_data_table_param_set can validate a subset of columns", {
  pool <- data.table(
    x1 = c(1L, 2L),
    candidate_id = c("a", "b")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  result <- assert_data_table_param_set(copy(pool), domain, presence = "subset")
  expect_data_table(result)
  expect_false("x2" %in% names(result))
})

test_that("assert_data_table_param_set errors on out-of-bounds values", {
  pool <- data.table(
    x1 = c(1L, 100L),
    candidate_id = c("a", "b")
  )

  domain <- ps(x1 = p_int(lower = 1L, upper = 10L))

  expect_error(
    assert_data_table_param_set(copy(pool), domain),
    "<=|Element"
  )
})

test_that("assert_data_table_param_set allows special values", {
  pool <- data.table(
    x1 = c(1L, 100L),
    candidate_id = c("a", "b")
  )

  domain <- ps(x1 = p_int(lower = 1L, upper = 10L, special_vals = list(100L)))

  result <- assert_data_table_param_set(copy(pool), domain)
  expect_equal(result$x1, c(1L, 100L))
})

test_that("assert_data_table_param_set allows ParamUty columns without custom checks", {
  pool <- data.table(
    x = c(1L, 2L),
    payload = I(list(list(value = 10), "anything"))
  )

  domain <- ps(
    x = p_int(lower = 1L, upper = 3L),
    payload = p_uty()
  )

  result <- assert_data_table_param_set(
    copy(pool),
    domain,
    allow_untyped = TRUE,
    require_uniqueness = FALSE
  )

  expect_data_table(result)
  expect_equal(result$payload[[1L]]$value, 10)
  expect_equal(result$payload[[2L]], "anything")
})

test_that("assert_data_table_param_set applies ParamUty custom checks when allowed", {
  pool <- data.table(
    x = c(1L, 2L),
    payload = I(list(
      list(value = 10),
      list(other = 20)
    ))
  )

  domain <- ps(
    x = p_int(lower = 1L, upper = 3L),
    payload = p_uty(custom_check = function(value) {
      if (is.list(value) && !is.null(value$value)) TRUE else "need payload$value"
    })
  )

  expect_error(
    assert_data_table_param_set(copy(pool), domain, allow_untyped = TRUE),
    "need payload\\$value"
  )
})

test_that("assert_data_table_param_set allows inactive dependency NAs", {
  pool <- data.table(
    gate = FALSE,
    x = NA_integer_
  )

  domain <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
  )

  expect_no_error(assert_data_table_param_set(copy(pool), domain))
})

test_that("assert_data_table_param_set rejects inactive dependency values", {
  pool <- data.table(
    gate = FALSE,
    x = 1L
  )

  domain <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
  )

  expect_error(
    assert_data_table_param_set(copy(pool), domain),
    "dependency 'gate == TRUE'"
  )
})

test_that("assert_data_table_param_set still checks ParamSet constraints", {
  pool <- data.table(
    x1 = 1.5,
    x2 = 1.5
  )

  domain <- ps(
    x1 = p_dbl(lower = 0, upper = 2),
    x2 = p_dbl(lower = 0, upper = 2),
    .constraint = function(x) x$x1 + x$x2 <= 2
  )

  expect_error(
    assert_data_table_param_set(copy(pool), domain),
    "Constraint not fulfilled"
  )
})

test_that("assert_data_table_param_set converts character columns for factor domains", {
  pool <- data.table(
    cat = c("a", "b", "c"),
    candidate_id = c("a1", "b1", "c1")
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))

  result <- assert_data_table_param_set(copy(pool), domain)
  expect_character(result$cat)
  expect_equal(result$cat, c("a", "b", "c"))
})

test_that("assert_data_table_param_set can allow duplicate configurations", {
  pool <- data.table(
    x1 = c(1L, 1L, 2L),
    x2 = c(0.5, 0.5, 1.0),
    candidate_id = c("a", "b", "c")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  expect_no_error(
    assert_data_table_param_set(copy(pool), domain, require_uniqueness = FALSE)
  )
})

test_that("assert_data_table_param_set errors on duplicate configurations by default", {
  pool <- data.table(
    x1 = c(1L, 1L, 2L),
    x2 = c(0.5, 0.5, 1.0),
    candidate_id = c("a", "b", "c")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  expect_error(
    assert_data_table_param_set(copy(pool), domain),
    "duplicate configurations"
  )
})

test_that("Pool-backed objectives reject domains with trafos", {
  pool <- data.table(
    x = c(0, 1),
    y = c(1, 2)
  )

  expect_error(
    ObjectivePoolRFun$new(
      pool = pool,
      fun = function(xdt) {
        data.table(y = xdt$y)
      },
      domain = ps(x = p_dbl(lower = 0, upper = 1, trafo = exp)),
      codomain = ps(y = p_dbl(tags = "learn"))
    ),
    "do not support domains with trafos"
  )
})


# Tests for ObjectivePoolRFun class

test_that("ObjectivePoolRFun basic functionality with data.table", {
  pool <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    candidate_id = c("a", "b", "c"),
    weight = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$weight / 10)
    },
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj, "ObjectivePoolRFun")
  expect_r6(obj, "ObjectivePoolAbstract")
  expect_r6(obj, "Objective")
  expect_equal(obj$pool_size, 3L)
  expect_equal(obj$id, "pool")
  expect_equal(obj$domain$ids(), c("x1", "x2"))
  expect_equal(obj$codomain$ids(), "y")
  expect_equal(obj$pool$candidate_id, c("a", "b", "c"))
})

test_that("ObjectivePoolRFun basic functionality with data.frame", {
  pool <- data.frame(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    candidate_id = c("a", "b", "c")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$x1 + xdt$x2)
    },
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj, "ObjectivePoolRFun")
  expect_equal(obj$pool_size, 3L)
})

test_that("ObjectivePoolRFun eval uses matched pool rows including extra columns", {
  pool <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    candidate_id = c("alpha", "beta", "gamma"),
    score = c(11, 22, 33)
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$score)
    },
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(x1 = 2L, x2 = 1.0))
  expect_equal(result$y, 22)

  result_dt <- obj$eval_dt(data.table(
    x1 = c(3L, 1L),
    x2 = c(1.5, 0.5)
  ))
  expect_equal(result_dt$y, c(33, 11))
})

test_that("ObjectivePoolRFun eval_dt preserves query order and duplicates", {
  pool <- data.table(
    x = c(1L, 2L, 3L),
    value = c(10, 20, 30)
  )

  domain <- ps(x = p_int(lower = 1L, upper = 10L))
  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$value)
    },
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval_dt(data.table(x = c(2L, 2L, 1L)))
  expect_equal(result$y, c(20, 20, 10))
})

test_that("ObjectivePoolRFun errors for configurations outside the pool", {
  pool <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5)
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )
  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$x1 + xdt$x2)
    },
    domain = domain,
    codomain = codomain
  )

  expect_error(
    obj$eval(list(x1 = 5L, x2 = 1.0)),
    "not found in pool"
  )
})

test_that("ObjectivePoolRFun handles factor columns", {
  pool <- data.table(
    cat = factor(c("a", "b", "c")),
    candidate_id = c("a1", "b1", "c1"),
    value = c(1, 2, 3)
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$value)
    },
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(cat = "b"))
  expect_equal(result$y, 2)

  result_dt <- obj$eval_dt(data.table(cat = c("c", "a")))
  expect_equal(result_dt$y, c(3, 1))
})

test_that("ObjectivePoolRFun rejects duplicate configurations at construction", {
  pool <- data.table(
    x1 = c(1L, 1L, 2L),
    x2 = c(0.5, 0.5, 1.0),
    candidate_id = c("a", "b", "c")
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )
  codomain <- ps(y = p_dbl(tags = "learn"))

  expect_error(
    ObjectivePoolRFun$new(
      pool = pool,
      fun = function(xdt) data.table(y = seq_len(nrow(xdt))),
      domain = domain,
      codomain = codomain
    ),
    "duplicate configurations"
  )
})

test_that("ObjectivePoolRFun clone works correctly", {
  pool <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    value = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1L, upper = 10L),
    x2 = p_dbl(lower = 0, upper = 2)
  )
  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$value)
    },
    domain = domain,
    codomain = codomain
  )

  obj2 <- obj$clone(deep = TRUE)

  expect_r6(obj2, "ObjectivePoolRFun")
  expect_equal(obj2$pool$value, c(10, 20, 30))
  expect_equal(obj$eval(list(x1 = 1L, x2 = 0.5))$y, 10)
  expect_equal(obj2$eval(list(x1 = 1L, x2 = 0.5))$y, 10)
})

test_that("ObjectivePoolRFun supports multi-criterion codomains", {
  pool <- data.table(
    x = c(1L, 2L, 3L),
    a = c(10, 20, 30),
    b = c(5, 4, 3)
  )

  domain <- ps(x = p_int(lower = 1L, upper = 10L))
  codomain <- ps(
    y1 = p_dbl(tags = "learn"),
    y2 = p_dbl(tags = "learn")
  )

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y1 = xdt$a, y2 = xdt$b)
    },
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval_dt(data.table(x = c(1L, 3L)))
  expect_equal(result$y1, c(10, 30))
  expect_equal(result$y2, c(5, 3))
})

test_that("ObjectivePoolRFun exposes custom properties and packages", {
  pool <- data.table(
    x = c(1L, 2L),
    value = c(10, 20)
  )

  domain <- ps(x = p_int(lower = 1L, upper = 10L))
  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) {
      data.table(y = xdt$value)
    },
    domain = domain,
    codomain = codomain,
    properties = "noisy",
    packages = "stats"
  )

  expect_true("noisy" %in% obj$properties)
  expect_true("pool_restricted" %in% obj$properties)
  expect_equal(obj$packages, "stats")
})

test_that("ObjectivePoolRFun has pool_restricted property", {
  pool <- data.table(x = c(1L, 2L), value = c(10, 20))
  domain <- ps(x = p_int(lower = 1L, upper = 10L))
  codomain <- ps(y = p_dbl(tags = "learn"))

  obj <- ObjectivePoolRFun$new(
    pool = pool,
    fun = function(xdt) data.table(y = xdt$value),
    domain = domain,
    codomain = codomain
  )

  expect_true("pool_restricted" %in% obj$properties)
})


ObjectivePoolWrapperDtSpy <- R6Class("ObjectivePoolWrapperDtSpy",
  inherit = ObjectiveRFunDt,
  public = list(
    dt_calls = 0L,
    many_calls = 0L,
    last_xdt = NULL,

    initialize = function(domain, check_values = FALSE) {
      super$initialize(
        fun = function(xdt) {
          data.table(y = seq_len(nrow(xdt)))
        },
        domain = domain,
        codomain = ps(y = p_dbl(tags = "minimize")),
        check_values = check_values
      )
    },

    eval_dt = function(xdt) {
      self$dt_calls <- self$dt_calls + 1L
      self$last_xdt <- copy(xdt)
      super$eval_dt(xdt)
    },

    eval_many = function(xss) {
      self$many_calls <- self$many_calls + 1L
      stop("eval_many should not be called")
    }
  )
)

ObjectivePoolWrapperForwardingSpy <- R6Class("ObjectivePoolWrapperForwardingSpy",
  inherit = Objective,
  public = list(
    dt_calls = 0L,
    many_calls = 0L,
    last_xss = NULL,

    initialize = function(domain, check_values = TRUE) {
      super$initialize(
        id = "pool_wrapper_forwarding_spy",
        domain = domain,
        codomain = ps(y = p_dbl(tags = "minimize")),
        check_values = check_values
      )
    },

    eval_dt = function(xdt) {
      self$dt_calls <- self$dt_calls + 1L
      super$eval_dt(xdt)
    },

    eval_many = function(xss) {
      self$many_calls <- self$many_calls + 1L
      self$last_xss <- xss

      if (self$check_values) {
        lapply(xss, self$domain$assert)
      }

      data.table(y = vapply(xss, function(xs) {
        if (is.null(xs$x)) 0 else xs$x
      }, numeric(1L)))
    }
  )
)

make_wrapped_dt_objective <- function(domain, fun, check_values = TRUE) {
  ObjectiveRFunDt$new(
    fun = fun,
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = check_values
  )
}


# Tests for ObjectivePoolWrapper class

test_that("ObjectivePoolWrapper delegates evaluation to wrapped objective", {
  pool <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    candidate_id = c("a", "b", "c")
  )

  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = xdt$x1 + xdt$x2)
    },
    domain = ps(
      x1 = p_int(lower = 1L, upper = 10L),
      x2 = p_dbl(lower = 0, upper = 2)
    ),
    codomain = ps(y = p_dbl(tags = "minimize")),
    properties = "deterministic"
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped
  )

  expect_r6(obj, "ObjectivePoolWrapper")
  expect_r6(obj, "ObjectivePoolAbstract")
  expect_equal(obj$domain$ids(), c("x1", "x2"))
  expect_equal(obj$codomain$ids(), "y")
  expect_true("deterministic" %in% obj$properties)
  expect_true("pool_restricted" %in% obj$properties)
  expect_equal(obj$packages, character(0))
  expect_equal(obj$eval(list(x1 = 2L, x2 = 1.0))$y, 3)
})

test_that("ObjectivePoolWrapper validates wrapped objective domain against pool", {
  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = xdt$x + xdt$z)
    },
    domain = ps(
      x = p_int(lower = 1L, upper = 10L),
      z = p_dbl(lower = 0, upper = 5)
    ),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  wrapper_domain <- ps(x = p_int(lower = 1L, upper = 10L))

  expect_error(
    ObjectivePoolWrapper$new(
      pool = data.table(x = 1L),
      objective = wrapped,
      domain = wrapper_domain$clone(deep = TRUE)
    ),
    "wrapped objective domain parameters not found in pool: z"
  )

  expect_error(
    ObjectivePoolWrapper$new(
      pool = data.table(x = 1:2, z = c(1, 6)),
      objective = wrapped,
      domain = wrapper_domain$clone(deep = TRUE)
    ),
    "pool column for parameter 'z'"
  )
})

test_that("ObjectivePoolWrapper reconstructs typed hidden columns when wrapper domain is a strict subset", {
  pool <- data.table(
    x = c(1L, 2L, 3L),
    z = c(10L, 20L, 30L)
  )

  wrapped <- make_wrapped_dt_objective(
    domain = ps(
      x = p_int(lower = 1L, upper = 3L),
      z = p_int(lower = 1L, upper = 30L)
    ),
    fun = function(xdt) {
      data.table(y = xdt$z)
    }
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped,
    domain = ps(x = p_int(lower = 1L, upper = 3L))
  )

  ydt <- obj$eval_dt(data.table(x = c(3L, 1L)))

  expect_equal(ydt$y, c(30, 10))
})

test_that("ObjectivePoolWrapper rejects pools that are ambiguous in the wrapper domain", {
  pool <- data.table(
    x = c(1L, 1L),
    z = c(10L, 20L)
  )

  wrapped <- make_wrapped_dt_objective(
    domain = ps(
      x = p_int(lower = 1L, upper = 3L),
      z = p_int(lower = 1L, upper = 30L)
    ),
    fun = function(xdt) {
      data.table(y = xdt$z)
    }
  )

  expect_error(
    ObjectivePoolWrapper$new(
      pool = pool,
      objective = wrapped,
      domain = ps(x = p_int(lower = 1L, upper = 3L))
    ),
    "duplicate configurations"
  )
})

test_that("ObjectivePoolWrapper drops wrapper-only columns when wrapper domain is a strict superset", {
  pool <- data.table(
    x = c(1L, 2L),
    gate = c(TRUE, FALSE)
  )
  last_xdt <- NULL

  wrapped <- make_wrapped_dt_objective(
    domain = ps(x = p_int(lower = 1L, upper = 3L)),
    fun = function(xdt) {
      last_xdt <<- copy(xdt)
      data.table(y = xdt$x)
    }
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped,
    domain = ps(
      x = p_int(lower = 1L, upper = 3L),
      gate = p_lgl()
    )
  )

  ydt <- obj$eval_dt(data.table(x = 2L, gate = FALSE))

  expect_equal(ydt$y, 2)
  expect_equal(names(last_xdt), "x")
})

test_that("ObjectivePoolWrapper supports partially overlapping wrapper and wrapped domains", {
  pool <- data.table(
    shared = c(1L, 2L),
    outer = c(TRUE, FALSE),
    inner = c(10L, 20L)
  )
  last_xdt <- NULL

  wrapped <- make_wrapped_dt_objective(
    domain = ps(
      shared = p_int(lower = 1L, upper = 3L),
      inner = p_int(lower = 1L, upper = 30L)
    ),
    fun = function(xdt) {
      last_xdt <<- copy(xdt)
      data.table(y = xdt$shared + xdt$inner)
    }
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped,
    domain = ps(
      shared = p_int(lower = 1L, upper = 3L),
      outer = p_lgl()
    )
  )

  ydt <- obj$eval_dt(data.table(shared = 2L, outer = FALSE))

  expect_equal(ydt$y, 22)
  expect_equal(names(last_xdt), c("shared", "inner"))
})

test_that("ObjectivePoolWrapper supports disjoint wrapper and wrapped domains", {
  pool <- data.table(
    selector = c("a", "b"),
    hidden = c(5L, 7L)
  )
  last_xdt <- NULL

  wrapped <- make_wrapped_dt_objective(
    domain = ps(hidden = p_int(lower = 1L, upper = 10L)),
    fun = function(xdt) {
      last_xdt <<- copy(xdt)
      data.table(y = xdt$hidden)
    }
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped,
    domain = ps(selector = p_fct(levels = c("a", "b")))
  )

  ydt <- obj$eval_dt(data.table(selector = "b"))

  expect_equal(ydt$y, 7)
  expect_equal(names(last_xdt), "hidden")
})

test_that("ObjectivePoolWrapper accepts pools inside both domains when numeric bounds differ", {
  pool <- data.table(x = c(2, 3))

  wrapped_wide <- make_wrapped_dt_objective(
    domain = ps(x = p_dbl(lower = 0, upper = 5)),
    fun = function(xdt) {
      data.table(y = xdt$x)
    }
  )
  obj_outer_wide <- ObjectivePoolWrapper$new(
    pool = copy(pool),
    objective = wrapped_wide,
    domain = ps(x = p_dbl(lower = 0, upper = 10))
  )

  wrapped_narrow <- make_wrapped_dt_objective(
    domain = ps(x = p_dbl(lower = 0, upper = 10)),
    fun = function(xdt) {
      data.table(y = xdt$x)
    }
  )
  obj_outer_narrow <- ObjectivePoolWrapper$new(
    pool = copy(pool),
    objective = wrapped_narrow,
    domain = ps(x = p_dbl(lower = 1, upper = 4))
  )

  expect_equal(obj_outer_wide$eval_dt(data.table(x = 3))$y, 3)
  expect_equal(obj_outer_narrow$eval_dt(data.table(x = 2))$y, 2)
})

test_that("ObjectivePoolWrapper validates pool values against both numeric domains independently", {
  pool <- data.table(x = 8)

  wrapped <- make_wrapped_dt_objective(
    domain = ps(x = p_dbl(lower = 0, upper = 10)),
    fun = function(xdt) {
      data.table(y = xdt$x)
    }
  )

  expect_error(
    ObjectivePoolWrapper$new(
      pool = pool,
      objective = wrapped,
      domain = ps(x = p_dbl(lower = 0, upper = 5))
    ),
    "pool column for parameter 'x'"
  )
})

test_that("ObjectivePoolWrapper accepts pools inside both factor domains when levels differ", {
  pool <- data.table(cat = c("b", "c"))

  wrapped_more_levels <- make_wrapped_dt_objective(
    domain = ps(cat = p_fct(levels = c("a", "b", "c", "d"))),
    fun = function(xdt) {
      data.table(y = match(xdt$cat, c("b", "c", "d")))
    }
  )
  obj_wrapper_fewer_levels <- ObjectivePoolWrapper$new(
    pool = copy(pool),
    objective = wrapped_more_levels,
    domain = ps(cat = p_fct(levels = c("b", "c")))
  )

  wrapped_fewer_levels <- make_wrapped_dt_objective(
    domain = ps(cat = p_fct(levels = c("b", "c"))),
    fun = function(xdt) {
      data.table(y = match(xdt$cat, c("b", "c")))
    }
  )
  obj_wrapper_more_levels <- ObjectivePoolWrapper$new(
    pool = copy(pool),
    objective = wrapped_fewer_levels,
    domain = ps(cat = p_fct(levels = c("a", "b", "c", "d")))
  )

  expect_equal(obj_wrapper_fewer_levels$eval_dt(data.table(cat = "c"))$y, 2)
  expect_equal(obj_wrapper_more_levels$eval_dt(data.table(cat = "b"))$y, 1)
})

test_that("ObjectivePoolWrapper accepts compatible numeric type mismatches", {
  integer_pool <- data.table(x = c(1L, 2L))
  double_pool <- data.table(x = c(1, 2))

  wrapped_double <- make_wrapped_dt_objective(
    domain = ps(x = p_dbl(lower = 0, upper = 3)),
    fun = function(xdt) {
      data.table(y = xdt$x)
    }
  )
  obj_wrapper_int <- ObjectivePoolWrapper$new(
    pool = copy(integer_pool),
    objective = wrapped_double,
    domain = ps(x = p_int(lower = 0L, upper = 3L))
  )

  wrapped_int <- make_wrapped_dt_objective(
    domain = ps(x = p_int(lower = 0L, upper = 3L)),
    fun = function(xdt) {
      data.table(y = xdt$x)
    }
  )
  obj_wrapper_double <- ObjectivePoolWrapper$new(
    pool = copy(double_pool),
    objective = wrapped_int,
    domain = ps(x = p_dbl(lower = 0, upper = 3))
  )

  expect_equal(obj_wrapper_int$eval_dt(data.table(x = 2L))$y, 2)
  expect_equal(obj_wrapper_double$eval_dt(data.table(x = 1))$y, 1)
})

test_that("ObjectivePoolWrapper rejects incompatible type mismatches on shared IDs", {
  pool <- data.table(x = c("a", "b"))

  wrapped <- make_wrapped_dt_objective(
    domain = ps(x = p_fct(levels = c("a", "b"))),
    fun = function(xdt) {
      data.table(y = seq_len(nrow(xdt)))
    }
  )

  expect_error(
    ObjectivePoolWrapper$new(
      pool = pool,
      objective = wrapped,
      domain = ps(x = p_int(lower = 1L, upper = 2L))
    ),
    "pool column for parameter 'x'"
  )
})

test_that("ObjectivePoolWrapper keeps native dt wrapped objectives on eval_dt path", {
  wrapped_domain <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
  )
  wrapped <- ObjectivePoolWrapperDtSpy$new(
    domain = wrapped_domain$clone(deep = TRUE),
    check_values = FALSE
  )
  pool <- data.table(
    gate = c(FALSE, TRUE),
    x = c(NA_integer_, 2L)
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped
  )

  ydt <- obj$eval_dt(data.table(gate = FALSE, x = NA_integer_))

  expect_equal(obj$objective$dt_calls, 1L)
  expect_equal(obj$objective$many_calls, 0L)
  expect_true(is.na(obj$objective$last_xdt$x[[1L]]))
  expect_equal(ydt$y, 1)
})

test_that("ObjectivePoolWrapper drops inactive dependency values for forwarding wrapped objectives", {
  wrapped_domain <- ps(
    gate = p_lgl(),
    x = p_int(lower = 1L, upper = 3L, depends = gate == TRUE)
  )
  wrapped <- ObjectivePoolWrapperForwardingSpy$new(
    domain = wrapped_domain$clone(deep = TRUE),
    check_values = TRUE
  )
  pool <- data.table(
    gate = c(FALSE, TRUE),
    x = c(NA_integer_, 2L)
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped
  )

  ydt <- obj$eval_dt(data.table(gate = FALSE, x = NA_integer_))

  expect_equal(obj$objective$dt_calls, 0L)
  expect_equal(obj$objective$many_calls, 1L)
  expect_null(obj$objective$last_xss[[1L]]$x)
  expect_equal(ydt$y, 0)
})

test_that("ObjectivePoolWrapper reconstructs hidden ParamUty columns from the pool", {
  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = vapply(xdt$payload, function(payload) {
        payload$value
      }, numeric(1L)))
    },
    domain = ps(
      x = p_int(lower = 1L, upper = 3L),
      payload = p_uty()
    ),
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = TRUE
  )
  pool <- data.table(x = 1:3)
  pool[, payload := list(
    list(value = 10),
    list(value = 20),
    list(value = 30)
  )]

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped,
    domain = ps(x = p_int(lower = 1L, upper = 3L))
  )

  ydt <- obj$eval_dt(data.table(x = c(3L, 1L)))

  expect_equal(ydt$y, c(30, 10))
})

test_that("ObjectivePoolWrapper uses outer id and clones wrapped objective", {
  pool <- data.table(
    x = c(1L, 2L),
    value = c(10, 20)
  )

  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = xdt$x + 1)
    },
    domain = ps(x = p_int(lower = 1L, upper = 10L)),
    codomain = ps(y = p_dbl(tags = "learn")),
    id = "wrapped_objective",
    properties = "noisy",
    packages = "stats"
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped,
    id = "wrapped_pool"
  )

  # Modifying the original does not affect the clone inside the wrapper
  wrapped$id <- "modified_original"

  expect_equal(obj$id, "wrapped_pool")
  expect_r6(obj$objective, "Objective")
  expect_equal(obj$objective$id, "wrapped_objective")
  expect_true("noisy" %in% obj$properties)
  expect_true("pool_restricted" %in% obj$properties)
  expect_equal(obj$packages, "stats")
  # When domain is NULL, wrapped objective's check_values is set to FALSE
  expect_false(obj$objective$check_values)
})

test_that("ObjectivePoolWrapper has pool_restricted property", {
  pool <- data.table(x = c(1L, 2L, 3L))

  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x),
    domain = ps(x = p_int(lower = 1L, upper = 10L)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  obj <- ObjectivePoolWrapper$new(pool = pool, objective = wrapped)

  expect_true("pool_restricted" %in% obj$properties)
})

test_that("ObjectivePoolWrapper rejects duplicate configurations at construction", {
  pool <- data.table(
    x = c(1L, 1L, 2L),
    candidate_id = c("a", "b", "c")
  )

  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = xdt$x)
    },
    domain = ps(x = p_int(lower = 1L, upper = 10L)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  expect_error(
    ObjectivePoolWrapper$new(
      pool = pool,
      objective = wrapped
    ),
    "duplicate configurations"
  )
})

test_that("ObjectivePoolWrapper errors for configurations outside the pool", {
  pool <- data.table(
    x = c(1L, 2L, 3L)
  )

  wrapped <- ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = xdt$x)
    },
    domain = ps(x = p_int(lower = 1L, upper = 10L)),
    codomain = ps(y = p_dbl(tags = "learn"))
  )

  obj <- ObjectivePoolWrapper$new(
    pool = pool,
    objective = wrapped
  )

  expect_error(
    obj$eval(list(x = 5L)),
    "not found in pool"
  )
})
