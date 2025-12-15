
# Tests for assert_dataset_domain_codomain helper

test_that("assert_dataset_domain_codomain validates correctly", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  # Should succeed without error
  result <- assert_dataset_domain_codomain(copy(dt), domain, codomain)
  expect_data_table(result)
})

test_that("assert_dataset_domain_codomain errors on missing domain columns", {
  dt <- data.table(x1 = c(1, 2), y = c(10, 20))

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    assert_dataset_domain_codomain(copy(dt), domain, codomain),
    "not found in dataset"
  )
})

test_that("assert_dataset_domain_codomain errors on extra feature columns", {
  dt <- data.table(x1 = c(1L, 2L), x2 = c(0.5, 1.0), y = c(10, 20))

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    assert_dataset_domain_codomain(copy(dt), domain, codomain),
    "not in domain"
  )
})

test_that("assert_dataset_domain_codomain errors on out of bounds values", {
  dt <- data.table(x1 = c(1L, 100L), y = c(10, 20))

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    assert_dataset_domain_codomain(copy(dt), domain, codomain),
    "outside domain bounds"
  )
})

test_that("assert_dataset_domain_codomain converts character to factor", {
  dt <- data.table(cat = c("a", "b", "c"), y = c(1, 2, 3))

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  result <- assert_dataset_domain_codomain(copy(dt), domain, codomain)
  expect_factor(result$cat)
  expect_equal(levels(result$cat), c("a", "b", "c"))
})

test_that("assert_dataset_domain_codomain expands factor levels", {
  dt <- data.table(cat = factor(c("a", "b")), y = c(1, 2))

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  result <- assert_dataset_domain_codomain(copy(dt), domain, codomain)
  expect_equal(levels(result$cat), c("a", "b", "c"))
})

test_that("assert_dataset_domain_codomain errors on invalid factor levels", {
  dt <- data.table(cat = factor(c("a", "d")), y = c(1, 2))

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    assert_dataset_domain_codomain(copy(dt), domain, codomain),
    "levels not in domain"
  )
})

test_that("assert_dataset_domain_codomain requires logical for ParamLgl", {
  dt <- data.table(flag = c(1L, 0L), y = c(1, 2))

  domain <- ps(flag = p_lgl())
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    assert_dataset_domain_codomain(copy(dt), domain, codomain),
    "ParamLgl"
  )
})

# Tests for ObjectiveDataset class

test_that("ObjectiveDataset basic functionality with data.table", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj, "ObjectiveDataset")
  expect_r6(obj, "Objective")
  expect_equal(obj$nrow, 3L)
  expect_equal(obj$id, "dataset")
  expect_equal(obj$domain$ids(), c("x1", "x2"))
  expect_equal(obj$codomain$ids(), "y")
})

test_that("ObjectiveDataset basic functionality with data.frame", {
  df <- data.frame(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "maximize"))

  obj <- ObjectiveDataset$new(
    dataset = df,
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj, "ObjectiveDataset")
  expect_equal(obj$nrow, 3L)
})

test_that("ObjectiveDataset basic functionality with TaskRegr", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )
  task <- as_task_regr(dt, target = "y")

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = task,
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj, "ObjectiveDataset")
  expect_equal(obj$nrow, 3L)
})

test_that("ObjectiveDataset eval works with exact matching", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  # Single evaluation
  result <- obj$eval(list(x1 = 1L, x2 = 0.5))
  expect_list(result)
  expect_equal(result$y, 10)

  result <- obj$eval(list(x1 = 2L, x2 = 1.0))
  expect_equal(result$y, 20)

  # Batch evaluation
  result_dt <- obj$eval_many(list(
    list(x1 = 1L, x2 = 0.5),
    list(x1 = 3L, x2 = 1.5)
  ))
  expect_data_table(result_dt, nrows = 2)
  expect_equal(result_dt$y, c(10, 30))
})

test_that("ObjectiveDataset eval_dt works", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  query <- data.table(x1 = c(1L, 2L), x2 = c(0.5, 1.0))
  result <- obj$eval_dt(query)
  expect_data_table(result, nrows = 2)
  expect_equal(result$y, c(10, 20))
})

test_that("ObjectiveDataset throws error for missing configurations", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    x2 = c(0.5, 1.0, 1.5),
    y = c(10, 20, 30)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_error(
    obj$eval(list(x1 = 5L, x2 = 1.0)),
    "not found"
  )
})

test_that("ObjectiveDataset handles factor columns", {
  dt <- data.table(
    cat = factor(c("a", "b", "c")),
    y = c(1, 2, 3)
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_equal(obj$nrow, 3L)

  result <- obj$eval(list(cat = "a"))
  expect_equal(result$y, 1)

  result <- obj$eval(list(cat = "c"))
  expect_equal(result$y, 3)
})

test_that("ObjectiveDataset handles character columns (converts to factor)", {
  dt <- data.table(
    cat = c("a", "b", "c"),
    y = c(1, 2, 3)
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_equal(obj$nrow, 3L)
  expect_factor(obj$data$cat)

  result <- obj$eval(list(cat = "b"))
  expect_equal(result$y, 2)
})

test_that("ObjectiveDataset lookup works with character query for factor column", {
  # Test that data.table join accepts character for factor column lookup
  dt <- data.table(
    cat = factor(c("a", "b", "c")),
    y = c(1, 2, 3)
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  # Query with character (not factor)
  query <- data.table(cat = c("a", "c"))
  result <- obj$eval_dt(query)
  expect_equal(result$y, c(1, 3))
})

test_that("ObjectiveDataset expands factor levels when data has fewer levels", {
  dt <- data.table(
    cat = factor(c("a", "b")),
    y = c(1, 2)
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_equal(obj$nrow, 2L)
  expect_equal(levels(obj$data$cat), c("a", "b", "c"))

  result <- obj$eval(list(cat = "a"))
  expect_equal(result$y, 1)

  expect_error(
    obj$eval(list(cat = "c")),
    "not found"
  )
})

test_that("ObjectiveDataset errors when data has factor levels not in domain", {
  dt <- data.table(
    cat = factor(c("a", "b", "d")),
    y = c(1, 2, 3)
  )

  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "levels not in domain"
  )
})

test_that("ObjectiveDataset handles ParamLgl with logical columns", {
  dt <- data.table(
    flag = c(TRUE, FALSE),
    y = c(1, 2)
  )

  domain <- ps(flag = p_lgl())
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(flag = TRUE))
  expect_equal(result$y, 1)

  result <- obj$eval(list(flag = FALSE))
  expect_equal(result$y, 2)
})

test_that("ObjectiveDataset errors when ParamLgl has non-logical column", {
  dt <- data.table(
    flag = c(1L, 0L),
    y = c(1, 2)
  )

  domain <- ps(flag = p_lgl())
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "ParamLgl"
  )
})

test_that("ObjectiveDataset errors when domain has missing columns", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "not found in dataset"
  )
})

test_that("ObjectiveDataset errors when dataset has extra feature columns", {
  dt <- data.table(
    x1 = c(1L, 2L),
    x2 = c(0.5, 1.0),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "not in domain"
  )
})

test_that("ObjectiveDataset errors when codomain column is missing", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(
    y = p_dbl(tags = "minimize"),
    y2 = p_dbl()
  )

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "Codomain columns not found"
  )
})

test_that("ObjectiveDataset errors when values are out of domain bounds", {
  dt <- data.table(
    x1 = c(1L, 100L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "outside domain bounds"
  )
})

test_that("ObjectiveDataset errors with empty domain", {
  dt <- data.table(x1 = c(1L, 2L), y = c(10, 20))

  domain <- ps()
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "at least one parameter"
  )
})

test_that("ObjectiveDataset errors with codomain without targets", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl())  # no minimize/maximize tag

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "tagged with"
  )
})

test_that("ObjectiveDataset task active binding returns TaskRegr", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    y = c(10, 20, 30)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  task <- obj$task
  expect_r6(task, "TaskRegr")
  expect_equal(task$nrow, 3L)
  expect_equal(task$target_names, "y")
  expect_equal(task$feature_names, "x1")

  # Modifying the task should not affect the internal data
  task$filter(1L)
  expect_equal(obj$nrow, 3L)
  expect_equal(obj$task$nrow, 3L)
})

test_that("ObjectiveDataset data active binding returns data.table", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  data <- obj$data
  expect_data_table(data, nrows = 2)

  expect_error(obj$data <- dt, "read-only")
})

test_that("ObjectiveDataset clone works correctly", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  obj2 <- obj$clone(deep = TRUE)
  expect_r6(obj2, "ObjectiveDataset")
  expect_equal(obj2$nrow, 2L)

  expect_equal(obj$eval(list(x1 = 1L))$y, 10)
  expect_equal(obj2$eval(list(x1 = 1L))$y, 10)
})

test_that("ObjectiveDataset properties is deterministic", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_true("deterministic" %in% obj$properties)
})

test_that("ObjectiveDataset handles mixed parameter types", {
  dt <- data.table(
    int_param = c(1L, 2L, 3L),
    dbl_param = c(0.1, 0.2, 0.3),
    fct_param = factor(c("a", "b", "c")),
    lgl_param = c(TRUE, FALSE, TRUE),
    y = c(10, 20, 30)
  )

  domain <- ps(
    int_param = p_int(lower = 1, upper = 10),
    dbl_param = p_dbl(lower = 0, upper = 1),
    fct_param = p_fct(levels = c("a", "b", "c", "d")),
    lgl_param = p_lgl()
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  expect_equal(obj$nrow, 3L)
  expect_equal(levels(obj$data$fct_param), c("a", "b", "c", "d"))

  result <- obj$eval(list(
    int_param = 1L,
    dbl_param = 0.1,
    fct_param = "a",
    lgl_param = TRUE
  ))
  expect_equal(result$y, 10)
})

test_that("ObjectiveDataset errors with ParamUty", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_uty())
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveDataset$new(
      dataset = dt,
      domain = domain,
      codomain = codomain
    ),
    "untyped"
  )
})

test_that("ObjectiveDataset with multi-criterion codomain", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    y1 = c(10, 20, 30),
    y2 = c(100, 200, 300)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "maximize")
  )

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(x1 = 2L))
  expect_equal(result$y1, 20)
  expect_equal(result$y2, 200)

  # task uses first target
  expect_equal(obj$task$target_names, "y1")
})

test_that("ObjectiveDataset with extra (non-target) codomain columns", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L),
    y = c(10, 20, 30),
    extra = c(1.1, 2.2, 3.3)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(
    y = p_dbl(tags = "minimize"),
    extra = p_dbl()
  )

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(x1 = 2L))
  expect_equal(result$y, 20)
  expect_equal(result$extra, 2.2)
})

test_that("ObjectiveDataset check_values = FALSE skips validation", {
  dt <- data.table(
    x1 = c(1L, 2L),
    y = c(10, 20)
  )

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain,
    check_values = FALSE
  )

  # Still fails on lookup (value not in table), but doesn't validate bounds
  expect_error(
    obj$eval(list(x1 = 100L)),
    "not found"
  )
})

test_that("ObjectiveDataset uses exact matching (no tolerance)", {
  # Verify that exact matching is used
  dt <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    y = c(10, 20, 30)
  )

  domain <- ps(x1 = p_dbl(lower = 0, upper = 1))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt,
    domain = domain,
    codomain = codomain
  )

  # Exact value should work
  result <- obj$eval(list(x1 = 0.1))
  expect_equal(result$y, 10)

  # Slightly different value should fail (no tolerance)
  expect_error(
    obj$eval(list(x1 = 0.1 + 1e-10)),
    "not found"
  )
})

test_that("data.table join accepts character for factor lookup", {
  # Confirm that data.table join works with character input for factor column
  # This is a sanity check for the lookup implementation
  dt <- data.table(
    cat = factor(c("a", "b", "c")),
    val = c(1, 2, 3)
  )
  setkey(dt, cat)

  # Query with character
  query <- data.table(cat = "b")
  result <- dt[query, on = "cat"]

  expect_equal(result$val, 2)
})

test_that("ObjectiveDataset with character values in data matches correctly", {
  # Data stored as character, domain as p_fct
  dt_char <- data.table(
    cat = c("x", "y", "z"),
    y = c(1, 2, 3)
  )

  domain <- ps(cat = p_fct(levels = c("x", "y", "z")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveDataset$new(
    dataset = dt_char,
    domain = domain,
    codomain = codomain
  )

  # After construction, should be factor
  expect_factor(obj$data$cat)

  # Query should work
  result <- obj$eval(list(cat = "y"))
  expect_equal(result$y, 2)
})
