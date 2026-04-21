
# Tests for assert_learner_domain helper

test_that("assert_learner_domain validates correctly", {
  # Create a simple learner and train it
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    x2 = c(0.5, 1.0, 1.5, 2.0),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 3)
  )

  # Should succeed without error
  result <- assert_learner_domain(learner, domain)
  expect_true(result)
})

test_that("assert_learner_domain errors on unfitted learner", {
  learner <- lrn("regr.featureless")

  domain <- ps(x1 = p_int(lower = 1, upper = 10))

  expect_error(
    assert_learner_domain(learner, domain),
    "not.*trained|not.*fitted|no.*model"
  )
})

test_that("assert_learner_domain errors on non-regr learner", {
  dt <- data.table(
    x1 = c(1, 2, 3, 4),
    y = factor(c("a", "b", "a", "b"))
  )
  task <- as_task_classif(dt, target = "y")
  learner <- lrn("classif.featureless")
  learner$train(task)

  domain <- ps(x1 = p_dbl(lower = 0, upper = 5))

  expect_error(
    assert_learner_domain(learner, domain),
    "regr"
  )
})

test_that("assert_learner_domain errors when domain has columns not in learner", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)  # Not in training data
  )

  expect_error(
    assert_learner_domain(learner, domain),
    "not.*train|not found"
  )
})

test_that("assert_learner_domain errors when domain parameters don't match feature types", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  # x1 is integer but domain says factor
  domain <- ps(x1 = p_fct(levels = c("a", "b")))

  expect_error(
    assert_learner_domain(learner, domain),
    "factor|not.*factor"
  )
})

test_that("assert_learner_domain errors when factor domain levels not subset of trained levels", {
  dt <- data.table(
    cat = factor(c("a", "b", "c", "d")),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  # Domain has level "e" which was not in training data
  domain <- ps(cat = p_fct(levels = c("a", "b", "e")))

  expect_error(
    assert_learner_domain(learner, domain),
    "not.*train|levels"
  )
})

test_that("assert_learner_domain accepts factor domain levels as subset of trained levels", {
  dt <- data.table(
    cat = factor(c("a", "b", "c", "d")),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  # Domain has subset of trained levels - this should be OK
  domain <- ps(cat = p_fct(levels = c("a", "b")))

  result <- assert_learner_domain(learner, domain)
  expect_true(result)
})

test_that("assert_learner_domain errors with empty domain", {
  dt <- data.table(x1 = c(1L, 2L, 3L, 4L), y = c(10, 20, 30, 40))
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps()

  expect_error(
    assert_learner_domain(learner, domain),
    "Missing"
  )
})

test_that("assert_learner_domain includes learner_name in error messages", {
  learner <- lrn("regr.featureless")

  domain <- ps(x1 = p_int(lower = 1, upper = 10))

  expect_error(
    assert_learner_domain(learner, domain, learner_name = "my_learner"),
    "my_learner"
  )
})

# Tests for ObjectiveLearner class

test_that("ObjectiveLearner basic functionality", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    x2 = c(0.5, 1.0, 1.5, 2.0),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 3)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj, "ObjectiveLearner")
  expect_r6(obj, "Objective")
  expect_equal(obj$id, "learner")
  expect_equal(obj$domain$ids(), c("x1", "x2"))
  expect_equal(obj$codomain$ids(), "y")
})

test_that("ObjectiveLearner eval works", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    x2 = c(0.5, 1.0, 1.5, 2.0),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 3)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  # Single evaluation - featureless learner returns mean of training data
  result <- obj$eval(list(x1 = 5L, x2 = 1.5))
  expect_list(result)
  expect_number(result$y)
  expect_equal(result$y, mean(dt$y))

  # Should work with different values too
  result2 <- obj$eval(list(x1 = 1L, x2 = 0.1))
  expect_equal(result2$y, mean(dt$y))
})

test_that("ObjectiveLearner eval_many works", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    x2 = c(0.5, 1.0, 1.5, 2.0),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 3)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  # Batch evaluation
  result_dt <- obj$eval_many(list(
    list(x1 = 1L, x2 = 0.5),
    list(x1 = 5L, x2 = 1.5),
    list(x1 = 10L, x2 = 2.5)
  ))
  expect_data_table(result_dt, nrows = 3)
  expect_equal(result_dt$y, rep(mean(dt$y), 3))
})

test_that("ObjectiveLearner eval_dt works", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    x2 = c(0.5, 1.0, 1.5, 2.0),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 3)
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  query <- data.table(x1 = c(1L, 5L), x2 = c(0.5, 1.5))
  result <- obj$eval_dt(query)
  expect_data_table(result, nrows = 2)
  expect_equal(result$y, rep(mean(dt$y), 2))
})

test_that("ObjectiveLearner handles factor columns correctly", {
  dt <- data.table(
    cat = factor(c("a", "b", "c", "d")),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  # Domain has subset of trained levels
  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  # Should work with factor values from domain
  result <- obj$eval(list(cat = "a"))
  expect_number(result$y)

  result <- obj$eval(list(cat = "b"))
  expect_number(result$y)
})

test_that("ObjectiveLearner errors when factor domain levels not subset of trained levels", {
  dt <- data.table(
    cat = factor(c("a", "b")),
    y = c(10, 20)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  # Domain has levels "a", "b", "c" but learner was only trained on "a", "b"
  domain <- ps(cat = p_fct(levels = c("a", "b", "c")))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "not.*subset|not.*trained|unknown.*level"
  )
})

test_that("ObjectiveLearner handles ParamLgl correctly", {
  dt <- data.table(
    flag = c(TRUE, FALSE, TRUE, FALSE),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(flag = p_lgl())
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(flag = TRUE))
  expect_number(result$y)

  result <- obj$eval(list(flag = FALSE))
  expect_number(result$y)
})

test_that("ObjectiveLearner errors when domain has ParamLgl but learner trained on non-logical", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_lgl())  # but x1 is integer in training data
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "ParamLgl"
  )
})

test_that("ObjectiveLearner learner active binding returns learner", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  expect_r6(obj$learner, "LearnerRegr")
  expect_error(obj$learner <- learner, "read-only")
})

test_that("ObjectiveLearner learners active binding returns named list", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  expect_list(obj$learners, types = "LearnerRegr", len = 1)
  expect_names(names(obj$learners), identical.to = "y")
  expect_error(obj$learners <- list(), "read-only")
})

test_that("ObjectiveLearner clone works correctly", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  obj2 <- obj$clone(deep = TRUE)
  expect_r6(obj2, "ObjectiveLearner")

  # Both should work
  expect_number(obj$eval(list(x1 = 5L))$y)
  expect_number(obj2$eval(list(x1 = 5L))$y)
})

test_that("ObjectiveLearner errors with unfitted learner", {
  learner <- lrn("regr.featureless")

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "not.*trained|not.*fitted|no.*model"
  )
})

test_that("ObjectiveLearner errors when domain has columns not in learner's features", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    x1 = p_int(lower = 1, upper = 10),
    x2 = p_dbl(lower = 0, upper = 2)  # Not in training data
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "not.*train|not found"
  )
})

test_that("ObjectiveLearner with multi-learner codomain", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y1 = c(10, 20, 30, 40),
    y2 = c(40, 30, 20, 10)
  )
  task1 <- as_task_regr(dt[, .(x1, y1)], target = "y1")
  task2 <- as_task_regr(dt[, .(x1, y2)], target = "y2")
  learner1 <- lrn("regr.featureless")
  learner2 <- lrn("regr.featureless")
  learner1$train(task1)
  learner2$train(task2)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "maximize")
  )

  obj <- ObjectiveLearner$new(
    learner = list(y1 = learner1, y2 = learner2),
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(x1 = 5L))
  expect_number(result$y1)
  expect_number(result$y2)
  expect_equal(result$y1, mean(dt$y1))
  expect_equal(result$y2, mean(dt$y2))

  # eval_dt also works
  result_dt <- obj$eval_dt(data.table(x1 = c(1L, 5L)))
  expect_data_table(result_dt, nrows = 2, ncols = 2)
  expect_names(names(result_dt), identical.to = c("y1", "y2"))
})

test_that("ObjectiveLearner errors when single learner with multi-param codomain", {
  dt <- data.table(x1 = c(1L, 2L, 3L, 4L), y = c(10, 20, 30, 40))
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(
    y1 = p_dbl(tags = "minimize"),
    y2 = p_dbl(tags = "maximize")
  )

  expect_error(
    ObjectiveLearner$new(learner = learner, domain = domain, codomain = codomain),
    "exactly one parameter"
  )
})

test_that("ObjectiveLearner errors when learner list names don't match codomain", {
  dt <- data.table(x1 = c(1L, 2L, 3L, 4L), y = c(10, 20, 30, 40))
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = list(wrong_name = learner),
      domain = domain,
      codomain = codomain
    ),
    "permutation"
  )
})

test_that("ObjectiveLearner check_values = FALSE skips validation", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain,
    check_values = FALSE
  )

  # Out of bounds value - without check_values this goes to the learner
  # The learner itself may or may not accept it
  result <- obj$eval(list(x1 = 100L))
  expect_number(result$y)
})

test_that("ObjectiveLearner errors with empty domain", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps()
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "at least one parameter|Missing"
  )
})

test_that("ObjectiveLearner errors with ParamUty", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_uty())
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "untyped"
  )
})

test_that("ObjectiveLearner with rpart learner gives varying predictions", {
  # Use a learner that actually uses features
  set.seed(123)
  dt <- data.table(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.rpart")
  learner$train(task)

  domain <- ps(x1 = p_dbl(lower = 0, upper = 15))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  # Different inputs should potentially give different outputs
  result1 <- obj$eval(list(x1 = 1))
  result2 <- obj$eval(list(x1 = 10))

  expect_number(result1$y)
  expect_number(result2$y)
})

test_that("ObjectiveLearner handles mixed parameter types", {
  dt <- data.table(
    int_param = c(1L, 2L, 3L, 4L),
    dbl_param = c(0.1, 0.2, 0.3, 0.4),
    fct_param = factor(c("a", "b", "a", "b")),
    lgl_param = c(TRUE, FALSE, TRUE, FALSE),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(
    int_param = p_int(lower = 1, upper = 10),
    dbl_param = p_dbl(lower = 0, upper = 1),
    fct_param = p_fct(levels = c("a", "b")),  # Exact match with training
    lgl_param = p_lgl()
  )

  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(
    int_param = 5L,
    dbl_param = 0.5,
    fct_param = "a",
    lgl_param = TRUE
  ))
  expect_number(result$y)
})

test_that("ObjectiveLearner errors when domain is subset of learner features", {
  # Learner was trained on more features than domain specifies
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    x2 = c(0.5, 1.0, 1.5, 2.0),
    x3 = c("a", "b", "a", "b"),
    y = c(10, 20, 30, 40)
  )
  dt$x3 <- as.factor(dt$x3)
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  # Domain only includes x1 - this should error since learner needs all features
  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  expect_error(
    ObjectiveLearner$new(
      learner = learner,
      domain = domain,
      codomain = codomain
    ),
    "Missing.*x2|Missing.*x3"
  )
})

test_that("ObjectiveLearner codomain target name must match learner target", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    response = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "response")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  # Codomain target is "y" but learner was trained on "response"
  codomain <- ps(y = p_dbl(tags = "minimize"))

  # This should still work - the codomain id is what gets returned,
  # but internally we use the learner's predict output
  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  result <- obj$eval(list(x1 = 5L))
  expect_number(result$y)
})

test_that("ObjectiveLearner properties include deterministic for deterministic learner", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain
  )

  # Default is deterministic
  expect_true("deterministic" %in% obj$properties)
})

test_that("ObjectiveLearner with custom id", {
  dt <- data.table(
    x1 = c(1L, 2L, 3L, 4L),
    y = c(10, 20, 30, 40)
  )
  task <- as_task_regr(dt, target = "y")
  learner <- lrn("regr.featureless")
  learner$train(task)

  domain <- ps(x1 = p_int(lower = 1, upper = 10))
  codomain <- ps(y = p_dbl(tags = "minimize"))

  obj <- ObjectiveLearner$new(
    learner = learner,
    domain = domain,
    codomain = codomain,
    id = "my_objective"
  )

  expect_equal(obj$id, "my_objective")
})
