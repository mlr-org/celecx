
test_that("ConfigurableComponent initialization with id", {
  comp <- ConfigurableComponent$new(id = "test_id")
  expect_r6(comp, "ConfigurableComponent")
  expect_identical(comp$id, "test_id")
  expect_identical(comp$format(), "<ConfigurableComponent:test_id>")
})

test_that("ConfigurableComponent initialization without id", {
  comp <- ConfigurableComponent$new()
  expect_r6(comp, "ConfigurableComponent")
  expect_null(comp$id)
  expect_identical(comp$format(), "<ConfigurableComponent>")
})

test_that("ConfigurableComponent with ParamSet", {
  pars <- ps(
    x = p_dbl(lower = 0, upper = 1),
    y = p_int(lower = 1, upper = 10)
  )
  comp <- ConfigurableComponent$new(id = "with_params", param_set = pars)
  expect_r6(comp$param_set, "ParamSet")
  expect_identical(comp$param_set, pars)
  expect_setequal(comp$param_set$ids(), c("x", "y"))
})

test_that("ConfigurableComponent with NULL param_set", {
  comp <- ConfigurableComponent$new(id = "no_params", param_set = NULL)
  expect_null(comp$param_set)
})

test_that("ConfigurableComponent id is mutable when set", {
  comp <- ConfigurableComponent$new(id = "original")
  expect_identical(comp$id, "original")
  comp$id <- "modified"
  expect_identical(comp$id, "modified")
})

test_that("ConfigurableComponent id is read-only when not set", {
  comp <- ConfigurableComponent$new()
  expect_error(comp$id <- "new_id", "id is read-only")
})

test_that("ConfigurableComponent param_set is read-only", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(param_set = pars)
  new_pars <- ps(y = p_int(lower = 1, upper = 5))
  expect_error(comp$param_set <- new_pars, "param_set is read-only")
})

test_that("ConfigurableComponent configure sets parameter values", {
  pars <- ps(
    x = p_dbl(lower = 0, upper = 1),
    y = p_int(lower = 1, upper = 10)
  )
  comp <- ConfigurableComponent$new(id = "config_test", param_set = pars)
  comp$configure(x = 0.5, y = 5L)
  expect_identical(comp$param_set$values$x, 0.5)
  expect_identical(comp$param_set$values$y, 5L)
})

test_that("ConfigurableComponent configure with .values argument", {
  pars <- ps(
    x = p_dbl(lower = 0, upper = 1),
    y = p_int(lower = 1, upper = 10)
  )
  comp <- ConfigurableComponent$new(id = "config_test", param_set = pars)
  comp$configure(.values = list(x = 0.7, y = 3L))
  expect_identical(comp$param_set$values$x, 0.7)
  expect_identical(comp$param_set$values$y, 3L)
})

test_that("ConfigurableComponent configure rejects duplicate names and overlaps", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(param_set = pars)
  duplicate_values <- structure(list(0.1, 0.2), names = c("x", "x"))
  expect_error(comp$configure(.values = duplicate_values), "unique")
  expect_error(comp$configure(x = 0.1, .values = list(x = 0.2)), "disjunct")
})

test_that("ConfigurableComponent configure rejects invalid parameter", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(param_set = pars)
  expect_error(comp$configure(z = 99), "Cannot set argument 'z'")
})

test_that("ConfigurableComponent configure returns self invisibly", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(param_set = pars)
  result <- withVisible(comp$configure(x = 0.5))
  expect_false(result$visible)
  expect_identical(result$value, comp)
})

test_that("ConfigurableComponent print with id", {
  comp <- ConfigurableComponent$new(id = "print_test")
  output <- capture.output(comp$print())
  expect_match(output, "<ConfigurableComponent:print_test>")
})

test_that("ConfigurableComponent print without id", {
  comp <- ConfigurableComponent$new()
  output <- capture.output(comp$print())
  expect_match(output, "<ConfigurableComponent>")
})

test_that("ConfigurableComponent hash is read-only", {
  comp <- ConfigurableComponent$new(id = "hash_test")
  expect_error(comp$hash <- "fake_hash", "hash is read-only")
})

test_that("ConfigurableComponent phash is read-only", {
  comp <- ConfigurableComponent$new(id = "phash_test")
  expect_error(comp$phash <- "fake_phash", "phash is read-only")
})

test_that("ConfigurableComponent hash changes with parameter values", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(id = "hash_test", param_set = pars)
  hash1 <- comp$hash
  comp$configure(x = 0.5)
  hash2 <- comp$hash
  expect_false(identical(hash1, hash2))
})

test_that("ConfigurableComponent hash works without ParamSet", {
  comp <- ConfigurableComponent$new(id = "no_param_set", param_set = NULL)
  expect_type(comp$hash, "character")
  expect_gt(nchar(comp$hash), 0)
})

test_that("ConfigurableComponent param_set sources build ParamSetCollection lazily", {
  comp <- ConfigurableComponent$new(
    param_set = list(
      quote(ps(a = p_dbl(lower = 0, upper = 1))),
      quote(ps(b = p_int(lower = 1, upper = 3)))
    )
  )
  expect_setequal(comp$param_set$ids(), c("a", "b"))
  expect_true(inherits(comp$param_set, "ParamSetCollection"))
  comp$configure(a = 0.3, b = 2L)
  expect_identical(comp$param_set$values$a, 0.3)
  expect_identical(comp$param_set$values$b, 2L)
})

test_that("ConfigurableComponent phash is stable across parameter changes", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(id = "phash_test", param_set = pars)
  phash1 <- comp$phash
  comp$configure(x = 0.5)
  phash2 <- comp$phash
  expect_identical(phash1, phash2)
})

test_that("ConfigurableComponent phash differs with different ids", {
  comp1 <- ConfigurableComponent$new(id = "id1")
  comp2 <- ConfigurableComponent$new(id = "id2")
  expect_false(identical(comp1$phash, comp2$phash))
})

test_that("ConfigurableComponent phash updates when id changes", {
  comp <- ConfigurableComponent$new(id = "initial")
  phash1 <- comp$phash
  comp$id <- "updated"
  expect_false(identical(phash1, comp$phash))
})

test_that("ConfigurableComponent clone works", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(id = "original", param_set = pars)
  comp$configure(x = 0.5)

  clone <- comp$clone(deep = TRUE)
  expect_identical(comp$id, clone$id)
  expect_identical(comp$param_set$values, clone$param_set$values)
  expect_identical(comp$hash, clone$hash)

  # Modify clone shouldn't affect original
  clone$configure(x = 0.9)
  expect_false(identical(comp$param_set$values$x, clone$param_set$values$x))
})

test_that("hash_transform.ConfigurableComponent returns phash", {
  comp <- ConfigurableComponent$new(id = "hash_transform_test")
  expect_identical(hash_transform(comp), comp$phash)
})

test_that("hash_transform.list calls hash_list", {
  lst <- list(a = 1, b = 2)
  result <- hash_transform(lst)
  expect_type(result, "character")
})

test_that("hash_transform.default keeps objects and list hashing composes", {
  expect_identical(hash_transform(1:3), 1:3)
  hash_nested <- hash_transform(list(a = 1, b = list(c = 2)))
  expect_type(hash_nested, "character")
  expect_type(hash_list(list(hash_transform(1), hash_transform(list(c = 2)))), "character")
})

test_that("hash_transform.R6 with phash field", {
  comp <- ConfigurableComponent$new(id = "r6_test")
  result <- hash_transform.R6(comp)
  expect_identical(result, comp$phash)
})

test_that("hash_transform.R6 without phash field errors", {
  # Create a simple R6 object without phash
  SimpleR6 <- R6Class("SimpleR6", public = list(value = NULL))
  obj <- SimpleR6$new()
  expect_error(hash_transform.R6(obj), "Cannot hash R6 object without phash field")
})

test_that("ConfigurableComponent additional_phash_input validation", {
  # Create a subclass with a public field
  TestComponent <- R6Class("TestComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom_field = NULL
    )
  )

  comp <- TestComponent$new(id = "test", additional_phash_input = "custom_field")
  expect_r6(comp, "TestComponent")
})

test_that("ConfigurableComponent additional_phash_input rejects invalid fields", {
  expect_error(
    ConfigurableComponent$new(additional_phash_input = "nonexistent_field"),
    "additional_phash_input.*subset"
  )
})

test_that("ConfigurableComponent phash reflects additional_phash_input fields and class", {
  PhashComponent <- R6Class("PhashComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom_field = NULL
    )
  )
  c1 <- PhashComponent$new(id = "a", additional_phash_input = "custom_field")
  c2 <- PhashComponent$new(id = "a", additional_phash_input = "custom_field")
  c1$custom_field <- 1
  c2$custom_field <- 1
  expect_identical(c1$phash, c2$phash)
  c2$custom_field <- 2
  expect_false(identical(c1$phash, c2$phash))

  OtherComponent <- R6Class("OtherPhashComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom_field = NULL
    )
  )
  other <- OtherComponent$new(id = "a", additional_phash_input = "custom_field")
  other$custom_field <- 1
  expect_false(identical(c1$phash, other$phash))
})

test_that("ConfigurableComponent additional_configuration validation", {
  # Create a subclass with a public field
  TestComponent <- R6Class("TestComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom_config = NULL
    )
  )

  comp <- TestComponent$new(id = "test", additional_configuration = "custom_config")
  expect_r6(comp, "TestComponent")
})

test_that("ConfigurableComponent additional_configuration rejects duplicates and reserved names", {
  TestComponent <- R6Class("TestComponentConfig",
    inherit = ConfigurableComponent,
    public = list(
      custom_config = NULL
    )
  )
  expect_error(TestComponent$new(additional_configuration = c("custom_config", "custom_config")), "duplicated")
  expect_error(TestComponent$new(additional_configuration = "format"), "disjunct")
})

test_that("ConfigurableComponent additional_configuration rejects parameters", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  expect_error(
    ConfigurableComponent$new(param_set = pars, additional_configuration = "x"),
    "subset"
  )
})

test_that("ConfigurableComponent additional_configuration alone does not affect hash or phash", {
  ConfigComponent <- R6Class("ConfigComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom_config = NULL
    )
  )
  c1 <- ConfigComponent$new(id = "cfg", additional_configuration = "custom_config")
  c2 <- ConfigComponent$new(id = "cfg", additional_configuration = "custom_config")
  c1$configure(custom_config = 1)
  c2$configure(custom_config = 2)
  expect_identical(c1$phash, c2$phash)
  expect_identical(c1$hash, c2$hash)
})

test_that("ConfigurableComponent additional_phash_input can reflect derived configuration data", {
  DerivedComponent <- R6Class("DerivedComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom_config = NULL,
      set_config = function(value) {
        self$custom_config <- value
        private$.raw_config <- value
      }
    ),
    private = list(
      .raw_config = NULL
    )
  )

  c1 <- DerivedComponent$new(
    id = "cfg",
    additional_configuration = "custom_config",
    additional_phash_input = ".raw_config"
  )
  c2 <- DerivedComponent$new(
    id = "cfg",
    additional_configuration = "custom_config",
    additional_phash_input = ".raw_config"
  )

  c1$set_config(1)
  c2$set_config(2)
  expect_false(identical(c1$phash, c2$phash))
  expect_false(identical(c1$hash, c2$hash))
})

test_that("ConfigurableComponent configure with empty values returns self", {
  comp <- ConfigurableComponent$new()
  result <- comp$configure()
  expect_identical(result, comp)
})

test_that("ConfigurableComponent configure is atomic with invalid keys", {
  pars <- ps(x = p_dbl(lower = 0, upper = 1))
  comp <- ConfigurableComponent$new(param_set = pars)
  expect_error(comp$configure(x = 0.2, unknown = 1), "Cannot set argument 'unknown'")
  expect_null(comp$param_set$values$x)
})

test_that("ConfigurableComponent configure can set public fields", {
  FieldComponent <- R6Class("FieldComponent",
    inherit = ConfigurableComponent,
    public = list(
      custom = NULL
    )
  )
  comp <- FieldComponent$new()
  comp$configure(custom = 42)
  expect_identical(comp$custom, 42)
})

test_that("ConfigurableComponent id validation rejects empty string", {
  comp <- ConfigurableComponent$new(id = "valid")
  expect_error(comp$id <- "", "at least 1")
})
