test_that("utilities", {
  string <- c(
    "foo bar", "fooBar", "FooBar", "foo_bar", "FOO BAR", "foo__BAR", "foo(bar)",
    "__FOO__Bar____", "foo####bar"
  )
  expect_no_error(snake_case <- toSnakeCase(string))
  expect_true(all(snake_case == "foo_bar"))
  expect_no_error(camelCase <- toCamelCase(string))
  expect_true(all(camelCase == "fooBar"))

  string <- c("12345", "_", "", "foo123baa", "123foo", "bar12HHHJ", "#'[")
  expect_no_error(snake_case <- toSnakeCase(string))
  expect_true(all(snake_case == c(
    "12345", "_", "", "foo123baa", "123foo", "bar12_hhhj", "_"
  )))
  expect_no_error(camelCase <- toCamelCase(string))
  expect_true(all(camelCase == c(
    "12345", "_", "", "foo123baa", "123foo", "bar12Hhhj", "_"
  )))

  expect_no_error(camelCase <- toCamelCase(as.character(NA)))
  expect_true(all(camelCase == ""))
})

test_that("temporary behaviour", {
  expect_true(is.null(getOption("intermediate_as_temp")))
  expect_true(is.null(getOption("cohort_as_temp")))

  setTemporary()
  expect_true(getOption("intermediate_as_temp") == TRUE)
  expect_true(getOption("cohort_as_temp") == FALSE)

  setTemporary(intermediateAsTemp = FALSE)
  expect_true(getOption("intermediate_as_temp") == FALSE)
  expect_true(getOption("cohort_as_temp") == FALSE)

  setTemporary(cohortAsTemp = TRUE)
  expect_true(getOption("intermediate_as_temp") == TRUE)
  expect_true(getOption("cohort_as_temp") == TRUE)

  setTemporary(intermediateAsTemp = FALSE, cohortAsTemp = TRUE)
  expect_true(getOption("intermediate_as_temp") == FALSE)
  expect_true(getOption("cohort_as_temp") == TRUE)

})

test_that("test asType", {
  expect_warning(asType(1, "adfsgagd"))
  expect_true(1 == asType(1, "float"))
})
