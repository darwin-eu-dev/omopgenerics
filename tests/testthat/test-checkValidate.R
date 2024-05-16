test_that("snake case validation", {
  variable <- c("NotSnakeCase", "snake_case", "MyDummyVaRiable1")
  string1 <- c("asfdafc", "my_variable")

  expect_error(checkSnakeCase(variable))
  expect_no_error(checkSnakeCase(string1))

  expect_warning(checkSnakeCase(variable, type = "warning"))
  expect_no_warning(checkSnakeCase(string1, type = "warning"))

  expect_message(x <- validateSnakeCase(variable))
  expect_identical(x, c("not_snake_case", "snake_case", "my_dummy_va_riable1"))

  expect_no_message(x <- validateSnakeCase(string1))
  expect_identical(x, string1)

})


