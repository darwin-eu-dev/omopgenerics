test_that("casting works", {
  name <- "my_custom_table"

  # one column nothing to do
  x <- dplyr::tibble(a = 1L)
  cols <- list(a = "integer")
  expect_no_error(y <- castColumns(x, cols, name))

  # the column must be casted to character
  cols <- list(a = "character")
  expect_warning(y <- castColumns(x, cols, name))
  expect_true(is.character(y$a))

  # only one of the two columns is casted
  cols <- list(a = "character", person_id = "integer")
  x <- dplyr::tibble(x = c(1, 2), person_id = "1", a = "asdf")
  expect_warning(y <- castColumns(x, cols, name))
  expect_true(is.character(y$a))
  expect_true(is.integer(y$person_id))
  expect_true(is.numeric(y$x))

  # cast multiple columns
  cols <- list(a = "character", person_id = "integer", cohort_start_date = "date")
  x <- dplyr::tibble(x = c(1, 2), person_id = "1", a = "asdf", cohort_start_date = "2021-01-01")
  expect_warning(y <- castColumns(x, cols, name))
  expect_true(is.character(y$a))
  expect_true(is.integer(y$person_id))
  expect_true(is.numeric(y$x))
  expect_true(inherits(y$cohort_start_date, "Date"))
})
