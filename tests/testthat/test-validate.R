
test_that("test validateNameArgument", {
  expect_error(validateNameArgument(name = 1))
  expect_error(validateNameArgument(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateNameArgument("my_name"))
  expect_error(validateNameArgument("myName"))
  expect_warning(expect_identical("my_name", validateNameArgument("myName", validation = "warning")))
  expect_warning(expect_warning(expect_identical(
    "my_name", validateNameArgument("myName", list("my_name" = 1), validation = "warning")
  )))
})

test_that("test validateCohortIdArgument", {
  cohort <- 1
  class(cohort) <- c("cohort_table", "cdm_table")
  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = c(2, 4), "cohort_name" = c("a", "b")
  )
  expect_error(validateCohortIdArgument("adsfd", cohort))
  expect_identical(validateCohortIdArgument(2, cohort), 2L)
  expect_identical(validateCohortIdArgument(c(2, 4), cohort), c(2L, 4L))
  expect_identical(validateCohortIdArgument(c(4L, 2L), cohort), c(4L, 2L))
  expect_error(validateCohortIdArgument(1, cohort))
})


test_that("test validateWindowArgument", {

  window <- c(0, 1)
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, 1), c(2, 3))
  expect_no_error(validateWindowArgument(window))
  window <- list(c("a", 1))
  expect_error(validateWindowArgument(window))
  window <- list("window" = c(0, 1), "window2" = c(-1, 1))
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, -1))
  expect_error(validateWindowArgument(window))
  window <- list(c(-Inf, -Inf))
  expect_error(validateWindowArgument(window))
  window <- list(c(Inf, Inf))
  expect_error(validateWindowArgument(window))

  #window name check
  window <- list(c(-1, 1))

  window <- window |> validateWindowArgument(snakeCase = FALSE)

  expect_true(names(window) == "-1 to 1")

  window <- list(c(-1, 1))

  window <- window |> validateWindowArgument(snakeCase = TRUE)

  expect_true(names(window) == "m1_to_1")


  window <- list("window" = c(-1, 1))
  window <- window |> validateWindowArgument(snakeCase = TRUE)
  expect_true(names(window) == "window")

})
