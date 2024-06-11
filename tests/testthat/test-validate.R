
test_that("test validateName", {
  expect_error(validateName(name = 1))
  expect_error(validateName(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateName("my_name"))
  expect_message(expect_identical("my_name", validateName("myName")))
  expect_message(expect_message(expect_identical(
    "my_name", validateName("myName", list("my_name" = 1))
  )))
})

test_that("test validateCohort", {
  expect_error(validateCohort(1))
  x <- 1
  class(x) <- "cohort_table"
  expect_error(validateCohort(x))
  x <- 1
  class(x) <- c("cohort_table", "cdm_table")
  expect_no_error(validateCohort(x))
})

test_that("test validateCohortId", {
  cohort <- 1
  class(cohort) <- c("cohort_table", "cdm_table")
  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = c(2, 4), "cohort_name" = c("a", "b")
  )
  expect_error(validateCohortId("adsfd", cohort))
  expect_identical(validateCohortId(2, cohort), 2L)
  expect_identical(validateCohortId(c(2, 4), cohort), c(2L, 4L))
  expect_identical(validateCohortId(c(4L, 2L), cohort), c(4L, 2L))
  expect_error(validateCohortId(1, cohort))
})
