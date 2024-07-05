
test_that("test validateName", {
  expect_error(validateName(name = 1))
  expect_error(validateName(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateName("my_name"))
  expect_error(validateName("myName"))
  expect_warning(expect_identical("my_name", validateName("myName", validation = "warning")))
  expect_warning(expect_warning(expect_identical(
    "my_name", validateName("myName", list("my_name" = 1), validation = "warning")
  )))
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
