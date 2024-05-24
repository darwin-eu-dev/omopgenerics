
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
