
test_that("test validateCohort", {
  expect_error(validateCohort(1))
  x <- 1
  class(x) <- "cohort_table"
  expect_error(validateCohort(x))
  x <- 1
  class(x) <- c("cohort_table", "cdm_table")
  expect_no_error(validateCohort(x))
})
