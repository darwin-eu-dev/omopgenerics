test_that("test create cdm_cohort", {
  x <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  expect_error(newCdmCohort(x))

})
