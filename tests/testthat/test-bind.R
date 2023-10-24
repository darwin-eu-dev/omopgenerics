test_that("bind no class", {
  x <- 1
  expect_error(bind(x))
})

test_that("bind generated_cohort_set", {
  # create mock data
  cohort1 <- generatedCohortSet(
    cohortTable = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 3),
      subject_id = c(1, 1, 1, 2),
      cohort_start_date = as.Date(
        c(1257, 4582, 1535, 2546), origin = "1970-01-01"
      )
    ) |>
      dplyr::mutate(
        cohort_end_date = cohort_start_date + c(651, 378, 637, 654)
      ),
    cohortSetTable = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("a", "b", "c")
    ),
    cohortName = "cohort1"
  )
  cohort2 <- generatedCohortSet(
    cohortTable = dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 3),
      subject_id = c(1, 1, 1, 2),
      cohort_start_date = as.Date(
        c(1535, 2587, 3695, 10000), origin = "1970-01-01"
      )
    ) |>
      dplyr::mutate(
        cohort_end_date = cohort_start_date + c(637, 120, 0, 12)
      ),
    cohortSetTable = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("b", "c", "d")
    ),
    cohortName = "cohort2"
  )

  cdm <- cdmReference(
    cdmTables = list(cohort1 = cohort1, cohort2 = cohort2),
    cdmName = "MOCK CDM",
    cdmVersion = "5.4"
  )

  # bind both cohorts
  expect_error(bind(cdm$cohort1, 1))

})
