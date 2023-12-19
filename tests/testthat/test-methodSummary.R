test_that("summary a cdm reference", {
  cdm <- cdmReference(
    cdmTables = list(
      "person" = dplyr::tibble(
        person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
        race_concept_id = 0, ethnicity_concept_id = 0
      ),
      "observation_period" = dplyr::tibble(
        observation_period_id = 1, person_id = 1,
        observation_period_start_date = as.Date("2000-01-01"),
        observation_period_end_date = as.Date("2025-12-31"),
        period_type_concept_id = 0
      )
    ),
    cdmName = "mock"
  )
  expect_no_error(summary(cdm))
})

test_that("summary a generated cohort set", {
  cdm <- cdmReference(
    cdmTables = list(
      "person" = dplyr::tibble(
        person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
        race_concept_id = 0, ethnicity_concept_id = 0
      ),
      "observation_period" = dplyr::tibble(
        observation_period_id = 1, person_id = 1,
        observation_period_start_date = as.Date("2000-01-01"),
        observation_period_end_date = as.Date("2025-12-31"),
        period_type_concept_id = 0
      )
    ),
    cohortTables = list(
      "cohort1" = dplyr::tibble(
        cohort_definition_id = 1,
        subject_id = 1,
        cohort_start_date = as.Date("2010-01-01"),
        cohort_end_date = as.Date("2010-01-05")
      )
    ),
    cdmName = "mock"
  )
  expect_no_error(summary(cdm$cohort1))
})
