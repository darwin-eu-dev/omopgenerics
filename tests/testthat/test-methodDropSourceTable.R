test_that("dropSourceTable", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cohort <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-01")
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test",
    cohortTables = list("cohort1" = cohort)
  )

  expect_true("cohort1" %in% names(cdm))
  expect_no_error(cdm <- dropSourceTable(cdm = cdm, name = "cohort1"))
  expect_true(!"cohort1" %in% names(cdm))

  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test",
    cohortTables = list("cohort1" = cohort)
  )

  expect_true("cohort1" %in% names(cdm))
  expect_no_error(cdm <- dropSourceTable(cdm = cdm, name = dplyr::starts_with("cohort1")))
  expect_true(!"cohort1" %in% names(cdm))

})

