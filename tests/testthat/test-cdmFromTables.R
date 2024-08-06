test_that("test cdmFromTables", {
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
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  ) |>
    expect_no_error()

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
  ) |>
    expect_no_error()

  expect_equal(settings(cdm$cohort1), dplyr::tibble(
    "cohort_definition_id" = 1L, "cohort_name" = "cohort_1"
  ))

  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = 1L, "cohort_name" = "my_cohort"
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test",
    cohortTables = list("cohort1" = cohort)
  ) |>
    expect_no_error()
  expect_equal(settings(cdm$cohort1), dplyr::tibble(
    "cohort_definition_id" = 1L, "cohort_name" = "my_cohort"
  ))

  expect_warning(
    cdm <- cdmFromTables(
      tables = list(
        "person" = person, "observation_period" = observation_period,
        "cdm_source" = dplyr::tibble(cdm_source_name = "mocktest")
      ),
      cohortTables = list("cohort1" = cohort)
    )
  )
  expect_identical(cdmName(cdm), "mocktest")

  cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
        race_concept_id = 0L, ethnicity_concept_id = 0L
      ),
      "observation_period" = dplyr::tibble(
        observation_period_id = 1L, person_id = 1L,
        observation_period_start_date = as.Date("2000-01-01"),
        observation_period_end_date = as.Date("2023-12-31"),
        period_type_concept_id = 0L
      ),
      "cdm_source" = dplyr::tibble(
        cdm_source_name = "test", cdm_version = NA_character_
      )
    ),
    cdmName = "mock"
  ) |>
    expect_no_error()

  # overlap between observation periods
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date(c("2000-01-01", "2020-01-01")),
    observation_period_end_date = as.Date(c("2023-12-31", "2020-01-01")),
    period_type_concept_id = 0L
  )
  expect_error(cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  ))

  # start before end date of observation periods
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date(c("2050-01-01", "2020-01-01")),
    observation_period_end_date = as.Date(c("2023-12-31", "2020-01-01")),
    period_type_concept_id = 0L
  )
  expect_error(cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  ))

  # no drug_exposure
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date(c("2000-01-01")),
    observation_period_end_date = as.Date(c("2023-12-31")),
    period_type_concept_id = 0L
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1L,
    person_id = 1L,
    drug_concept_id = 0L,
    drug_exposure_start_date = as.Date("2020-01-01"),
    drug_exposure_end_date = as.Date("2020-01-01")
  )
  expect_warning(cdm <- cdmFromTables(
    tables = list(
      "person" = person, "observation_period" = observation_period,
      "drug_exposure" = drug_exposure
    ),
    cdmName = "test"
  ))
  expect_false("drug_exposure" %in% names(cdm))

  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date(c("2000-01-01")),
    observation_period_end_date = as.Date(c("2023-12-31")),
    period_type_concept_id = 0L
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1L,
    person_id = 1L,
    drug_concept_id = 0L,
    drug_exposure_start_date = as.Date("2020-01-01"),
    drug_exposure_end_date = as.Date("2020-01-01"),
    drug_type_concept_id = 0L
  )
  expect_no_warning(cdm <- cdmFromTables(
    tables = list(
      "person" = person, "observation_period" = observation_period,
      "drug_exposure" = drug_exposure
    ),
    cdmName = "test"
  ))
  expect_true("drug_exposure" %in% names(cdm))
  expect_no_error(cdm[["drug_exposure"]] <- NULL)
  expect_false("drug_exposure" %in% names(cdm))
  expect_no_error(cdm[["drug_exposure"]] <- drug_exposure)
  expect_true("drug_exposure" %in% names(cdm))
  expect_false(inherits(cdm$drug_exposure, "omop_table"))
  expect_no_error(cdm <- insertTable(
    cdm = cdm, name = "drug_exposure", table = drug_exposure
  ))
  expect_true(inherits(cdm$drug_exposure, "omop_table"))
})
