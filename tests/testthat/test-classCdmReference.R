test_that("test cdm_reference", {

  cdmTables <- list(
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
  )

  expect_no_error(x <- cdmReference(
    cdmTables = cdmTables,
    cdmName = "mock",
    sourceCdm = "source"
  ))

})
