test_that("export a cdm object", {
  person <- dplyr::tibble(
    person_id = 1:10, gender_concept_id = 0, year_of_birth = 1990,
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    person_id = c(1:10, 1),
    observation_period_start_date = as.Date(c(rep("1990-01-01", 10), "2020-01-01")),
    observation_period_end_date = as.Date(c(rep("2015-01-01", 10), "2022-01-01")),
    period_type_concept_id = 0
  ) |>
    dplyr::mutate(observation_period_id = dplyr::row_number())
  vocabulary <- dplyr::tibble(
    vocabulary_id = "None", vocabulary_name = NA, vocabulary_reference = NA,
    vocabulary_concept_id = 0, vocabulary_version = "v5.5.8"
  )
  cdm_source <- dplyr::tibble(
    cdm_source_name = "mock db",
    cdm_holder = "omopverse",
    cdm_release_date = "2023-08-01",
    source_description = "This is a mock data cdm that contains synthetic data",
    source_documentation_reference = "www.omopverse.com"
  )
  #cdm <- newCdmReference()
})

test_that("export a cohort object", {
  # create a cohort object
})
