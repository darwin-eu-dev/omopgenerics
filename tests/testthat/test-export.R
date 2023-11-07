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
  cdmTables <- list(
    "person" = person, "observation_period" = observation_period,
    "vocabulary" = vocabulary, "cdm_source" = cdm_source
  )

  cdm <- cdmReference(
    cdmTables = cdmTables, cohortTables = list(), cdmName = "mock"
  )

  path <- tempdir()
  expect_no_error(export(x = cdm, path = path))
  x <- list.files(path)
  expect_true("cdm_snapshot_mock.csv" %in% x)
  result <- readr::read_csv(
    file = file.path(path, "cdm_snapshot_mock.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_true(all(
    c(
      "result_type", "cdm_name", "cdm_source_name", "cdm_description",
      "cdm_documentation_reference", "cdm_version", "cdm_holder",
      "cdm_release_date", "vocabulary_version", "person_count",
      "observation_period_count", "earliest_observation_period_start_date",
      "latest_observation_period_end_date", "snapshot_date"
    ) %in% colnames(result)
  ))

  expect_identical(result$person_count, "10")
  expect_identical(result$observation_period_count, "11")
  expect_identical(result$earliest_observation_period_start_date, "1990-01-01")
  expect_identical(result$latest_observation_period_end_date, "2022-01-01")

  # test obscure
  expect_no_error(export(x = cdm, path = path, minCellCount = 10))
  result <- readr::read_csv(
    file = file.path(path, "cdm_snapshot_mock.csv"),
    col_types = readr::cols(.default = "c")
  )
  expect_identical(result$person_count, "10")
  expect_identical(result$observation_period_count, "11")
  expect_identical(result$earliest_observation_period_start_date, "1990-01-01")
  expect_identical(result$latest_observation_period_end_date, "2022-01-01")

  expect_no_error(export(x = cdm, path = path, minCellCount = 11))
  result <- readr::read_csv(
    file = file.path(path, "cdm_snapshot_mock.csv"),
    col_types = readr::cols(.default = "c")
  )
  expect_identical(result$person_count, "<11")
  expect_identical(result$observation_period_count, "11")
  expect_identical(result$earliest_observation_period_start_date, "1990-01-01")
  expect_identical(result$latest_observation_period_end_date, "2022-01-01")

  expect_no_error(export(x = cdm, path = path, minCellCount = 12))
  result <- readr::read_csv(
    file = file.path(path, "cdm_snapshot_mock.csv"),
    col_types = readr::cols(.default = "c")
  )
  expect_identical(result$person_count, "<12")
  expect_identical(result$observation_period_count, "<12")
  expect_identical(result$earliest_observation_period_start_date, as.character(NA))
  expect_identical(result$latest_observation_period_end_date, as.character(NA))

  # namePrefix
  expect_no_error(export(x = cdm, path = path, namePrefix = "test"))
  x <- list.files(path)
  expect_true("test_cdm_snapshot_mock.csv" %in% x)

  # resultId
  expect_no_error(export(x = cdm, path = path, resultId = "P2-C1-001"))
  x <- list.files(path)
  expect_true("cdm_snapshot_mock_P2-C1-001.csv" %in% x)
  result <- readr::read_csv(
    file = file.path(path, "cdm_snapshot_mock_P2-C1-001.csv"),
    col_types = readr::cols(.default = "c")
  )
  expect_true(all(
    c(
      "result_id", "result_type", "cdm_name", "cdm_source_name",
      "cdm_description", "cdm_documentation_reference", "cdm_version",
      "cdm_holder", "cdm_release_date", "vocabulary_version", "person_count",
      "observation_period_count", "earliest_observation_period_start_date",
      "latest_observation_period_end_date", "snapshot_date"
    ) %in% colnames(result)
  ))

  cdmTables$cdm_source <- NULL
  cdm <- cdmReference(
    cdmTables = cdmTables, cohortTables = list(), cdmName = "mock2"
  )
  expect_no_error(export(x = cdm, path = path))
  x <- list.files(path)
  expect_true("cdm_snapshot_mock2.csv" %in% x)

})

test_that("export a cohort object", {
  # create a cohort object
  cohort = generatedCohortSet(dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-12-31")
  ))
})
