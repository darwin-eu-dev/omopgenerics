test_that("logging queries does nothing if local cdm", {
  src <- newLocalSource()
  cdmTables <- list(
    "person" = dplyr::tibble(
      person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
      race_concept_id = 0L, ethnicity_concept_id = 0L
    ) |>
      newCdmTable(src, "person"),
    "observation_period" = dplyr::tibble(
      observation_period_id = 1L, person_id = 1L,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2023-12-31"),
      period_type_concept_id = 0L
    ) |>
      newCdmTable(src, "observation_period")
  )
  cdm <- newCdmReference(tables = cdmTables, cdmName = "mock")
  withr::local_options("omopgenerics.log_sql_path" =
            tempdir())
  expect_no_error(result <- cdm$person |>
    dplyr::tally() |>
    dplyr::compute())

  withr::local_options("omopgenerics.log_sql_explain_path" =
                         tempdir())
  expect_no_error(result <- cdm$person |>
                    dplyr::tally() |>
                    dplyr::compute())

})
