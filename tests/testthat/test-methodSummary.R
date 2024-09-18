test_that("summary a cdm reference", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1L, 2L), person_id = 1L,
    observation_period_start_date = as.Date(c("2000-01-01", "2021-01-01")),
    observation_period_end_date = as.Date(c("2019-12-31", "2022-01-01")),
    period_type_concept_id = 0L
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "mock"
  )
  expect_no_error(summary(cdm))
  cdm <- insertTable(cdm, "cdm_source", dplyr::tibble(cdm_source_name = "test"))
  expect_no_error(x <- summary(cdm))
  expect_equal(
    x$estimate_value[x$estimate_name == "source_name"],
    cdm$cdm_source |> dplyr::pull("cdm_source_name")
  )
  cdm <- insertTable(cdm, "cdm_source", dplyr::tibble(
    cdm_source_name = "test", vocabulary_version = 5.3
  ))
  expect_no_error(x <- summary(cdm))
  expect_equal(
    x$estimate_value[x$estimate_name == "version" & x$variable_name == "vocabulary"],
    cdm$cdm_source |> dplyr::pull("vocabulary_version") |> as.character()
  )
  cdm <- insertTable(cdm, "cdm_source", dplyr::tibble(
    cdm_source_name = "test", cdm_version = 5.3
  ))
  expect_no_error(x <- summary(cdm))
  expect_equal(
    x$estimate_value[x$estimate_name == "version" & x$variable_name == "cdm"],
    cdm$cdm_source |> dplyr::pull("cdm_version") |> as.character()
  )
  cdm <- insertTable(cdm, "cdm_source", dplyr::tibble(
    cdm_source_name = "test", cdm_version = 5.3, cdm_holder = "me",
    vocabulary_version = "5.3.8 AUG 2022", cdm_release_date = Sys.Date(),
    source_description = "this is mock data qith only 1 individual",
    source_documentation_reference = "www.omopgenerics.com"
  ))
  expect_no_error(x <- summary(cdm))
  expt <- dplyr::tibble(
    variable_name = c(
      "snapshot_date", "person_count", "observation_period_count",
      "cdm", "vocabulary", rep("cdm", 5), "observation_period_start_date",
      "observation_period_end_date"
    ),
    estimate_name = c(
      "value", "count", "count", "source_name", "version", "version",
      "holder_name", "release_date", "description", "documentation_reference",
      "min", "max"
    ),
    column  = c(
      rep(NA_character_, 3), "cdm_source_name", "vocabulary_version",
      "cdm_version", "cdm_holder", "cdm_release_date", "source_description",
      "source_documentation_reference", rep(NA_character_, 2)
    ),
    value = c(
      as.character(Sys.Date()), "1", "2", rep(NA_character_, 7),
      "2000-01-01", "2022-01-01"
    )
  )
  for (k in seq_len(nrow(expt))) {
    if (is.na(expt$column[k])) {
      value <- expt$value[k]
    } else {
      value <- cdm$cdm_source |>
        dplyr::pull(dplyr::all_of(expt$column[k])) |>
        as.character()
    }
    expect_equal(
      x$estimate_value[x$estimate_name == expt$estimate_name[k] & x$variable_name == expt$variable_name[k]],
      value
    )
  }

  expect_identical(
    x$estimate_value[x$estimate_name == "source_type" & x$variable_name == "cdm"],
    "local"
  )

})

test_that("summary a generated cohort set", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2022-12-31"),
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
    cohortTables = list("cohort1" = cohort, "cohort2" = cohort)
  )
  expect_no_error(summary(cdm$cohort1))
  expect_no_error(summary(cdm$cohort2))
  cdm$cohort2 <- cdm$cohort2 |>
    newCohortTable(cohortSetRef = dplyr::tibble(
      cohort_definition_id = 1L, cohort_name = "my_cohort", parameter = 1
    ))
  expect_no_error(cdm <- bind(cdm$cohort1, cdm$cohort2, name = "cohort3"))
  expect_no_error(summary(cdm$cohort3))

  x <- settings(summary(cdm$cohort3))
  expect_true(inherits(x, "data.frame"))
  expect_equal(
    x |>
      dplyr::select("cohort_definition_id", "parameter") |>
      dplyr::distinct(),
    settings(cdm$cohort3) |>
      dplyr::select(!"cohort_name")
  )

})
