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
  person <- dplyr::tibble(
    person_id = c(1, 2), gender_concept_id = 8507, year_of_birth = 1970,
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2), person_id = c(1, 2),
    observation_period_start_date = as.Date("1970-01-01"),
    observation_period_end_date = as.Date("2070-01-01"),
    period_type_concept_id = 0
  )

  cdm <- cdmReference(
    cdmTables = list(
      person = person, observation_period = observation_period,
      cohort1 = cohort1, cohort2 = cohort2
    ),
    cdmName = "MOCK CDM",
    cdmVersion = "5.4"
  )

  # bind both cohorts
  expect_error(bind(cdm$cohort1, 1))
  expect_error(bind(1, cdm$cohort2))
  expect_error(bind(cdm$cohort1, cdm$cohort2, cohortName = "new_cohort"))

  attr(cdm$cohort2, "cohort_set") <- attr(cdm$cohort2, "cohort_set") |>
    dplyr::mutate(cohort_name = paste0("new_", .data$cohort_name))

  expect_error(bind(cdm$cohort1, cdm$cohort2, cohortName = 1))
  expect_error(bind(cdm$cohort1, cdm$cohort2, cohortName = c("ad", "as")))
  expect_no_error(cdm$new_cohort <- bind(cdm$cohort1, cdm$cohort2, cohortName = "new_cohort"))

  expect_true(all(colnames(cdm$new_cohort) %in% c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )))
  expect_true(all(colnames(cohortSet(cdm$new_cohort)) %in% c(
    "cohort_definition_id", "cohort_name"
  )))

  expect_identical(
    cdm$new_cohort |> dplyr::tally() |> dplyr::pull(),
    cdm$cohort1 |> dplyr::tally() |> dplyr::pull() +
      cdm$cohort2 |> dplyr::tally() |> dplyr::pull()
  )
  expect_identical(
    cohortSet(cdm$new_cohort) |> dplyr::tally() |> dplyr::pull(),
    cohortSet(cdm$cohort1) |> dplyr::tally() |> dplyr::pull() +
      cohortSet(cdm$cohort2) |> dplyr::tally() |> dplyr::pull()
  )
  expect_identical(
    cohortCount(cdm$new_cohort) |> dplyr::tally() |> dplyr::pull(),
    cohortCount(cdm$cohort1) |> dplyr::tally() |> dplyr::pull() +
      cohortCount(cdm$cohort2) |> dplyr::tally() |> dplyr::pull()
  )
  expect_identical(
    cohortAttrition(cdm$new_cohort) |> dplyr::tally() |> dplyr::pull(),
    cohortAttrition(cdm$cohort1) |> dplyr::tally() |> dplyr::pull() +
      cohortAttrition(cdm$cohort2) |> dplyr::tally() |> dplyr::pull()
  )

  attr(cdm$cohort2, "cohort_set") <- attr(cdm$cohort2, "cohort_set") |>
    dplyr::mutate(limit = "first")

  expect_no_error(cdm$new_cohort <- bind(cdm$cohort1, cdm$cohort2, cohortName = "new_cohort"))

  expect_true(all(colnames(cdm$new_cohort) %in% c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  )))
  expect_true(all(colnames(cohortSet(cdm$new_cohort)) %in% c(
    "cohort_definition_id", "cohort_name", "limit"
  )))

  expect_identical(
    cdm$new_cohort |> dplyr::tally() |> dplyr::pull(),
    cdm$cohort1 |> dplyr::tally() |> dplyr::pull() +
      cdm$cohort2 |> dplyr::tally() |> dplyr::pull()
  )
  expect_identical(
    cohortSet(cdm$new_cohort) |> dplyr::tally() |> dplyr::pull(),
    cohortSet(cdm$cohort1) |> dplyr::tally() |> dplyr::pull() +
      cohortSet(cdm$cohort2) |> dplyr::tally() |> dplyr::pull()
  )
  expect_identical(
    cohortCount(cdm$new_cohort) |> dplyr::tally() |> dplyr::pull(),
    cohortCount(cdm$cohort1) |> dplyr::tally() |> dplyr::pull() +
      cohortCount(cdm$cohort2) |> dplyr::tally() |> dplyr::pull()
  )
  expect_identical(
    cohortAttrition(cdm$new_cohort) |> dplyr::tally() |> dplyr::pull(),
    cohortAttrition(cdm$cohort1) |> dplyr::tally() |> dplyr::pull() +
      cohortAttrition(cdm$cohort2) |> dplyr::tally() |> dplyr::pull()
  )

})
