test_that("multiplication works", {
  person <- dplyr::tibble(
    person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1, person_id = 1,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2025-12-31"),
    period_type_concept_id = 0
  )
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = 1,
    cohort_start_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01")),
    cohort_end_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01")),
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test",
    cohortTables = list("cohort1" = cohort)
  )

  expect_equal(cohortCount(cdm$cohort1)$number_records, c(3, 1))
  expect_equal(cohortCount(cdm$cohort1)$number_subjects, c(1, 1))

  date <- as.Date("2021-06-01")

  cdm$cohort1 <- cdm$cohort1 |>
    dplyr::filter(cohort_start_date < date) |>
    dplyr::compute(name = "cohort1", temporary = FALSE) |>
    recordCohortAttrition(reason = "Before {date}") |>
    expect_no_error()

  expect_equal(cdm$cohort1 |> dplyr::tally() |> dplyr::pull(), 2)
  expect_true(paste0("Before ", date) %in% attrition(cdm$cohort1)$reason)

  expect_equal(cohortCount(cdm$cohort1)$number_records, c(2, 0))
  expect_equal(cohortCount(cdm$cohort1)$number_subjects, c(1, 0))
  att <- attrition(cdm$cohort1)
  expect_equal(nrow(att), 4)
  expect_equal(
    att |> dplyr::as_tibble(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 2),
      number_records = c(3, 2, 1, 0),
      number_subjects = c(1, 1, 1, 0),
      reason_id = c(1, 2, 1, 2),
      reason = rep(c("Initial qualifying events", "Before 2021-06-01"), 2),
      excluded_records = c(0, 1, 0, 1),
      excluded_subjects = c(0, 0, 0, 1)
    )
  )

  cdm$cohort1 <- cdm$cohort1 |>
    dplyr::group_by(cohort_definition_id, subject_id) |>
    dplyr::filter(cohort_start_date == min(cohort_start_date)) |>
    dplyr::ungroup() |>
    dplyr::compute(name = "cohort1", temporary = FALSE) |>
    recordCohortAttrition(reason = "First record") |>
    expect_no_error()

  expect_equal(cdm$cohort1 |> dplyr::tally() |> dplyr::pull(), 1)

  expect_equal(cohortCount(cdm$cohort1)$number_records, c(1, 0))
  expect_equal(cohortCount(cdm$cohort1)$number_subjects, c(1, 0))
  att <- attrition(cdm$cohort1)
  expect_equal(nrow(att), 6)
  expect_equal(
    att |> dplyr::as_tibble(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2, 2),
      number_records = c(3, 2, 1, 1, 0, 0),
      number_subjects = c(1, 1, 1, 1, 0, 0),
      reason_id = c(1, 2, 3, 1, 2, 3),
      reason = rep(c("Initial qualifying events", "Before 2021-06-01", "First record"), 2),
      excluded_records = c(0, 1, 1, 0, 1, 0),
      excluded_subjects = c(0, 0, 0, 0, 1, 0)
    )
  )

  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test",
    cohortTables = list("cohort1" = cohort, "cohort2" = cohort)
  )

  expect_equal(cohortCount(cdm$cohort1)$number_records, c(3, 1))
  expect_equal(cohortCount(cdm$cohort1)$number_subjects, c(1, 1))

  cdm$cohort1 <- cdm$cohort1 |>
    dplyr::filter(
      cohort_start_date < as.Date("2021-06-01") | cohort_definition_id != 1
    ) |>
    dplyr::compute(name = "cohort1", temporary = FALSE) |>
    recordCohortAttrition(reason = "Before june 2021", cohortId = 1) |>
    expect_no_error()

  expect_equal(cdm$cohort1 |> dplyr::tally() |> dplyr::pull(), 3)

  expect_equal(cohortCount(cdm$cohort1)$number_records, c(2, 1))
  expect_equal(cohortCount(cdm$cohort1)$number_subjects, c(1, 1))
  att <- attrition(cdm$cohort1)
  expect_equal(nrow(att), 3)
  expect_equal(
    att |> dplyr::as_tibble(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 2),
      number_records = c(3, 2, 1),
      number_subjects = c(1, 1, 1),
      reason_id = c(1, 2, 1),
      reason = c("Initial qualifying events", "Before june 2021", "Initial qualifying events"),
      excluded_records = c(0, 1, 0),
      excluded_subjects = c(0, 0, 0)
    )
  )

  cdm$cohort1 <- cdm$cohort1 |>
    dplyr::group_by(cohort_definition_id, subject_id) |>
    dplyr::filter(cohort_start_date == min(cohort_start_date)) |>
    dplyr::ungroup() |>
    dplyr::compute(name = "cohort1", temporary = FALSE) |>
    recordCohortAttrition(reason = "First record") |>
    expect_no_error()

  expect_equal(cdm$cohort1 |> dplyr::tally() |> dplyr::pull(), 2)

  expect_equal(cohortCount(cdm$cohort1)$number_records, c(1, 1))
  expect_equal(cohortCount(cdm$cohort1)$number_subjects, c(1, 1))
  att <- attrition(cdm$cohort1)
  expect_equal(nrow(att), 5)
  expect_equal(
    att |> dplyr::as_tibble(),
    dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2),
      number_records = c(3, 2, 1, 1, 1),
      number_subjects = c(1, 1, 1, 1, 1),
      reason_id = c(1, 2, 3, 1, 2),
      reason = c(
        "Initial qualifying events", "Before june 2021", "First record",
        "Initial qualifying events", "First record"
      ),
      excluded_records = c(0, 1, 1, 0, 0),
      excluded_subjects = c(0, 0, 0, 0, 0)
    )
  )

  dose1 <- 1
  dose2 <- 2
  x <- rep(1, 5)
  xx <- rep("abcd", 100) |> paste0(collapse = " ")

  expect_no_error(
    cdm$cohort2 <- cdm$cohort2 |>
      recordCohortAttrition("At least {dose1} dose{?s}", 1) |>
      recordCohortAttrition("At least {dose2} dose{?s}", 1) |>
      recordCohortAttrition("At least {length(x)} dose{?s}", 1) |>
      recordCohortAttrition("{xx}", 1)
  )

  expect_true(nrow(attrition(cdm$cohort2)) == 6)
  a1 <- attrition(cdm$cohort2) |>
    dplyr::filter(.data$cohort_definition_id == 1)
  expect_true(nrow(a1) == 5)
  expect_true(all(a1 |> dplyr::pull("reason") == c(
    "Initial qualifying events", "At least 1 dose", "At least 2 doses",
    "At least 5 doses", xx
  )))

})
