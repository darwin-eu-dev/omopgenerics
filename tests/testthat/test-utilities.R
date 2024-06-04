test_that("test getCohortName and getCohortId", {
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
  x <- dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  )
  attr(x, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 4),
    cohort_name = c("condition1", "drug1", "covid", "asthma")
  )
  y <- dplyr::tibble(
    cohort_definition_id = 1:5, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cohortTables = list("my_first_cohort" = x, "my_second_cohort" = y),
    cdmName = "test"
  )

  expect_identical(
    getCohortId(cdm$my_first_cohort),
    c("condition1" = 1, "drug1" = 2, "covid" = 3, "asthma" = 4)
  )

  expect_identical(
    getCohortId(cdm$my_first_cohort, "drug1"), c("drug1" = 2)
  )
  expect_identical(
    getCohortId(cdm$my_first_cohort, c("asthma", "covid")),
    c(asthma = 4, covid = 3)
  )
  expect_identical(
    getCohortId(cdm$my_first_cohort, c("covid", "asthma")),
    c(covid = 3, asthma = 4)
  )
  expect_warning(expect_identical(
    getCohortId(cdm$my_first_cohort, c("covid", "random", "asthma")),
    c(covid = 3, asthma = 4)
  ))
  expect_warning(getCohortId(cdm$my_first_cohort, "random"))

  expect_identical(
    getCohortName(cdm$my_first_cohort),
    c("1" = "condition1", "2" = "drug1", "3" = "covid", "4" = "asthma")
  )
  expect_identical(
    getCohortName(cdm$my_second_cohort, 1), c("1" = "cohort_1")
  )
  expect_identical(
    getCohortName(cdm$my_second_cohort, 1L), c("1" = "cohort_1")
  )
  expect_identical(
    getCohortName(cdm$my_second_cohort, c(4, 2)),
    c("4" = "cohort_4", "2" = "cohort_2")
  )
  expect_warning(expect_identical(
    getCohortName(cdm$my_second_cohort, c(6:1)),
    c("5" = "cohort_5", "4" = "cohort_4", "3" = "cohort_3", "2" = "cohort_2",
      "1" = "cohort_1")
  ))
  expect_warning(getCohortName(cdm$my_second_cohort, 8))
  expect_error(getCohortName(cdm$my_second_cohort, "1"))

})

test_that("test getPersonIdentifier", {
  expect_identical(
    getPersonIdentifier(dplyr::tibble("person_id" = 1L, "a" = "a", "sdas" = 3)),
    "person_id"
  )
  expect_identical(
    getPersonIdentifier(dplyr::tibble("subject_id" = 1L, "a" = "a")),
    "subject_id"
  )
  expect_error(
    getPersonIdentifier(dplyr::tibble("subject_id" = 1L, "person_id" = "a"))
  )
  expect_error(getPersonIdentifier(dplyr::tibble("y" = 1L, "x" = "a")))
})
