test_that("test getCohortName and getCohortId", {
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = Sys.Date(),
    period_type_concept_id = 0L
  )
  x <- dplyr::tibble(
    cohort_definition_id = 1L, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  )
  attr(x, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 4) |> as.integer(),
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
    c("condition1" = 1L, "drug1" = 2L, "covid" = 3L, "asthma" = 4L)
  )

  expect_identical(
    getCohortId(cdm$my_first_cohort, "drug1"), c("drug1" = 2L)
  )
  expect_identical(
    getCohortId(cdm$my_first_cohort, c("asthma", "covid")),
    c(asthma = 4L, covid = 3L)
  )
  expect_identical(
    getCohortId(cdm$my_first_cohort, c("covid", "asthma")),
    c(covid = 3L, asthma = 4L)
  )
  expect_warning(expect_identical(
    getCohortId(cdm$my_first_cohort, c("covid", "random", "asthma")),
    c(covid = 3L, asthma = 4L)
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

test_that("uniqueId", {
  expect_no_error(uniqueId())
  expect_true(is.character(uniqueId()))
  expect_true(length(uniqueId()) == 1)
  expect_true(length(uniqueId(n = 8)) == 8)
  expect_true(grepl("pref_", uniqueId(prefix = "pref_")))
  expect_true(nchar(uniqueId()) == 6)
  expect_true(nchar(uniqueId(nChar = 4, prefix = "")) == 4)
  xx <- letters[c(-1, -8)]
  expect_identical(
    uniqueId(n = 2, exclude = paste0("m", xx), nChar = 1, prefix = "m"),
    c("ma", "mh")
  )
})

test_that("isTableEmpty", {
  table <- dplyr::tibble(a = "1")

  expect_error(table |> isTableEmpty())

  class(table) <- c("cdm_table", "tbl_df", "tbl", "data.frame")

  expect_false(table |> isTableEmpty())

  table <- dplyr::tibble()

  class(table) <- c("cdm_table", "tbl_df", "tbl", "data.frame")

  expect_true(table |> isTableEmpty())

})

test_that("omopTableFields", {

  expect_no_error(omopTableFields())

  expect_identical(omopTableFields(), omopTableFields("5.3"))

  expect_false(omopTableFields(cdmVersion = "5.4") |> nrow() ==
                     omopTableFields(cdmVersion = "5.3") |> nrow())

  expect_error(omopTableFields(cdmVersion = "5.5"))

})


test_that("resultPackageVersion", {

  x <- dplyr::tibble(
    "result_id" = c(as.integer(1),as.integer(2)),
    "cdm_name" = c("cprd","omock"),
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |> newSummarisedResult()

  expect_invisible(x |> resultPackageVersion())

  x <- dplyr::tibble(
    "result_id" = c(as.integer(1),as.integer(2)),
    "cdm_name" = c("cprd","omock"),
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = c("0.4.0","0.5.0"),
    "group_name" = "cohort_name",
    "group_level" = "cohort1",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |> newSummarisedResult()

  expect_message(x |> resultPackageVersion())
})

test_that("packages versions", {
  x <- emptySummarisedResult(settings = dplyr::tibble(
    result_id = 1:5,
    result_type = "unknown",
    package_name = c(
      "PatientProfiles", "CohortCharacteristics", "visOmopResults",
      "PatientProfiles", "CohortCharacteristics"
    ),
    package_version = c("1.2.0", "0.2.2", "0.3.0", "1.2.0", "0.2.2")
  ))
  expect_snapshot(resultPackageVersion(x))

  x <- emptySummarisedResult(settings = dplyr::tibble(
    result_id = 1:5,
    result_type = "unknown",
    package_name = c(
      "PatientProfiles", "CohortCharacteristics", "visOmopResults",
      "PatientProfiles", "CohortCharacteristics"
    ),
    package_version = c("1.2.0", "0.2.2", "0.3.0", "1.1.0", "0.2.2")
  ))
  expect_snapshot(resultPackageVersion(x))
})
