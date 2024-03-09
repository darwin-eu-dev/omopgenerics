test_that("test codelist from cohort", {
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
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1,2),
    subject_id = c(1,1),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  cdm <- cdmFromTables(
    tables = list(
      "person" = person,
      "observation_period" = observation_period,
      "cohort1" = cohort1
    ),
    cdmName = "test"
  )
  cdm$cohort1 <- newCohortTable(table = cdm$cohort1)
  # empty by default
  expect_warning(cohortCodelist(cdm$cohort1, cohortId = 1))
  expect_warning(cohortCodelist(cdm$cohort1, cohortId = 1,
                                    type = "index event"))

  # with attribute added
  cdm$cohort1 <- newCohortTable(table = cdm$cohort1,
                                cohortCodelistRef = dplyr::tibble(
                                  cohort_definition_id = c(1,1,1,2,2),
                                  codelist_name =c("disease X", "disease X", "disease X",
                                                   "disease Y", "disease Y"),
                                  concept_id = c(1,2,3,4,5),
                                  type = rep("index event", 5)
                                ))

  # only works for a specific cohort definition id
  codes_used_1 <- cohortCodelist(cdm$cohort1, cohortId = 1,
                                     type = "index event")
  expect_true("codelist" %in% class(codes_used_1))
  expect_equal(omopgenerics::newCodelist(list("disease X" = c(1,2,3))),
               codes_used_1)

  expect_warning(cohortCodelist(cdm$cohort1,
                                     cohortId = 1,
                                     type = "exit criteria")) # none with this type
  expect_error(cohortCodelist(cdm$cohort1,
                     cohortId = 1,
                     type = "another criteria"))

  codes_used_2 <- cohortCodelist(cdm$cohort1, cohortId = 2)
  expect_true("codelist" %in% class(codes_used_2))
  expect_equal(omopgenerics::newCodelist(list("disease Y" = c(4,5))),
               codes_used_2)

  # only one id allowed
  expect_error(cohortCodelist(cdm$cohort1, cohortId = c(1,2)))
  # error as none of the cohorts is present
  expect_error(cohortCodelist(cdm$cohort1, cohortId = 3))

})

test_that("test epected error cohort_codelist in wrong format", {

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
  cohort_start_date = as.Date(c(
    "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
  )),
  cohort_end_date = as.Date(c(
    "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
  ))
)
cdm <- cdmFromTables(
  tables = list("person" = person, "observation_period" = observation_period),
  cdmName = "my_example_cdm",
  cohortTables = list("cohort1" = cohort)
)
expect_error(cdm$cohort1 <- newCohortTable(table = cdm$cohort1,
                              cohortCodelistRef = dplyr::tibble(
                                not_a_cohort_definition_id = c(1,1,1,2,2),
                                a_codelist_name =c("disease X", "disease X", "disease X",
                                                 "disease Y", "disease Y"),
                                concept_id = c(1,2,3,4,5),
                                type = "index event"
                              )))

expect_error(newCohortTable(table = cdm$cohort1,
               cohortCodelistRef = dplyr::tibble(
                 cohort_definition_id = c(1,1,1,2,2),
                 codelist_name =c("disease X", "disease X", "disease X",
                                    "disease Y", "disease Y"),
                 concept_id = c(1,2,3,4,5),
                 type = "another name"
               )))

expect_no_error(newCohortTable(table = cdm$cohort1,
               cohortCodelistRef = NULL))

})
