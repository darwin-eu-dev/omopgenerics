test_that("test create omop_cohort", {
  x <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  expect_no_error(cohort <- newOmopCohort(x))
  expect_true("omop_cohort" %in% class(cohort))
  expect_true("GeneratedCohortSet" %in% class(cohort)) # to be removed
  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohort))
  ))
  expect_equal(cohortSet(cohort), attr(cohort, "cohort_set"))
  expect_no_error(cohortCount(cohort))
  expect_equal(cohortAttrition(cohort), attr(cohort, "cohort_attrition"))

  # check cohort set
  cohort_set1 <- dplyr::tibble(cohort_definition_id = 1)
  cohort_set2 <- dplyr::tibble(cohort_definition_id = 1, cohort_name = "hi")
  cohort_set3 <- dplyr::tibble(
    cohort_definition_id = 1, cohort_name = "hi", rand = "random"
  )
  cohort_set4 <- dplyr::tibble(
    cohort_definition_id = c(1, 2), cohort_name = c("hi", "ha")
  )
  cohort_set5 <- dplyr::tibble(cohort_definition_id = 2, cohort_name = c("ha"))
  expect_error(cohort1 <- newOmopCohort(x, cohort_set1))
  expect_no_error(cohort2 <- newOmopCohort(x, cohort_set2))
  expect_no_error(cohort3 <- newOmopCohort(x, cohort_set3))
  expect_error(cohort4 <- newOmopCohort(x, cohort_set4))
  expect_error(cohort5 <- newOmopCohort(x, cohort_set5))
  expect_equal(cohortSet(cohort2), cohort_set2)
  expect_equal(cohortSet(cohort3), cohort_set3)

  # check cohort attrition
  cohort_attrition1 <- dplyr::tibble(cohort_definition_id = 1)
  cohort_attrition2 <- dplyr::tibble(
    cohort_definition_id = 1, number_records = 2, number_subjects = 1,
    reason_id = 1, reason = "a", excluded_records = 0, excluded_subjects = 0
  )
  cohort_attrition3 <- dplyr::tibble(
    cohort_definition_id = 1, number_records = 2, number_subjects = 1,
    reason_id = 1, reason = "a", excluded_records = 0, excluded_subjects = 0,
    extra_field = "random"
  )
  cohort_attrition4 <- dplyr::tibble(
    cohort_definition_id = c(1, 2), number_records = 2, number_subjects = 1,
    reason_id = 1, reason = "a", excluded_records = 0, excluded_subjects = 0
  )
  cohort_attrition5 <- dplyr::tibble(
    cohort_definition_id = 2, number_records = 2, number_subjects = 1,
    reason_id = 1, reason = "a", excluded_records = 0, excluded_subjects = 0
  )
  expect_error(cohort1 <- newOmopCohort(x, cohortAttritionTable = cohort_attrition1))
  expect_no_error(cohort2 <- newOmopCohort(x, cohortAttritionTable = cohort_attrition2))
  expect_no_error(cohort3 <- newOmopCohort(x, cohortAttritionTable = cohort_attrition3))
  expect_error(cohort4 <- newOmopCohort(x, cohortAttritionTable = cohort_attrition4))
  expect_error(cohort5 <- newOmopCohort(x, cohortAttritionTable = cohort_attrition5))
  expect_equal(cohortAttrition(cohort2), cohort_attrition2)
  expect_equal(cohortAttrition(cohort3), cohort_attrition3)

  expect_equal(
    cohortCount(cohort2),
    dplyr::tibble(
      cohort_definition_id = 1, number_records = 2, number_subjects = 1
    )
  )
  expect_equal(
    cohortCount(cohort3),
    dplyr::tibble(
      cohort_definition_id = 1, number_records = 2, number_subjects = 1
    )
  )

  # collect
  expect_equal(cohort, dplyr::collect(cohort))

  # classes
  class(x) <- c("tbl_sql", class(x))
  expect_snapshot_error(cohort <- newOmopCohort(x, cohort_set2))
  class(x) <- class(x)[class(x) != "tbl_sql"]
  expect_no_error(cohort <- newOmopCohort(x, cohort_set2))
  class(cohort_set2) <- c("tbl_sql", class(cohort_set2))
  expect_error(cohort <- newOmopCohort(x, cohort_set2))

  # omop_cohort class
  expect_no_error(validateCdmCohort(cohort2))
  class(cohort2) <- class(cohort2)[class(cohort2) != "omop_cohort"]
  expect_error(validateCdmCohort(cohort2))

  # remove cols
  expect_no_error(validateCdmCohort(cohort3))
  attr(cohort3, "cohort_set") <- attr(cohort3, "cohort_set") %>%
    dplyr::select(-"cohort_name")
  expect_error(validateCdmCohort(cohort3))

  # remove attribute
  expect_no_error(cohort2 <- newOmopCohort(x, cohortAttritionTable = cohort_attrition2))
  expect_no_error(validateCdmCohort(cohort2))
  attr(cohort2, "cohort_set") <- NULL
  expect_error(validateCdmCohort(cohort2))

  # cohort count
  expect_warning(cohort <- newOmopCohort(x, cohortCountTable = NULL))
})
