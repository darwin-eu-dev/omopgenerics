test_that("test create cohort", {
  x <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  set <- defaultCohortSet(x)
  attrition <- defaultCohortAttrition(x)
  cohortname <- "cohort_interest"

  expect_error(cohort <- generatedCohortSet(cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))
  expect_true("generated_cohort_set" %in% class(cohort))
  expect_true("GeneratedCohortSet" %in% class(cohort)) # to be removed
  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohort))
  ))
  expect_equal(cohortSet(cohort), attr(cohort, "cohort_set"))
  expect_no_error(cohortCount(cohort))
  expect_equal(cohortAttrition(cohort), attr(cohort, "cohort_attrition"))

  set$cohort_name <- "Cohort 1"
  expect_error(generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))

  x <- dplyr::tibble(
    cohort_definition_id = c(1, 2), subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  set <- defaultCohortSet(x)
  attrition <- defaultCohortAttrition(x)
  expect_no_error(generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))
  set$cohort_name <- "cohort_1"
  expect_error(generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))

  x <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  set <- defaultCohortSet(x)
  attrition <- defaultCohortAttrition(x)
  cohortname <- "cohort_interest"

  expect_no_error(cohort <- generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortName = cohortname))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = x, cohortAttritionRef = attrition, cohortName = cohortname))
  expect_error(cohort <- generatedCohortSet(cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition, cohortName = cohortname))
  expect_no_error(cohort <- generatedCohortSet(x))

  expect_true("generated_cohort_set" %in% class(cohort))
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
  expect_error(cohort1 <- generatedCohortSet(x, cohort_set1))
  expect_no_error(cohort2 <- generatedCohortSet(x, cohort_set2))
  expect_no_error(cohort3 <- generatedCohortSet(x, cohort_set3))
  expect_error(cohort4 <- generatedCohortSet(x, cohort_set4))
  expect_error(cohort5 <- generatedCohortSet(x, cohort_set5))
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
  expect_error(cohort1 <- generatedCohortSet(x, cohortAttritionRef = cohort_attrition1))
  expect_no_error(cohort2 <- generatedCohortSet(x, cohortAttritionRef = cohort_attrition2))
  expect_no_error(cohort3 <- generatedCohortSet(x, cohortAttritionRef = cohort_attrition3))
  expect_error(cohort4 <- generatedCohortSet(x, cohortAttritionRef = cohort_attrition4))
  expect_error(cohort5 <- generatedCohortSet(x, cohortAttritionRef = cohort_attrition5))
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
  expect_no_error(cohort <- generatedCohortSet(x, cohort_set2))
  class(cohort_set2) <- c("tbl_sql", class(cohort_set2))
  expect_error(cohort <- generatedCohortSet(x, cohort_set2))

  # remove cols
  expect_no_error(validateGeneratedCohortSet(cohort3))
  attr(cohort3, "cohort_set") <- attr(cohort3, "cohort_set") |>
    dplyr::select(-"cohort_name")
  expect_error(validateGeneratedCohortSet(cohort3))

  # remove attribute
  expect_no_error(cohort2 <- generatedCohortSet(x, cohortAttritionRef = cohort_attrition2))
  expect_no_error(validateGeneratedCohortSet(cohort2))
  attr(cohort2, "cohort_set") <- NULL
  expect_error(validateGeneratedCohortSet(cohort2))

})
