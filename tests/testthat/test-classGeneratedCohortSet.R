test_that("test create cohort", {
  src <- localSource("test")
  cdmTables <- list(
    "person" = dplyr::tibble(
      person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
      race_concept_id = 0, ethnicity_concept_id = 0
    ),
    "observation_period" = dplyr::tibble(
      observation_period_id = 1, person_id = 1,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2025-12-31"),
      period_type_concept_id = 0
    )
  )
  cdm <- cdmReference(cdmTables = cdmTables, cdmName = "mock", cdmSource = src)
  cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  ))
  set <- defaultCohortSet(cdm$cohort1)
  attrition <- defaultCohortAttrition(cdm$cohort1, set)

  expect_error(cohort <- generatedCohortSet(cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = cdm$cohort1, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_true("generated_cohort_set" %in% class(cohort))
  expect_true("GeneratedCohortSet" %in% class(cohort)) # to be removed
  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohort))
  ))
  expect_equal(
    set(cohort) |> unclass(), attr(cohort, "cohort_set") |> unclass()
  )
  expect_no_error(cohortCount(cohort))
  expect_equal(
    attrition(cohort) |> unclass(), attr(cohort, "cohort_attrition") |> unclass()
  )

  set$cohort_name <- "Cohort 1"
  expect_error(generatedCohortSet(cohortRef = x, cohortSetRef = set, cohortAttritionRef = attrition))

  cdm <- insertTable(cdm, name = "cohort2", table = dplyr::tibble(
    cohort_definition_id = c(1, 2), subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  ))
  set <- defaultCohortSet(cdm$cohort2)
  attrition <- defaultCohortAttrition(cdm$cohort2, set)
  expect_no_error(generatedCohortSet(cohortRef = cdm$cohort2, cohortSetRef = set, cohortAttritionRef = attrition))
  set$cohort_name <- "cohort_1"
  expect_error(generatedCohortSet(cohortRef = cdm$cohort2, cohortSetRef = set, cohortAttritionRef = attrition))

  cdm <- insertTable(cdm, name = "cohort3", table = dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  ))
  set <- defaultCohortSet(cdm$cohort3)
  attrition <- defaultCohortAttrition(cdm$cohort3, set)

  expect_no_error(cohort <- generatedCohortSet(cohortRef = cdm$cohort3, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = cdm$cohort3, cohortSetRef = set))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = cdm$cohort3, cohortAttritionRef = attrition))
  expect_error(cohort <- generatedCohortSet(cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- generatedCohortSet(cohortRef = cdm$cohort3, cohortSetRef = set, cohortAttritionRef = attrition))
  expect_no_error(cohort <- generatedCohortSet(cdm$cohort3))

  expect_true("generated_cohort_set" %in% class(cohort))
  expect_true("GeneratedCohortSet" %in% class(cohort)) # to be removed
  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohort))
  ))
  expect_equal(
    set(cohort) |> unclass(), attr(cohort, "cohort_set") |> unclass()
  )
  expect_no_error(cohortCount(cohort))
  expect_equal(
    attrition(cohort) |> unclass(), attr(cohort, "cohort_attrition") |> unclass()
  )

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
  expect_error(cohort1 <- generatedCohortSet(cdm$cohort3, cohort_set1))
  expect_no_error(cohort2 <- generatedCohortSet(cdm$cohort3, cohort_set2))
  expect_no_error(cohort3 <- generatedCohortSet(cdm$cohort3, cohort_set3))
  expect_error(cohort4 <- generatedCohortSet(cdm$cohort3, cohort_set4))
  expect_error(cohort5 <- generatedCohortSet(cdm$cohort3, cohort_set5))
  # expect_equal(set(cohort2) |> unclass(), cohort_set2 |> unclass())
  # expect_equal(set(cohort3), cohort_set3)

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
  expect_error(cohort1 <- generatedCohortSet(cdm$cohort3, cohortAttritionRef = cohort_attrition1))
  expect_no_error(cohort2 <- generatedCohortSet(cdm$cohort3, cohortAttritionRef = cohort_attrition2))
  expect_no_error(cohort3 <- generatedCohortSet(cdm$cohort3, cohortAttritionRef = cohort_attrition3))
  expect_error(cohort4 <- generatedCohortSet(cdm$cohort3, cohortAttritionRef = cohort_attrition4))
  expect_error(cohort5 <- generatedCohortSet(cdm$cohort3, cohortAttritionRef = cohort_attrition5))
  # TODO
  # expect_equal(attrition(cohort2), cohort_attrition2)
  # expect_equal(attrition(cohort3), cohort_attrition3)

  expect_true(is.integer(attrition(cohort2)$cohort_definition_id))
  expect_true(is.integer(attrition(cohort2)$number_records))
  expect_true(is.integer(attrition(cohort2)$number_subjects))
  expect_true(is.integer(attrition(cohort2)$reason_id))
  expect_true(is.character(attrition(cohort2)$reason))
  expect_true(is.integer(attrition(cohort2)$excluded_records))
  expect_true(is.integer(attrition(cohort2)$excluded_subjects))

  expect_equal(
    cohortCount(cohort2),
    dplyr::tibble(
      cohort_definition_id = 1, number_records = 2, number_subjects = 1
    )
  )
  expect_true(is.integer(cohortCount(cohort2)$cohort_definition_id))
  expect_true(is.integer(cohortCount(cohort2)$number_records))
  expect_true(is.integer(cohortCount(cohort2)$number_subjects))


  expect_equal(
    cohortCount(cohort3),
    dplyr::tibble(
      cohort_definition_id = 1, number_records = 2, number_subjects = 1
    )
  )

  # collect
  expect_equal(cohort, dplyr::collect(cohort))

  # classes
  expect_no_error(cohort <- generatedCohortSet(cdm$cohort3, cohort_set2))
  class(cohort_set2) <- c("tbl_sql", class(cohort_set2))
  expect_error(cohort <- generatedCohortSet(x, cohort_set2))

  # remove cols
  expect_no_error(validateGeneratedCohortSet(cohort3))
  attr(cohort3, "cohort_set") <- attr(cohort3, "cohort_set") |>
    dplyr::select(-"cohort_name")
  expect_error(validateGeneratedCohortSet(cohort3))

  # remove attribute
  expect_no_error(cohort2 <- generatedCohortSet(cdm$cohort3, cohortAttritionRef = cohort_attrition2))
  expect_no_error(validateGeneratedCohortSet(cohort2))
  attr(cohort2, "cohort_set") <- NULL
  expect_error(validateGeneratedCohortSet(cohort2))

})

