test_that("test cdm_reference", {
  cohorts <- list("cohort1" = generatedCohortSet(dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-12-31")
  )))
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

  expect_no_error(cdm <- cdmReference(
    cdmTables = cdmTables, cohortTables = cohorts, cdmName = "mock"
  ))

  expect_snapshot(cdm)

  expect_identical(cdmName(cdm), "mock")
  expect_true("cdm_reference" %in% names(attributes(cdm$person)))
  expect_true("cdm_reference" %in% names(attributes(cdm[["person"]])))

  expect_error(cdmReference(
    cdmTables = cdmTables, cohortTables = cohorts, cdmName = 1
  ))

  expect_no_error(cdmReference(
    cdmTables = cdmTables, cohortTables = list(), cdmName = "mock"
  ))

  expect_error(cdmReference(
    cdmTables = cdmTables["person"], cohortTables = list(), cdmName = "mock"
  ))

  expect_error(cdmReference(
    cdmTables = list(), cohortTables = cohorts, cdmName = "mock"
  ))

  cdmUpper <- cdmTables
  names(cdmUpper) <- toupper(names(cdmUpper))
  expect_error(cdmReference(
    cdmTables = cdmUpper, cohortTables = list(), cdmName = "mock"
  ))

  cdmTables$person <- cdmTables$person |>
    dplyr::rename("PERSON_ID" = "person_id")
  expect_error(cdmReference(
    cdmTables = cdmTables, cohortTables = list(), cdmName = "mock"
  ))

  cdmTables$person <- cdmTables$person |>
    dplyr::select(-"PERSON_ID")
  expect_error(cdmReference(
    cdmTables = cdmTables, cohortTables = list(), cdmName = "mock"
  ))

})

test_that("test assign and extract from cdm object", {
  x <- list(a = 1, b = 2)
  class(x) <- "cdm_reference"
  expect_true("cdm_reference" %in% names(attributes(x[["a"]])))
  expect_true("cdm_reference" %in% names(attributes(x$a)))

  # if I do it for an object that it is not a cdm_reference it wont
  xu <- unclass(x)
  expect_false("cdm_reference" %in% names(attributes(xu[["a"]])))
  expect_false("cdm_reference" %in% names(attributes(xu$a)))

  # I define an element with a cdm reference on it
  xx <- 3
  attr(xx, "cdm_reference") <- 4
  expect_true("cdm_reference" %in% names(attributes(xx)))

  # if I assign this element to a list with no class the attribute will persist
  xu$c <- xx
  expect_true("cdm_reference" %in% names(attributes(xu[["c"]])))
  expect_true("cdm_reference" %in% names(attributes(xu$c)))

  # if I assign to a cdm_reference it wont but it will appear back when I access
  # to one of the elements
  x$c <- xx
  expect_true("cdm_reference" %in% names(attributes(x[["c"]])))
  expect_true("cdm_reference" %in% names(attributes(x$c)))

  # but if after assigning I remove the class the attribute wont be there
  # because when I assigned it was eliminated
  xuu <- unclass(x)
  expect_false("cdm_reference" %in% names(attributes(xuu[["c"]])))
  expect_false("cdm_reference" %in% names(attributes(xuu$c)))
})
