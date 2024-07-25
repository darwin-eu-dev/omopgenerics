
test_that("test validateNameArgument", {
  expect_error(validateNameArgument(name = 1))
  expect_error(validateNameArgument(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateNameArgument("my_name"))
  expect_error(validateNameArgument("myName"))
  expect_warning(expect_identical("my_name", validateNameArgument("myName", validation = "warning")))
  expect_warning(expect_warning(expect_identical(
    "my_name", validateNameArgument("myName", list("my_name" = 1), validation = "warning")
  )))
})

test_that("test validateCohortIdArgument", {
  cohort <- 1
  class(cohort) <- c("cohort_table", "cdm_table")
  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = c(2, 4), "cohort_name" = c("a", "b")
  )
  expect_error(validateCohortIdArgument("adsfd", cohort))
  expect_identical(validateCohortIdArgument(2, cohort), 2L)
  expect_identical(validateCohortIdArgument(c(2, 4), cohort), c(2L, 4L))
  expect_identical(validateCohortIdArgument(c(4L, 2L), cohort), c(4L, 2L))
  expect_error(validateCohortIdArgument(1, cohort))
})

test_that("test validateCdmArgument", {



  cdm_object <- 1
  class(cdm_object) <- c("cdm_reference")
  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = FALSE,
      checkStartBeforeEndObservation = FALSE
    )
  )
  expect_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = 1L, person_id = 1L,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2025-12-31"),
      period_type_concept_id = 0L
    ))

  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkClass = FALSE,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  expect_error(
    validateCdmArgument(
      cdm_object,
      checkClass = TRUE,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L,1L), person_id = c(1L,1L),
      observation_period_start_date = c(as.Date("2000-01-01"),as.Date("2000-01-01")),
      observation_period_end_date = c(as.Date("2025-12-31"),as.Date("2023-01-01")),
      period_type_concept_id = c(0L,0L)
    ))

  expect_error(
    validateCdmArgument(
      cdm_object,
      checkClass = FALSE,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkClass = FALSE,
      checkOverlapObservation = FALSE,
      checkStartBeforeEndObservation = FALSE
    )
  )


})
