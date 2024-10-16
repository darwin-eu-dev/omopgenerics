
test_that("test validateNameArgument", {
  expect_error(validateNameArgument(name = 1))
  expect_error(validateNameArgument(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateNameArgument("my_name"))
  expect_error(validateNameArgument("myName"))
  expect_warning(expect_identical("my_name", validateNameArgument("myName", validation = "warning")))
  expect_warning(expect_warning(expect_identical(
    "my_name", validateNameArgument("myName", list("my_name" = 1), validation = "warning")
  )))
  expect_no_error(validateNameArgument(name = NULL, null = TRUE))
  expect_error(validateNameArgument(name = NULL, null = FALSE))
})

test_that("test validateCohortIdArgument", {
  # toy cohort
  cohort <- dplyr::tibble(
    cohort_definition_id = 1:4L, subject_id = 1L, cohort_start_date = Sys.Date(),
    cohort_end_date = Sys.Date()
  )
  class(cohort) <- c("cohort_table", "cdm_table")
  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = c(1L, 2L, 3L, 4L),
    "cohort_name" = c("cohort_a", "acetaminophen", "paracetamol", "cohort_ol")
  )

  # numeric behavior
  expect_identical(validateCohortIdArgument(2, cohort), 2L)
  expect_identical(validateCohortIdArgument(c(2, 4), cohort), c(2L, 4L))
  expect_identical(validateCohortIdArgument(c(4L, 2L), cohort), c(4L, 2L))
  expect_error(validateCohortIdArgument(5, cohort))
  expect_warning(expect_identical(
    validateCohortIdArgument(c(2, 8), cohort, validation = "warning"),
    2L
  ))
  expect_warning(expect_warning(expect_identical(
    validateCohortIdArgument(5, cohort, validation = "warning"),
    integer()
  )))

  # character behavior
  expect_identical(validateCohortIdArgument("acetaminophen", cohort), 2L)
  expect_identical(
    validateCohortIdArgument(c("acetaminophen", "paracetamol"), cohort),
    c(2L, 3L))
  expect_identical(
    validateCohortIdArgument(c("paracetamol", "acetaminophen"), cohort),
    c(3L, 2L))
  expect_error(validateCohortIdArgument(c("not_present"), cohort))
  expect_warning(expect_identical(
    validateCohortIdArgument(
      c("paracetamol", "not_present"), cohort, validation = "warning"
    ),
    3L
  ))
  expect_warning(expect_warning(expect_identical(
    validateCohortIdArgument(c("not_present"), cohort, validation = "warning"),
    integer()
  )))

  # tidyselect behavior
  expect_identical(
    validateCohortIdArgument(dplyr::starts_with("cohort_"), cohort),
    c(1L, 4L)
  )
  expect_identical(
    validateCohortIdArgument(dplyr::ends_with("ol"), cohort),
    c(3L, 4L)
  )
  expect_identical(
    validateCohortIdArgument(dplyr::everything(), cohort),
    c(1L, 2L, 3L, 4L)
  )
  expect_error(
    validateCohortIdArgument(dplyr::any_of(c("sdfghjk", "dfg")), cohort)
  )
  expect_warning(expect_identical(
    validateCohortIdArgument(
      dplyr::any_of(c("sdfghjk", "dfg")), cohort, validation = "warning"),
    integer()
  ))

  # NULL
  expect_identical(validateCohortIdArgument(NULL, cohort), c(1L, 2L, 3L, 4L))

  # error if anything else is provided
  expect_error(validateCohortIdArgument(list(), cohort))
  expect_error(validateCohortIdArgument(list(), cohort, validation = "warning"))

  # check in external function
  filterCohort <- function(x, id) {
    id <- validateCohortIdArgument({{id}}, x)
    x |>
      dplyr::filter(.data$cohort_definition_id %in% .env$id)
  }
  expect_no_error(filterCohort(cohort, dplyr::starts_with("cohort")))

})

test_that("test validateWindowArgument", {
  window <- c(0, 1)
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, 1), c(2, 3))
  expect_no_error(validateWindowArgument(window))
  window <- list(c("a", 1))
  expect_error(validateWindowArgument(window))
  window <- list("window" = c(0, 1), "window2" = c(-1, 1))
  expect_no_error(validateWindowArgument(window))
  window <- list(c(0, -1))
  expect_error(validateWindowArgument(window))
  window <- list(c(-Inf, -Inf))
  expect_error(validateWindowArgument(window))
  window <- list(c(Inf, Inf))
  expect_error(validateWindowArgument(window))

  #window name check
  window <- list(c(-1, 1))

  window <- window |> validateWindowArgument(snakeCase = FALSE)

  expect_true(names(window) == "-1 to 1")

  window <- list(c(-1, 1))

  window <- window |> validateWindowArgument(snakeCase = TRUE)

  expect_true(names(window) == "m1_to_1")


  window <- list("window" = c(-1, 1))
  window <- window |> validateWindowArgument(snakeCase = TRUE)
  expect_true(names(window) == "window")

})

test_that("test validateAgeGroup", {
  #test list
  expect_error(validateAgeGroupArgument(c("1", "18")))
  ageGroup = list(c(0, 18))
  expect_no_error(validateAgeGroupArgument(ageGroup))

  #test name
  ageGroup = validateAgeGroupArgument(ageGroup)
  expect_true(names(ageGroup) == "age_group")

  # name multiple group
  ageGroup = list(list(c(0, 19)), list(c(0, 18)))
  ageGroup = validateAgeGroupArgument(ageGroup)
  expect_true(all(names(ageGroup) == c("age_group_1", "age_group_2")))

  #test overlap
  ageGroup = list(c(0, 18), c(16, 20))
  expect_error(validateAgeGroupArgument(ageGroup, overlap = FALSE))
  expect_no_error(validateAgeGroupArgument(ageGroup, overlap = TRUE))

  #test order
  ageGroup = list(c(19, 18), c(21, 20))
  expect_error(validateAgeGroupArgument(ageGroup, overlap = FALSE))

  # test multiple age group
  ageGroup = list(list(c(0, 19)), list(c(0, 18)))
  expect_error(validateAgeGroupArgument(
    ageGroup, overlap = FALSE, multipleAgeGroup = FALSE))
  expect_no_error(validateAgeGroupArgument(
    ageGroup, overlap = FALSE, multipleAgeGroup = TRUE))

  # null age group
  expect_no_error(validateAgeGroupArgument(
    ageGroup = NULL, overlap = FALSE, multipleAgeGroup = FALSE))
  expect_error(validateAgeGroupArgument(
    ageGroup = NULL, overlap = FALSE, null = FALSE, multipleAgeGroup = FALSE))

  # correct naming
  x <- list(0, c(1, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)) |>
    validateAgeGroupArgument()
  expect_identical(
    x,
    list("age_group" = list(
      "0 to 0" = c(0, 0), "1 to 19" = c(1, 19), "20 to 39" = c(20, 39),
      "40 to 59" = c(40, 59), "60 to 79" = c(60, 79), "80 or above" = c(80, Inf)
    ))
  )
  expect_identical(
    validateAgeGroupArgument(c(0, Inf), ageGroupName = "my_age_group"),
    list(my_age_group = list(overall = c(0L, Inf)))
  )
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
      observation_period_end_date = as.Date("2023-12-31"),
      period_type_concept_id = 0L
    ))

  class(cdm_object) <- c("cdm_reference")

  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L,1L), person_id = c(1L,1L),
      observation_period_start_date = c(as.Date("2000-01-01"),as.Date("2000-01-01")),
      observation_period_end_date = c(as.Date("2023-12-31"),as.Date("2023-01-01")),
      period_type_concept_id = c(0L,0L)
    ))
  class(cdm_object) <- c("cdm_reference")

  expect_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE
    )
  )

  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkOverlapObservation = FALSE,
      checkStartBeforeEndObservation = FALSE
    )
  )

  # implausible starting observation date
  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L,1L), person_id = c(1L,1L),
      observation_period_start_date = c(as.Date("1700-01-01"),as.Date("2000-01-01")),
      observation_period_end_date = c(as.Date("2000-01-01"),as.Date("2023-01-01")),
      period_type_concept_id = c(0L,0L)
    ))
  class(cdm_object) <- c("cdm_reference")
  expect_warning(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = TRUE
    )
  )
  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = FALSE
    )
  )

  # implausible ending observation date - currently a warning instead of e
  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L,1L), person_id = c(1L,1L),
      observation_period_start_date = c(as.Date("2000-01-01"),as.Date("2000-01-02")),
      observation_period_end_date = c(as.Date("2000-01-01"),as.Date("2100-01-01")),
      period_type_concept_id = c(0L,0L)
    ))
  class(cdm_object) <- c("cdm_reference")
  expect_warning(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = TRUE
    )
  )
  expect_no_error(
    validateCdmArgument(
      cdm_object,
      checkPlausibleObservationDates = FALSE
    )
  )


  # no errors or warnings if cdm is empty
  expect_no_error(
    validateCdmArgument(
      emptyCdmReference("test"),
      checkOverlapObservation = TRUE,
      checkStartBeforeEndObservation = TRUE,
      checkPlausibleObservationDates = TRUE
    )
    )

  # warnings if clinical table contain person id not in person  table

  cdm_object <- list(
    "observation_period" = dplyr::tibble(
      observation_period_id = c(1L,1L), person_id = c(1L,1L),
      observation_period_start_date = c(as.Date("2000-01-01"),as.Date("2000-01-02")),
      observation_period_end_date = c(as.Date("2000-01-01"),as.Date("2100-01-01")),
      period_type_concept_id = c(0L,0L)
    ),
    "person" = dplyr::tibble(
      person_id = c(1L,1L),
      gender_concept_id = c(8507L,8507L),
      year_of_birth = c(1991L,1992L)
    ),
    "condition_occurrence" = dplyr::tibble(
      condition_occurrence_id = c(1L,2L),
      person_id = c(8507L,8507L),
      condition_start_date =  c(as.Date("2000-01-01"),as.Date("2000-01-02")),
      condition_end_date = c(as.Date("2000-01-01"),as.Date("2100-01-01"))
    ))

  class(cdm_object) <- c("cdm_reference")

  expect_no_warning(
    validateCdmArgument(
      cdm_object,
      checkPerson = FALSE
    )
  )

  expect_warning(
    validateCdmArgument(
      cdm_object,
      checkPerson = TRUE
    )
  )

})

test_that("test validateResults", {

  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
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
  )



  expect_no_error(x |>
                    newSummarisedResult() |>
                    validateResultArgument())

})

test_that("test isResultSuppressed",{
  obj <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "mock",
    "group_name" = "overall",
    "group_level" = "overall",
    "strata_name" = c(rep("overall", 6), rep("sex", 3)),
    "strata_level" = c(rep("overall", 6), "male", "female", "female"),
    "variable_name" = c("number records", "age_group", "age_group", "age_group", "age_group", "my_variable", "number records", "age_group", "age_group"),
    "variable_level" = c(NA, "<50", "<50", ">=50", ">=50", NA, NA, "<50", "<50"),
    "estimate_name" = c("count", "count", "percentage", "count", "percentage", "random", "count", "count", "percentage"),
    "estimate_type" = c("integer", "integer", "percentage", "integer", "percentage", "numeric", "integer", "integer", "percentage"),
    "estimate_value" = c("10", "5", "50", "3", "30", "1", "3", "12", "6"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  ) |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = as.integer(1),
      "result_type" = "summarised_characteristics",
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics"))
    ))

  # Test for no min_cell_count column
  expect_warning(isResultSuppressed(result = obj, minCellCount = 3))

  result <- suppress(obj, minCellCount = 2)

  # Test for correctly specified min_cell_count
  expect_no_warning(isResultSuppressed(result = result, minCellCount = 2))

  # Test for greater actual min_cell_count
  expect_warning(isResultSuppressed(result = result, minCellCount = 1))

  # Test for smaller actual min_cell_count
  expect_warning(isResultSuppressed(result = result, minCellCount = 3))

})
