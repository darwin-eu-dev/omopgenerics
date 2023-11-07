test_that("test SummarisedResult object", {

  x <- dplyr::tibble(
    "cdm_name" = "cprd",
    "result_type" = "Summarised characteristics",
    "package" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "variable_type" = "date",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_no_error(summarisedResult(x = x))

  class <- x |> summarisedResult() |> class()
  expect_true(c("summarised_result") %in% class)

  # check none character
  x <- dplyr::tibble(
    "cdm_name" = 1,
    "result_type" = "Summarised characteristics",
    "package" = "patientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "variable_type" = "date",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_error(summarisedResult(x = x))

  #check none sentence case
  x <- dplyr::tibble(
    "cdm_name" = "cprd",
    "result_type" = "Summarised Characteristics",
    "package" = "patientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "variable_type" = "date",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_error(summarisedResult(x = x))

  #check wrong columns
  x <- dplyr::tibble(
    "package" = "patientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "variable_type" = "date",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_error(summarisedResult(x = x))

  # check NA
  x <- dplyr::tibble(
    "cdm_name" = as.character(NA),
    "result_type" = "Summarised characteristics",
    "package" = "patientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "variable_type" = "date",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_error(summarisedResult(x = x))

  # check wrong case
  x <- dplyr::tibble(
    "cdm_name" = "mock",
    "result_type" = "Summarised characteristics",
    "package" = "patientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex and cohort_name",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male and cohort1",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "variable_type" = "date",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_warning(expect_warning(summarisedResult(x = x)))
})
