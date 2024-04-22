test_that("test SummarisedResult object", {
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
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
  expect_no_error(newSummarisedResult(x = x))

  expect_identical(
    estimateTypeChoices() |> sort(),
    c(
      "numeric", "integer", "date", "character", "proportion", "percentage",
      "logical"
    ) |>
      sort()
  )

  x |>
    newSummarisedResult() |>
    inherits("summarised_result") |>
    expect_true()

  # check none character
  x <- dplyr::tibble(
    "result_id" = NA_character_,
    "cdm_name" = 1,
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
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
  expect_message(newSummarisedResult(x = x))

  #check wrong columns
  x <- dplyr::tibble(
    "package" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable" = "age_group",
    "variable_level" = "10 to 50",
    "estimate_type" = "count",
    "estimate" = "5"
  )
  expect_error(newSummarisedResult(x = x))

  # check NA
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = NA_character_,
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_error(newSummarisedResult(x = x))

  # check wrong paired
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex &&& cohort_name",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male &&& cohort1",
    "variable_name" = "Age group",
    "variable_level" = "10 to 50",
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_warning(expect_warning(newSummarisedResult(x = x)))

  # check wrong case
  x <- dplyr::tibble(
    "cdm_name" = "cprd",
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex &&& cohort_Name",
    "group_level" = "male &&& cohort1",
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
  expect_warning(newSummarisedResult(x = x))

  x <- dplyr::tibble(
    "result_id" = as.integer(c(1, 2)),
    "cdm_name" = c("cprd", "eunomia"),
    "result_type" = "summarised_characteristics",
    "package_name" = "PatientProfiles",
    "package_version" = "0.4.0",
    "group_name" = "sex",
    "group_level" = "male",
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
  expect_no_error(res <- newSummarisedResult(x = x, settings = dplyr::tibble(
    "result_id" = c(1, 2), "custom" = c("A", "B")
  )))

  expect_identical(
    sort(colnames(settings(res))),
    c("custom", "package_name", "package_version", "result_id",
      "result_type")
  )

  expect_identical(
    x |> newSummarisedResult(),
    x |> newSummarisedResult() |> newSummarisedResult()
  )

})
