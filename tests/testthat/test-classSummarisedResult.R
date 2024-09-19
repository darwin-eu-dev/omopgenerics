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
    "group_name" = "sex and cohort_Name",
    "group_level" = "male and cohort1",
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

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = rep("number records", 2),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = c("5", "6"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = c(1, 2),
    "cdm_name" = "eunomia",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = rep("number records", 2),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1,
    "cdm_name" = "eunomia",
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = c("number SUBJECTS", "number_subjects"),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1,
    "cdm_name" = c("eunomia", "cprd"),
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = c("number SUBJECTS", "number_subjects"),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = as.integer(c(1, 2)),
    "cdm_name" = c("cprd", "eunomia"),
    "group_name" = "sex",
    "group_level" = "male",
    "strata_name" = "sex",
    "strata_level" = "male",
    "variable_name" = c("number subjects", "number records"),
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = c("sex", "sex", "sex", "age_group", "age_group", "calendar"),
    "group_level" = c("male", "female", "none", ">=40", "<40", "2020"),
    "strata_name" = "overall",
    "strata_level" = "overall",
    "variable_name" = "xxx",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())
  expect_message(x |> dplyr::union_all(x) |> newSummarisedResult())
  expect_error(
    x |>
      dplyr::union_all(x |> dplyr::mutate(estimate_value = "0")) |>
      newSummarisedResult()
  )

  x <- dplyr::tibble(
    "result_id" = 1L,
    "cdm_name" = "eunomia",
    "group_name" = c("sex", "sex"),
    "group_level" = c("male", "male"),
    "strata_name" = "overall",
    "strata_level" = "overall",
    "variable_name" = "number_subjects",
    "variable_level" = NA_character_,
    "estimate_name" = c("count", "percentage"),
    "estimate_type" = "numeric",
    "estimate_value" = "5",
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  expect_no_error(x |> newSummarisedResult())

})

test_that("validateNameLevel", {
  sr <- dplyr::tibble(
    result_id = 1L,
    cdm_name = "mock",
    group_name = "cohort_name &&& age",
    group_level = "acetaminophen &&& between 18 and 50",
    strata_name = "overall",
    strata_level = "overall",
    variable_name = "number records",
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    estimate_value = "5",
    additional_name = "overall",
    additional_level = "overall"
  )
  expect_no_error(sr |> newSummarisedResult())
  expect_no_error(
    sr |>
      validateNameLevel(nameColumn = "group_name", levelColumn = "group_level")
  )
  expect_error(
    sr |>
      validateNameLevel(
        nameColumn = "group_name", levelColumn = "group_level", sep = " and ")
  )
  expect_warning(expect_warning(
    sr |>
      validateNameLevel(
        nameColumn = "group_name", levelColumn = "group_level", sep = " and ", warn = TRUE)
  ))
  expect_warning(
    sr |>
      validateNameLevel(
        nameColumn = "group_name", levelColumn = "group_level", sep = " &&& | and ", warn = TRUE)
  )
})

test_that("validate duplicates", {
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
  expect_no_error(x |> newSummarisedResult())
  sr <- dplyr::bind_rows(x, x |> dplyr::mutate(estimate_value = "6"))
  expect_error(sr |> newSummarisedResult())
})
