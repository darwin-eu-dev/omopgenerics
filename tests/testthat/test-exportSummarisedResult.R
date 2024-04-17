test_that("test export", {
  res <- dplyr::tibble(
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
  ) |>
    newSummarisedResult()

  path <- tempdir()
  expect_no_error(exportSummarisedResult(res, path = path))

  name <- paste0("results_cprd_", format(Sys.Date(), "%Y_%m_%d"), ".csv")
  expect_true(name %in% list.files(path))

  expect_identical(
    read.csv(file = file.path(path, name)) |> newSummarisedResult(), res
  )

})
