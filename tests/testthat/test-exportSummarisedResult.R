test_that("test export", {
  res <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
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
  ) |>
    newSummarisedResult(
      settings = dplyr::tibble(
        "result_id" = 1L,
        "result_type" = "summarised_characteristics",
        "package_name" = "PatientProfiles",
        "package_version" = "0.4.0",
      )
    )

  path <- tempdir()
  expect_no_error(exportSummarisedResult(res, path = path))

  name <- paste0("results_cprd_", format(Sys.Date(), "%Y_%m_%d"), ".csv")
  expect_true(name %in% list.files(path))

  resSuppr <- res |> newSummarisedResult(
    settings = settings(res) |> dplyr::mutate("min_cell_count" = 5L)
  )
  expect_identical(
    read.csv(file = file.path(path, name)) |> newSummarisedResult(), resSuppr
  )

})


test_that("test temp file", {
  res <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "cprd",
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
    newSummarisedResult(
      settings = dplyr::tibble(
        "result_id" = 1L,
        "result_type" = "summarised_characteristics",
        "package_name" = "PatientProfiles",
        "package_version" = "0.4.0",
      )
    )

  tempFile <- tempfile(fileext = ".csv")

  expect_no_error(exportSummarisedResult(results = res,
                                         fileName = tempFile))

})


test_that("empty result", {
  tempFile <- tempfile(fileext = ".csv")

  expect_warning(exportSummarisedResult(results = list(),
                                         fileName = tempFile))
  expect_warning(exportSummarisedResult(results = NULL,
                                         fileName = tempFile))

})

