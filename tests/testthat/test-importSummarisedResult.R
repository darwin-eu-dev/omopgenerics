test_that("import summarised result", {
  res_1 <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "database 1",
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
    )|>
    suppress(5)

  res_2 <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "database 2",
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
    ) |>
    suppress(5)

  dir.create(cs_path <- file.path(tempdir(), omopgenerics::uniqueTableName()))
  expect_no_error(exportSummarisedResult(res_1, fileName = "result_1.csv",
                                         path = cs_path))
  expect_no_error(exportSummarisedResult(res_2, fileName = "result_2.csv",
                                         path = cs_path))
 expect_true("result_1.csv" %in% list.files(cs_path))
 expect_true("result_2.csv" %in% list.files(cs_path))

 results_original <- bind(res_1, res_2)
 results_imported <- importSummarisedResult(path = cs_path)
 expect_identical(results_original,results_imported)

 # csv in wrong format
 expect_warning(importSummarisedResult(path = cs_path))
 # empty folder
 dir.create(cs_path_2 <- file.path(tempdir(), omopgenerics::uniqueTableName()))
 expect_warning(importSummarisedResult(path = cs_path_2))

 # expected errors
 expect_error(importSummarisedResult(path = "not a path"))
 readr::write_csv(cars, file = paste0(cs_path, "/cars.csv"))


 })
