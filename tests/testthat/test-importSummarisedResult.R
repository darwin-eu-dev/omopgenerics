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

  res_3 <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "database 3",
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


 # empty folder
 dir.create(cs_path_2 <- file.path(tempdir(), omopgenerics::uniqueTableName()))
 expect_warning(result <- importSummarisedResult(path = cs_path_2))
 expect_identical(result, emptySummarisedResult())

 # check recursive
 dir.create(cs_path_rex <- file.path(cs_path, omopgenerics::uniqueTableName()))
 expect_no_error(exportSummarisedResult(res_3, fileName = "result_3.csv",
                                        path = cs_path_rex))
results_imported_no_rec <- importSummarisedResult(path = cs_path, recursive = FALSE)
expect_identical(results_original,results_imported_no_rec)
results_imported_rec <- importSummarisedResult(path = cs_path, recursive = TRUE)
expect_identical(bind(res_1, res_2, res_3) |> arrange(cdm_name),
                 results_imported_rec |> arrange(cdm_name))

 # import from two folders
expect_no_error(results_imported <- importSummarisedResult(path = c(cs_path,
                                cs_path_rex),
                       recursive = FALSE))
expect_identical(bind(res_1, res_2, res_3) |> arrange(cdm_name),
                 results_imported |> arrange(cdm_name))

 # import one specific file
 expect_no_error(res_1_imported <- importSummarisedResult(path = here::here(cs_path, "/result_1.csv")))
 expect_identical(res_1, res_1_imported)

 # import two specific files
 expect_no_error(res_1_2_imported <- importSummarisedResult(path =
                                                            c(here::here(cs_path, "/result_1.csv"),
                                                              here::here(cs_path, "/result_2.csv"))))
 expect_identical(res_1_2_imported |> arrange(cdm_name),
                  results_original |> arrange(cdm_name))

 # mix of folders and files
 importSummarisedResult(path = c(cs_path,
                                 here::here(cs_path, "/result_1.csv")),
                                 recursive = FALSE)


 # expected errors
 expect_error(importSummarisedResult(path = "not a path"))
 # csv in wrong format
 readr::write_csv(cars, file = paste0(cs_path_2, "/cars.csv"))
 expect_warning(expect_warning(importSummarisedResult(path = cs_path_2)))


 })
