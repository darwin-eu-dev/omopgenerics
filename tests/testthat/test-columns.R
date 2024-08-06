
test_that("omop column functions work", {
  # basic functionality
  expect_no_error(omopColsReq <- omopColumns(table = "person", onlyRequired = TRUE))
  expect_no_error(omopColsAll <- omopColumns(table = "person", onlyRequired = FALSE))

  # correct values
  expect_identical(omopColsReq, intersect(omopColsReq, omopColsAll))
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "person", type == "cdm_table", is_required == TRUE,
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    omopColsReq
  )
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "person", type == "cdm_table",
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    omopColsAll
  )
})

test_that("achilles column functions work", {
  # basic functionality
  expect_no_error(achillesColsReq <- achillesColumns(table = "achilles_analysis", onlyRequired = TRUE))
  expect_no_error(achillesColsAll <- achillesColumns(table = "achilles_analysis", onlyRequired = FALSE))

  # correct values
  expect_identical(achillesColsReq, intersect(achillesColsReq, achillesColsAll))
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "achilles_analysis", type == "achilles", is_required == TRUE,
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    achillesColsReq
  )
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "achilles_analysis", type == "achilles",
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    achillesColsAll
  )
})

test_that("cohort column functions work", {
  # basic functionality
  expect_no_error(cohortCols <- cohortColumns(table = "cohort"))

  # correct values
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "cohort", type == "cohort",
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    cohortCols
  )
})
