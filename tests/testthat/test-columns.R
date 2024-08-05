
test_that("omop column functions work", {
  # basic functionality
  expect_no_error(omopColsReq <- omopColumns(table = "person", required = TRUE))
  expect_no_error(omopColsNre <- omopColumns(table = "person", required = FALSE))
  expect_no_error(omopColsAll <- omopColumns(table = "person", required = NULL))

  # correct values
  expect_identical(omopColsReq, intersect(omopColsReq, omopColsAll))
  expect_identical(omopColsNre, intersect(omopColsNre, omopColsAll))
  expect_identical(character(), intersect(omopColsReq, omopColsNre))
  expect_identical(sort(c(omopColsReq, omopColsNre)), sort(omopColsAll))
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
        cdm_table_name == "person", type == "cdm_table", is_required == FALSE,
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    omopColsNre
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
  expect_no_error(achillesColsReq <- achillesColumns(table = "achilles_analysis", required = TRUE))
  expect_no_error(achillesColsNre <- achillesColumns(table = "achilles_analysis", required = FALSE))
  expect_no_error(achillesColsAll <- achillesColumns(table = "achilles_analysis", required = NULL))

  # correct values
  expect_identical(achillesColsReq, intersect(achillesColsReq, achillesColsAll))
  expect_identical(achillesColsNre, intersect(achillesColsNre, achillesColsAll))
  expect_identical(character(), intersect(achillesColsReq, achillesColsNre))
  expect_identical(sort(c(achillesColsReq, achillesColsNre)), sort(achillesColsAll))
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
        cdm_table_name == "achilles_analysis", type == "achilles", is_required == FALSE,
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    achillesColsNre
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
  expect_no_error(cohortColsReq <- cohortColumns(table = "cohort", required = TRUE))
  expect_no_error(cohortColsNre <- cohortColumns(table = "cohort", required = FALSE))
  expect_no_error(cohortColsAll <- cohortColumns(table = "cohort", required = NULL))

  # correct values
  expect_identical(cohortColsReq, intersect(cohortColsReq, cohortColsAll))
  expect_identical(cohortColsNre, intersect(cohortColsNre, cohortColsAll))
  expect_identical(character(), intersect(cohortColsReq, cohortColsNre))
  expect_identical(sort(c(cohortColsReq, cohortColsNre)), sort(cohortColsAll))
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "cohort", type == "cohort", is_required == TRUE,
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    cohortColsReq
  )
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "cohort", type == "cohort", is_required == FALSE,
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    cohortColsNre
  )
  expect_identical(
    fieldsTables |>
      dplyr::filter(
        cdm_table_name == "cohort", type == "cohort",
        grepl("5.3", cdm_version)) |>
      dplyr::pull("cdm_field_name"),
    cohortColsAll
  )
})
