
test_that("omop column functions work", {
  # correct values
  expect_identical(
    fieldsTables[["5.3"]] |>
      dplyr::filter(cdm_table_name == "person", type == "cdm_table") |>
      dplyr::pull("cdm_field_name"),
    omopColumns("person")
  )
})

test_that("achilles column functions work", {
  # correct values
  expect_identical(
    fieldsTables[["5.3"]] |>
      dplyr::filter(
        cdm_table_name == "achilles_analysis", type == "achilles",
      ) |>
      dplyr::pull("cdm_field_name"),
    achillesColumns("achilles_analysis")
  )
})

test_that("cohort column functions work", {
  # basic functionality
  expect_no_error(cohortCols <- cohortColumns(table = "cohort"))

  # correct values
  expect_identical(
    fieldsTables[["5.3"]] |>
      dplyr::filter(cdm_table_name == "cohort", type == "cohort") |>
      dplyr::pull("cdm_field_name"),
    cohortCols
  )
})
