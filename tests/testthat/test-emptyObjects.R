
test_that("you cna create empty objects",{

  # cdm_reference
  expect_error(emptyCdmReference())
  expect_no_error(cdm <- emptyCdmReference(cdmName = "test"))
  expect_identical(cdmName(cdm), "test")
  expect_true(cdmVersion(cdm) == "5.3")
  expect_no_error(cdm <- emptyCdmReference(cdmName = "test", cdmVersion = "5.4"))
  expect_true(cdmVersion(cdm) == "5.4")
  expect_no_error(cdm <- emptyCdmReference(cdmName = "test", cdmVersion = "5.3"))
  expect_true(cdmVersion(cdm) == "5.3")
  expect_error(cdm <- emptyCdmReference(cdmName = "test", cdmVersion = "5.4.4"))

  # achilles table
  expect_error(cdm <- emptyAchillesTable(cdm = cdm, name = "asdfs"))
  for (nm in achillesTables()) {
    expect_no_error(cdm <- emptyAchillesTable(cdm = cdm, name = nm))
    expect_true(nm %in% names(cdm))
    expect_true(inherits(cdm[[nm]], "achilles_table"))
    expect_true(all(achillesColumns(nm) %in% colnames(cdm[[nm]])))
  }

  # omop table
  expect_error(cdm <- emptyOmopTable(cdm = cdm, name = "asdfs"))
  for (nm in omopTables()) {
    expect_no_error(cdm <- emptyOmopTable(cdm = cdm, name = nm))
    expect_true(nm %in% names(cdm))
    expect_true(inherits(cdm[[nm]], "omop_table"))
    expect_true(all(colnames(omopColumns(nm) %in% cdm[[nm]])))
  }

  # cohort table
  expect_no_error(cdm <- emptyCohortTable(cdm = cdm, name = "my_cohort"))
  expect_true("my_cohort" %in% names(cdm))
  expect_true(inherits(cdm$my_cohort, "cohort_table"))
  expect_true(cdm$my_cohort |> collect() |> nrow() == 0)
  expect_true(all(
    cohortColumns("cohort") %in% (cdm$my_cohort |> colnames())
  ))
  expect_true(inherits(settings(cdm$my_cohort), "data.frame"))
  expect_true(cdm$my_cohort |> settings() |> nrow() == 0)
  expect_true(all(
    cohortColumns("cohort_set") %in% (cdm$my_cohort |> settings() |> colnames())
  ))
  expect_true(inherits(attrition(cdm$my_cohort), "data.frame"))
  expect_true(cdm$my_cohort |> attrition() |> nrow() == 0)
  expect_true(all(
    cohortColumns("cohort_attrition") %in% (cdm$my_cohort |> attrition() |> colnames())
  ))
  expect_error(cdm <- emptyCohortTable(cdm = cdm, name = "person"))

  # summarised result
  expect_no_error(x <- emptySummarisedResult())
  expect_true(inherits(x, "summarised_result"))
  expect_identical(settings(x), dplyr::tibble("result_id" = integer()))

  set <- dplyr::tibble("result_id" = 1L, "my_setting" = TRUE)
  expect_no_error(x <- emptySummarisedResult(settings = set))
  expect_true(inherits(x, "summarised_result"))
  expect_identical(settings(x), set)

})
