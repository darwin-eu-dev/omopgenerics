test_that("test supress methods", {
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "mock",
    "group_name" = "overall",
    "group_level" = "overall",
    "strata_name" = c(rep("overall", 6), rep("sex", 3)),
    "strata_level" = c(rep("overall", 6), "male", "female", "female"),
    "variable_name" = c("number records", "age_group", "age_group", "age_group", "age_group", "my_variable", "number records", "age_group", "age_group"),
    "variable_level" = c(NA, "<50", "<50", ">=50", ">=50", NA, NA, "<50", "<50"),
    "estimate_name" = c("count", "count", "percentage", "count", "percenatge", "random", "count", "count", "percentage"),
    "estimate_type" = c("integer", "integer", "percentage", "integer", "percentage", "numeric", "integer", "integer", "percentage"),
    "estimate_value" = c("10", "5", "50", "3", "30", "1", "3", "12", "6"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )

  obj <- newSummarisedResult(
    x,
    settings = dplyr::tibble(
      "result_id" = as.integer(1),
      "result_type" = "summarised_characteristics",
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics"))))

  settingsTest <- function(minCellCount) {
    dplyr::tibble(
      "result_id" = as.integer(1),
      "result_type" = "summarised_characteristics",
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics")),
      "min_cell_count" = minCellCount)
  }

  objOut <- newSummarisedResult(x, settings = settingsTest(2))
  result <- suppress(obj, minCellCount = 2)
  expect_identical(result, objOut)

  objOut <- newSummarisedResult(x, settings = settingsTest(3))
  result <- suppress(obj, minCellCount = 3)
  expect_identical(result, objOut)

  objOut <- newSummarisedResult(x, settings = settingsTest(4))
  result <- suppress(obj, minCellCount = 4)
  expect_identical(
    result$estimate_value,
    c("10", "5", "50", NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  objOut <- newSummarisedResult(x, settings = settingsTest(5))
  result <- suppress(obj, minCellCount = 5)
  expect_identical(
    result$estimate_value,
    c("10", "5", "50", NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  objOut <- newSummarisedResult(x, settings = settingsTest(6))
  result <- suppress(obj, minCellCount = 6)
  expect_identical(
    result$estimate_value,
    c("10", NA, NA, NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  objOut <- newSummarisedResult(x, settings = settingsTest(10))
  result <- suppress(obj, minCellCount = 10)
  expect_identical(
    result$estimate_value,
    c("10", NA, NA, NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  objOut <- newSummarisedResult(x, settings = settingsTest(11))
  result <- suppress(obj, minCellCount = 11)
  expect_identical(
    result$estimate_value,
    c(NA, NA, NA, NA, NA, NA, NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  objOut <- newSummarisedResult(x, settings = settingsTest(12))
  result <- suppress(obj, minCellCount = 12)
  expect_identical(
    result$estimate_value,
    c(NA, NA, NA, NA, NA, NA, NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  objOut <- newSummarisedResult(x, settings = settingsTest(13))
  result <- suppress(obj, minCellCount = 13)
  expect_identical(result$estimate_value, rep(NA_character_, 9))
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    objOut |> dplyr::select(-"estimate_value")
  )

  # test already suppress input
  expect_warning(result1 <- suppress(result, minCellCount = 10))
  expect_identical(result, result1)

  # contains count
  x <- dplyr::tibble(
    "result_id" = as.integer(1),
    "cdm_name" = "mock",
    "group_name" = "overall",
    "group_level" = "overall",
    "strata_name" ="overall",
    "strata_level" = "overall",
    "variable_name" = c("concept id name 1", "concept id name 1", "concept id name 2", "concept id name 2"),
    "variable_level" = NA,
    "estimate_name" = c("record_count", "person_count", "record_count", "person_count"),
    "estimate_type" = c("integer", "integer", "integer", "integer"),
    "estimate_value" = c("6", "3", "4", "4"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )

  obj <- newSummarisedResult(
    x,
    settings = dplyr::tibble(
      "result_id" = as.integer(1),
      "result_type" = "summarised_characteristics",
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics"))))

  result <- suppress(obj)
  expect_identical(result$estimate_value, c(6, NA_character_, NA_character_, NA_character_))
})
