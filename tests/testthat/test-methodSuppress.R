test_that("test supress methods", {
  x <- dplyr::tibble(
    "cdm_name" = "mock",
    "result_type" = "summarised_characteristics",
    "package_name" = "omopgenerics",
    "package_version" = as.character(utils::packageVersion("omopgenerics")),
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

  obj <- newSummarisedResult(x)

  result <- suppress(obj, minCellCount = 2)
  expect_identical(result, obj)

  result <- suppress(obj, minCellCount = 3)
  expect_identical(result, obj)

  result <- suppress(obj, minCellCount = 4)
  expect_identical(
    result$estimate_value,
    c("10", "5", "50", "<4", NA, "1", "<4", "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 5)
  expect_identical(
    result$estimate_value,
    c("10", "5", "50", "<5", NA, "1", "<5", "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 6)
  expect_identical(
    result$estimate_value,
    c("10", "<6", NA, "<6", NA, "1", "<6", "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 10)
  expect_identical(
    result$estimate_value,
    c("10", "<10", NA, "<10", NA, "1", "<10", "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 11)
  expect_identical(
    result$estimate_value,
    c("<11", NA, NA, NA, NA, NA, "<11", "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 12)
  expect_identical(
    result$estimate_value,
    c("<12", NA, NA, NA, NA, NA, "<12", "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 13)
  expect_identical(
    result$estimate_value,
    c("<13", NA, NA, NA, NA, NA, "<13", "<13", NA)
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(
    obj, minCellCount = 13, suppressedValue = "x", suppressedGroup = "y"
  )
  expect_identical(
    result$estimate_value,
    c("x", "y", "y", "y", "y", "y", "x", "x", "y")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(
    obj, minCellCount = 13, suppressedValue = "x", suppressedGroup = NULL
  )
  expect_true(nrow(result) == 3)
  expect_true(all(result$estimate_value == "x"))

  result <- suppress(
    obj, minCellCount = 13, suppressedValue = NULL, suppressedGroup = "y"
  )
  expect_true(nrow(result) == 4)
  expect_true(all(result$estimate_value == "y"))

  result <- suppress(
    obj, minCellCount = 13, suppressedValue = NULL, suppressedGroup = NULL
  )
  expect_true(nrow(result) == 0)

  result <- suppress(obj, minCellCount = 6, suppressedGroup = NULL)
  expect_true(nrow(result) == 7)

  result <- suppress(obj, minCellCount = 6, suppressedValue = NULL)
  expect_true(nrow(result) == 6)

  result <- suppress(
    obj, minCellCount = 6, suppressedValue = NULL, suppressedGroup = NULL
  )
  expect_true(nrow(result) == 4)

})
