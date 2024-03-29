test_that("test supress methods", {
  x <- dplyr::tibble(
    "result_id" = NA_character_,
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
    c("10", "5", "50", NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 5)
  expect_identical(
    result$estimate_value,
    c("10", "5", "50", NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 6)
  expect_identical(
    result$estimate_value,
    c("10", NA, NA, NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 10)
  expect_identical(
    result$estimate_value,
    c("10", NA, NA, NA, NA, "1", NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 11)
  expect_identical(
    result$estimate_value,
    c(NA, NA, NA, NA, NA, NA, NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 12)
  expect_identical(
    result$estimate_value,
    c(NA, NA, NA, NA, NA, NA, NA, "12", "6")
  )
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

  result <- suppress(obj, minCellCount = 13)
  expect_identical(result$estimate_value, rep(NA_character_, 9))
  expect_identical(
    result |> dplyr::select(-"estimate_value"),
    obj |> dplyr::select(-"estimate_value")
  )

})
