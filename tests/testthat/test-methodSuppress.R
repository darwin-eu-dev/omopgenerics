test_that("test supress methods", {
  x <- dplyr::tibble(
    "cdm_name" = c("cprd", "cprd"),
    "result_type" = c("summarised_characteristics", "summarised_characteristics"),
    "package_name" = c("PatientProfiles", "PatientProfiles"),
    "package_version" = c("0.4.0", "0.4.0"),
    "group_name" = c("overall", "sex"),
    "group_level" = c("overall", "male"),
    "strata_name" = c("overall", "sex"),
    "strata_level" = c("overall", "male"),
    "variable_name" = c("number records", "age_group"),
    "variable_level" = c(NA, "10 to 50"),
    "variable_type" = c("numeric", "numeric"),
    "estimate_name" = c("count", "count"),
    "estimate_type" = c("numeric", "numeric"),
    "estimate_value" = c("10", "5"),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )

  obj <- summarisedResult(x)

  result <- suppress(obj, minCellCount = 8)

  expect_true(all(result$estimate_value %in% c("<8", 10)))

  result <- suppress(obj, minCellCount = 3)

  expect_true(all(result$estimate_value %in% c(5, 10)))

  result <- suppress(obj, minCellCount = 20)

  expect_true(all(result$estimate_value %in% c("<20", "<20")))

})
