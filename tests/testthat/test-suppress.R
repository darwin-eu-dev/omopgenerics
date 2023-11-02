test_that("test supress methods", {
  x <- dplyr::tibble(
    "cdm_name" = c("cprd", "cprd"),
    "result_type" = c("Summarised characteristics", "Summarised characteristics"),
    "package" = c("patientProfiles", "patientProfiles"),
    "package_version" = c("0.4.0", "0.4.0"),
    "group_name" = c("na", "sex"),
    "group_level" = c("na", "male"),
    "strata_name" = c("na", "sex"),
    "strata_level" = c("na", "male"),
    "variable" = c("number records", "age_group"),
    "variable_level" = c("na", "10 to 50"),
    "variable_type" = c("numeric", "numeric"),
    "estimate_type" = c("count", "count"),
    "estimate" = c("10", "5")
  )

  obj <- OMOPGenerics::summarisedResult(x)

  result <- OMOPGenerics::suppress(obj,
                                   minCellCount = 8,
                                   groupCount = "number records")

  expect_true(all(result$estimate %in% c("<8", 10)))

  result <- OMOPGenerics::suppress(obj,
                                   minCellCount = 3,
                                   groupCount = "number records")

  expect_true(all(result$estimate %in% c(5, 10)))


  result <- OMOPGenerics::suppress(obj,
                                   minCellCount = 20,
                                   groupCount = "number records")

  expect_true(all(result$estimate %in% c("<20", "<20")))

})
