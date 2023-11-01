test_that("test SummarisedResult object", {

  x <- dplyr::tibble(   "cdm_name" = "cprd",
                        "result_type" = "Summarised characteristics",
                        "package" = "patientProfiles",
                        "package_version" = "0.4.0",
                        "group_name" = "sex",
                        "group_level" = "male",
                        "strata_name" = "sex",
                        "strata_level" = "male",
                        "variable" = "age_group",
                        "variable_level" = "10 to 50",
                        "variable_type" = "date",
                        "estimate_type" = "count",
                        "estimate" = "5")

 expect_no_error(OMOPGenerics::summarisedResult(x = x))

 # check none character
 x <- dplyr::tibble(   "cdm_name" = 1,
                       "result_type" = "Summarised characteristics",
                       "package" = "patientProfiles",
                       "package_version" = "0.4.0",
                       "group_name" = "sex",
                       "group_level" = "male",
                       "strata_name" = "sex",
                       "strata_level" = "male",
                       "variable" = "age_group",
                       "variable_level" = "10 to 50",
                       "variable_type" = "date",
                       "estimate_type" = "count",
                       "estimate" = "5")

 expect_error(OMOPGenerics::summarisedResult(x = x))

 # check none snake_case
 x <- dplyr::tibble(   "cdm_name" = "cprd",
                       "result_type" = "Summarised characteristics",
                       "package" = "patientProfiles",
                       "package_version" = "0.4.0",
                       "group_name" = "sex hello",
                       "group_level" = "male",
                       "strata_name" = "sex",
                       "strata_level" = "male",
                       "variable" = "age_group",
                       "variable_level" = "10 to 50",
                       "variable_type" = "date",
                       "estimate_type" = "count",
                       "estimate" = "5")

 expect_error(OMOPGenerics::summarisedResult(x = x))

 #check none sentence case

 x <- dplyr::tibble(   "cdm_name" = "cprd",
                       "result_type" = "Summarised Characteristics",
                       "package" = "patientProfiles",
                       "package_version" = "0.4.0",
                       "group_name" = "sex",
                       "group_level" = "male",
                       "strata_name" = "sex",
                       "strata_level" = "male",
                       "variable" = "age_group",
                       "variable_level" = "10 to 50",
                       "variable_type" = "date",
                       "estimate_type" = "count",
                       "estimate" = "5")

 expect_error(OMOPGenerics::summarisedResult(x = x))


 #check wrong columns

 x <- dplyr::tibble(
                       "package" = "patientProfiles",
                       "package_version" = "0.4.0",
                       "group_name" = "sex",
                       "group_level" = "male",
                       "strata_name" = "sex",
                       "strata_level" = "male",
                       "variable" = "age_group",
                       "variable_level" = "10 to 50",
                       "variable_type" = "date",
                       "estimate_type" = "count",
                       "estimate" = "5")

 expect_error(OMOPGenerics::summarisedResult(x = x))

})
