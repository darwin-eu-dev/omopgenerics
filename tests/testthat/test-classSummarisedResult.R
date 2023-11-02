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

 class <- x %>% OMOPGenerics::summarisedResult() %>% class()
 expect_true(c("summarised_result") %in% class)


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

 # check NA
 x <- dplyr::tibble(   "cdm_name" = NA,
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

})

test_that("test comparedResult object", {

  x <- dplyr::tibble(    "cdm_name" = c("cprd","cprd"),
                         "result_type" = c("Compared result","Compared result"),
                         "package" = c("OmopCausal","OmopCausal"),
                         "package_version" = c("0.0.1","0.0.2"),
                         "group_name_reference" = c("sex","sex"),
                         "group_level_reference" = c("male","male"),
                         "strata_name_reference" = c("sex","sex"),
                         "strata_level_reference" = c("male","male"),
                         "group_name_comparator" = c("sex","sex"),
                         "group_level_comparator" = c("male","male"),
                         "strata_name_comparator" = c("sex","sex"),
                         "strata_level_comparator" = c("male","male"),
                         "variable" = c("age_group","age_group"),
                         "variable_level" = c("10 to 50","50+"),
                         "variable_type"= c("binary","binary"),
                         "estimate_type"= c("count","count"),
                         "estimate"= c("6","7"))

  class <- x %>% OMOPGenerics::comparedResult() %>% class()

  expect_no_error(OMOPGenerics::comparedResult(x = x))
  expect_true(c("compared_result") %in% class)

  # check none character
  x <- dplyr::tibble(    "cdm_name" = c(1,1),
                         "result_type" = c("Compared result","Compared result"),
                         "package" = c("OmopCausal","OmopCausal"),
                         "package_version" = c("0.0.1","0.0.2"),
                         "group_name_reference" = c("sex","sex"),
                         "group_level_reference" = c("male","male"),
                         "strata_name_reference" = c("sex","sex"),
                         "strata_level_reference" = c("male","male"),
                         "group_name_comparator" = c("sex","sex"),
                         "group_level_comparator" = c("male","male"),
                         "strata_name_comparator" = c("sex","sex"),
                         "strata_level_comparator" = c("male","male"),
                         "variable" = c("age_group","age_group"),
                         "variable_level" = c("10 to 50","50+"),
                         "variable_type"= c("binary","binary"),
                         "estimate_type"= c("count","count"),
                         "estimate"= c("6","7"))

  expect_error(OMOPGenerics::comparedResult(x = x))

  # check none snake_case
  x <- dplyr::tibble(    "cdm_name" = c("cprd","cprd"),
                         "result_type" = c("Compared result","Compared result"),
                         "package" = c("OmopCausal","OmopCausal"),
                         "package_version" = c("0.0.1","0.0.2"),
                         "group_name_reference" = c("sexAnd","sex"),
                         "group_level_reference" = c("male","male"),
                         "strata_name_reference" = c("sex","sex"),
                         "strata_level_reference" = c("male","male"),
                         "group_name_comparator" = c("sex","sex"),
                         "group_level_comparator" = c("male","male"),
                         "strata_name_comparator" = c("sex","sex"),
                         "strata_level_comparator" = c("male","male"),
                         "variable" = c("age_group","age_group"),
                         "variable_level" = c("10 to 50","50+"),
                         "variable_type"= c("binary","binary"),
                         "estimate_type"= c("count","count"),
                         "estimate"= c("6","7"))

  expect_error(OMOPGenerics::comparedResult(x = x))

  #check none sentence case

  x <- dplyr::tibble(    "cdm_name" = c("cprd","cprd"),
                         "result_type" = c("Compared Result","Compared result"),
                         "package" = c("OmopCausal","OmopCausal"),
                         "package_version" = c("0.0.1","0.0.2"),
                         "group_name_reference" = c("sex","sex"),
                         "group_level_reference" = c("male","male"),
                         "strata_name_reference" = c("sex","sex"),
                         "strata_level_reference" = c("male","male"),
                         "group_name_comparator" = c("sex","sex"),
                         "group_level_comparator" = c("male","male"),
                         "strata_name_comparator" = c("sex","sex"),
                         "strata_level_comparator" = c("male","male"),
                         "variable" = c("age_group","age_group"),
                         "variable_level" = c("10 to 50","50+"),
                         "variable_type"= c("binary","binary"),
                         "estimate_type"= c("count","count"),
                         "estimate"= c("6","7"))

  expect_error(OMOPGenerics::comparedResult(x = x))


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

  expect_error(OMOPGenerics::comparedResult(x = x))

  # check NA
  x <- dplyr::tibble(    "cdm_name" = c(NA,NA),
                         "result_type" = c("Compared result","Compared result"),
                         "package" = c("OmopCausal","OmopCausal"),
                         "package_version" = c("0.0.1","0.0.2"),
                         "group_name_reference" = c("sex","sex"),
                         "group_level_reference" = c("male","male"),
                         "strata_name_reference" = c("sex","sex"),
                         "strata_level_reference" = c("male","male"),
                         "group_name_comparator" = c("sex","sex"),
                         "group_level_comparator" = c("male","male"),
                         "strata_name_comparator" = c("sex","sex"),
                         "strata_level_comparator" = c("male","male"),
                         "variable" = c("age_group","age_group"),
                         "variable_level" = c("10 to 50","50+"),
                         "variable_type"= c("binary","binary"),
                         "estimate_type"= c("count","count"),
                         "estimate"= c("6","7"))

  expect_error(OMOPGenerics::comparedResult(x = x))

})
