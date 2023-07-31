test_that("test vocab", {
  expect_no_error(cdm <- mockVocabularyCdm())
  expect_no_error(cdm <- mockVocabularyCdm(concept = dplyr::tibble(
    concept_id = c(1, 2), concept_name = c("asthma", "copd"),
    domain_id = "Condition", vocabulary_id = "SNOMED",
    concept_class_id = "Condition", standard_concept = "S",
    concept_code = c("m1", "m2"), valid_start_date = as.Date("2000-01-01"),
    valid_end_date = as.Date("2099-12-31 "), invalid_reason = NA
  )))
  expect_no_error(cdm <- mockVocabularyCdm(concept = dplyr::tibble(
    concept_id = c(1, 2), concept_name = c("asthma", "copd"),
    domain_id = "Condition",
    concept_class_id = "Condition", standard_concept = "S",
    concept_code = c("m1", "m2"), valid_start_date = as.Date("2000-01-01"),
    valid_end_date = as.Date("2099-12-31 ")
  )))
  expect_true("vocabulary_id" %in% colnames(cdm$concept))
  expect_warning(cdm <- mockVocabularyCdm(concept = dplyr::tibble(
    concept_id = c(1, 2), concept_name = c("asthma", "copd"),
    domain_id = "Condition", vocabulary_id = "SNOMED",
    concept_class_id = "Condition", standard_concept = "S",
    concept_code = c("m1", "m2"), valid_start_date = as.Date("2000-01-01"),
    valid_end_date = as.Date("2099-12-31 "), reason_id = 1
  )))
})
