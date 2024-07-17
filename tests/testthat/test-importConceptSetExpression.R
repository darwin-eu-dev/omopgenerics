test_that("import concept set expression", {
  skip_if_not_installed("jsonlite")

  dir.create(cs_path <- file.path(tempdir(), omopgenerics::uniqueTableName()))
  asthma_cs <- newConceptSetExpression(list("asthma_narrow" = dplyr::tibble(
    "concept_id" = 1L,
    "excluded" = FALSE,
    "descendants" = TRUE,
    "mapped" = FALSE
  ),
  "asthma_broad" = dplyr::tibble(
    "concept_id" = c(1L,2L),
    "excluded" = FALSE,
    "descendants" = TRUE,
    "mapped" = FALSE
  )))
  expect_no_error(exportConceptSetExpression(x = asthma_cs,
                                 path = cs_path))
  expect_true("asthma_broad.json" %in% list.files(cs_path))
  expect_true("asthma_narrow.json" %in% list.files(cs_path))

  codes_imported <- importConceptSetExpression(path = cs_path)
  expect_identical(asthma_cs, codes_imported)

  # example concept sets
  x <- importConceptSetExpression(path =  system.file(package = "omopgenerics",
                                                      "concepts_for_mock")
  )
  expect_true(names(x[1]) == "oa_desc")
  expect_true(x[[1]]$excluded  == FALSE)
  expect_true(x[[1]]$descendants  == TRUE)
  expect_true(x[[1]]$mapped  == FALSE)

  expect_true(names(x[2]) == "oa_no_desc")
  expect_true(x[[2]]$excluded  == FALSE)
  expect_true(x[[2]]$descendants  == FALSE)
  expect_true(x[[2]]$mapped  == FALSE)

  # cohort jsons - won't work
  expect_warning(x <- importConceptSetExpression(path =  system.file(package = "omopgenerics",
                                     "cohorts_for_mock")))
  expect_true(length(x) == 0)

  # file with both cohorts and concept set jsons
  expect_warning(x <- importConceptSetExpression(path =  system.file(package = "omopgenerics",
                                                                     "not_all_concept_sets")))
  # 2 of the three were concept sets
  expect_true(length(x) == 2)

})
