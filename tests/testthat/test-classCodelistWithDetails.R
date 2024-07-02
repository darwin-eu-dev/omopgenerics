test_that("test codelist with details", {

  # single codelist
  codes <- list("disease" = dplyr::tibble(concept_id = c(1,2,3),
                                          other_info = c("a", "b", "c")))

  expect_no_error(codes <- newCodelistWithDetails(codes))
  expect_true(inherits(codes, "codelist_with_details"))
  expect_no_error(print(codes))


  # multiple codelists
  codes <- list("disease X" = dplyr::tibble(concept_id = c(1,2,3),
                                            other_info = c("a", "b", "c")),
                "disease Y" = dplyr::tibble(concept_id = c(4,5),
                                            other_info = c("d", "e")))
  expect_no_error(codes <- newCodelistWithDetails(codes))
  expect_true(inherits(codes, "codelist_with_details"))
  expect_no_error(print(codes))

  expect_no_error(emptyCodelistWithDetails())

  # expected errors
  codes <- list("disease" = c(1,2,3))
  expect_error(codes <- newCodelistWithDetails(codes))

  codes <- list("disease" = dplyr::tibble(other_info = c("a", "b", "c")))
  expect_error(codes <- newCodelistWithDetails(codes))

  expect_error(newCodelistWithDetails(list(dplyr::tibble(concept_id = c(1,2,3)))))
  # cannot be mixed
  codes <- list("disease X" = dplyr::tibble(concept_id = c(1,2,3),
                                            other_info = c("a", "b", "c")),
                "disease Y" = c(4,5))
  expect_error(codes <- newCodelistWithDetails(codes))


  codes <- list("disease" = c(1,2,3))
  expect_error(codes <- newCodelistWithDetails(codes))

  expect_no_error(emptyCodelistWithDetails())

})
