test_that("test codelist works", {

  # single codelist
  codes <- list("disease" = c(1L,2L,3L))
  expect_no_error(codes <- newCodelist(codes))
  class <- codes |> newCodelist() |> class()
  expect_true(c("codelist") %in% class)
  expect_no_error(print(codes))

  # multiple codelists
  codes <- list("disease X" = c(1L,2L,3L),
                "disease Y" = c(4L, 5L))
  expect_no_error(codes <- newCodelist(codes))
  class <- codes |> newCodelist() |> class()
  expect_true(c("codelist") %in% class)
  expect_no_error(print(codes))


  expect_no_error(emptyCodelist())

  # expected errors
  expect_error(newCodelist(c(1L,2L,3L)))
  expect_error(newCodelist(list(c(1L,2L,3L))))

  codes <- list("disease" = dplyr::tibble(concept_id = c(1L,2L,3L),
                                          other_info = c("a", "b", "c")))


  expect_error(codes <- newCodelist(codes))

  codes <- list("disease X" = c(1L,NA,3L),
                "disease Y" = c(4L,5L))
  expect_error(codes <- newCodelist(codes))

  codes <- list("disease X" = c(1L,2L,3L),
                "disease Y" = as.character(c(4,5)))
  expect_error(codes <- newCodelist(codes))

  # codelist with identical names
  codes_identical <- list(a = c(123L),
            a = c(123L))

  expect_error(newCodelist(codes_identical))


})


