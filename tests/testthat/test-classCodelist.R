test_that("test codelist works", {

  # single codelist
  codes <- list("disease" = c(1,2,3))
  expect_no_error(codes <- newCodelist(codes))
  class <- codes |> newCodelist() |> class()
  expect_true(c("codelist") %in% class)
  expect_no_error(print(codes))

  # multiple codelists
  codes <- list("disease X" = c(1,2,3),
                "disease Y" = c(4,5))
  expect_no_error(codes <- newCodelist(codes))
  class <- codes |> newCodelist() |> class()
  expect_true(c("codelist") %in% class)
  expect_no_error(print(codes))

  # expected errors
  expect_error(newCodelist(c(1,2,3)))
  expect_error(newCodelist(list(c(1,2,3))))
  expect_error(newCodelist(list("disease" = dplyr::tibble("disease" = c(1,2,3)))))

  codes <- list("disease X" = c(1,NA,3),
                "disease Y" = c(4,5))
  expect_error(codes <- newCodelist(codes))

})
