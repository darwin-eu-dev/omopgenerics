test_that("test codelist works", {

  # single codelist
  codes <- list("disease" = c(1,2,3))
  expect_no_error(codes <- codelist(codes))
  class <- codes |> codelist() |> class()
  expect_true(c("codelist") %in% class)
  print(codes)

  # multiple codelists
  codes <- list("disease X" = c(1,2,3),
                "disease Y" = c(4,5,6))
  expect_no_error(codes <- codelist(codes))
  class <- codes |> codelist() |> class()
  expect_true(c("codelist") %in% class)
  print(codes)

  # expected errors
  codelist(c(1,2,3))
  codelist(list(c(1,2,3)))
  codelist(list("disease" = dplyr::tibble("disease" = c(1,2,3))))


})
