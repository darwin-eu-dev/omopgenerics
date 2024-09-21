test_that("test export codelist", {

  skip_if_not_installed("jsonlite")

  # single codelist
  codes <- newCodelist(list("disease" = c(1L,2L,3L)))
  cs_path <- tempdir("concepts")
  expect_no_error(exportCodelist(x = codes,
                 path = cs_path))
  expect_true("disease.json" %in% list.files(cs_path))

  # multiple codelists
  codes <- list("disease X" = c(1L,2L,3L),
                "disease Y" = c(4L,5L))
  cs_path <- tempdir("concepts")
  expect_no_error(exportCodelist(x = codes,
                 path = cs_path))
  expect_true("disease X.json" %in% list.files(cs_path))
  expect_true("disease Y.json" %in% list.files(cs_path))

  # expect error
  expect_error(exportCodelist(x = "not codes",
                                 path = cs_path))
  expect_error(exportCodelist(x = codes,
                                 path = "not a path"))


  })
