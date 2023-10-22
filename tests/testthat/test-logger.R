test_that("link logger", {
  # format error
  cdm <- list()
  file <- 12434
  expect_error(linkLogger(cdm = cdm, file = file))
  class(cdm) <- "cdm_reference"
  expect_error(linkLogger(cdm = cdm, file = file))


  expect_true(is.null(attr(cdm, "logger")))
  f <- tempfile()
  cdm <- linkLogger()

})
