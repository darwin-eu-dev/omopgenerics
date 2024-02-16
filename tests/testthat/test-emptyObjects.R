
test_that("you cna create empty objects",{

  expect_error(emptyCdmReference())
  expect_no_error(cdm <- emptyCdmReference(cdmName = "test"))
  exepct_identical(cdmName(cdm), "test")
  expect_true(cdmVersion(cdm), "5.4")

})
