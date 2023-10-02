
test_that("checkInput behaviour", {
  expect_no_error(checkInput())
  expect_error(checkInput(adsdw = 1))
})
