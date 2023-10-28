
test_that("checkInput behaviour", {
  expect_no_error(checkInput())
  expect_snapshot_error(checkInput(notCheckedVariable = 1))
})
