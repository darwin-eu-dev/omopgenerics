test_that("multiplication works", {
  expect_no_error(newLocalSource())
  expect_error(newLocalSource("test"))
  expect_error(newLocalSource(c("test", "test2")))
  expect_error(newLocalSource(1))
  expect_error(newLocalSource("test", "x"))
})
