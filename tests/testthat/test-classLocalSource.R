test_that("multiplication works", {
  expect_no_error(localSource("test"))
  expect_error(localSource(c("test", "test2")))
  expect_error(localSource(1))
  expect_error(localSource("test", "x"))
})
