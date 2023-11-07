test_that("bind no class", {
  x <- 1
  expect_error(bind(x))
})
