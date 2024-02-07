test_that("writing results", {
  exportResults(
    resultList = list("mtcars" = mtcars), zipName = "test",
    outputFolder = tempdir()
  )
  expect_true("test.zip" %in% list.files(tempdir()))
})

test_that("writing results- expected errors", {
  # not a  result
  expect_error(exportResults(
    result = "a",
    zipName = "test_should_fail",
    outputFolder = tempdir()
  ))
  expect_error(exportResults(
    result = list(prev, "a"),
    zipName = "test_should_fail",
    outputFolder = tempdir()
  ))
  # zipName not length 1
  expect_error(exportResults(
    result = results,
    zipName = c("a", "test_should_fail"),
    outputFolder = tempdir()
  ))
  # outputFolder not a valid path
  expect_error(exportResults(
    result = results, zipName = "test_should_fail",
    outputFolder = "a"
  ))
  # outputFolder not an existing path
  expect_error(exportResults(
    result = results, zipName = "test_should_fail",
    outputFolder = file.path(tempdir(), "doesn_not_exist")
  ))
})
