test_that("writing results", {
  zipFolder <- tempdir()
  unzipFolder <- file.path(tempdir(), "unzip")
  dir.create(unzipFolder)
  dbName <- "IPCI"

  dataSet <- mtcars %>% dplyr::mutate(cdm_name = dbName)
  exportResults(
    resultList = list("mtcars" = dataSet), zipName = "test",
    outputFolder = zipFolder
  )
  expect_true("test.zip" %in% list.files(zipFolder))
  # check files
  zip::unzip(file.path(zipFolder, "test.zip"), exdir = unzipFolder, junkpaths = TRUE)
  extractedFiles <- list.files(unzipFolder, full.names = F)
  expect_true(length(extractedFiles) == 1)
  expect_true(grepl(paste0(dbName, "_mtcars"), extractedFiles))

  unlink(zipFolder)
  unlink(unzipFolder)
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
