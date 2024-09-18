test_that("test combineStrata", {
  expect_identical(combineStrata(c("age_group", "sex", "year")),
                list(
                  "age_group",
                  "sex",
                  "year",
                  c("age_group","sex"),
                  c("age_group","year"),
                  c("sex","year"),
                  c("age_group","sex","year")
                )
  )

  # Expected errors
  expect_error(combineStrata(c("sex")))
  expect_error(combineStrata(sex))
  expect_error(combineStrata(c(2,3)))
  expect_error(combineStrata(c("sex", NA)))
})
