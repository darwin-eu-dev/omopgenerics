test_that("uniteGroups", {
  tib <- dplyr::tibble(
    x = c("cohort_name", "cohort_name and age", "age and sex"),
    y = c("acetaminophen", "ibuprofen and 10 to 19", "20 to 29 and Male"),
    z = c(1, 2, 3),
    a = c("a", "b", "c")
  )
  expect_no_error(res <- tib |> uniteGroup("z"))
  expect_true("group_name" %in% colnames(res))
  expect_true("group_level" %in% colnames(res))
  expect_false("z" %in% colnames(res))
  expect_no_error(res <- tib |> uniteGroup("z", keep = TRUE))
  expect_true("group_name" %in% colnames(res))
  expect_true("group_level" %in% colnames(res))
  expect_true("z" %in% colnames(res))
  expect_no_error(res <- tib |> uniteGroup("z", name = "new_col", level ="new_col2"))
  expect_true("new_col" %in% colnames(res))
  expect_true("new_col2" %in% colnames(res))
  expect_no_error(res <- tib |> uniteGroup(c("z", "a")))
  expect_true("group_name" %in% colnames(res))
  expect_true("group_level" %in% colnames(res))
  expect_false(any(c("a", "z") %in% colnames(res)))
  expect_identical(res$group_name, c("z and a", "z and a", "z and a"))
  expect_identical(res$group_level, c("1 and a", "2 and b", "3 and c"))
  expect_identical(
    tib |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
    tib |> uniteGroup(c("z", "a")) |> splitGroup()
  )
  expect_error(tib |> uniteGroup(c("x")))
  expect_error(tib |> dplyr::rename("x and x" = "x") |> uniteGroup(c("x and x")))
  expect_warning(res <- tib |> uniteGroup("z", name = "x"))
})
