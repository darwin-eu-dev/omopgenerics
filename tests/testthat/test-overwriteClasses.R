test_that("test that classes and attributes are keep", {

  # add class and attributes
  x <- dplyr::tibble(a = 1) |>
    cdmTable(src = localSource(), name = "xuxudecrema")

  # nothing
  funs <- c("count", "rowwise", "tally", "ungroup")
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x)"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # a column
  funs <- c("group_by")
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x, a)"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # join functions
  funs <- c(
    "anti_join", "full_join", "inner_join", "left_join", "nest_join",
    "right_join",  "semi_join"
  )
  y <- dplyr::tibble(a = 1:5)
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x, y, by = 'a')"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # no by
  funs <- c("cross_join", "union", "union_all")
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x, y)"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # summarise
  expect_no_error(xn <- x |> dplyr::summarise(n = dplyr::n()))
  expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
  expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
  expect_true("cdm_table" %in% class(xn))
})

