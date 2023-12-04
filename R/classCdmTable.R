#' Extract a table from a cdm reference.
#'
#' @param table
#'
#' @return A table from the cdm reference
#'
#' @export
#'
cdmTable <- function(table) {
  addClass(table) <- "cdm_table"
  if ("generated_cohort_set" %in% class(table)) {
    addClass(attr(table, "cohort_set")) <- "cdm_table"
    addClass(attr(table, "cohort_attrition")) <- "cdm_table"
  }
  return(table)
}

#' @importFrom dplyr compute
#' @export
compute.cdm_table <- function(x, name) {
  computeTable(x = x, name = name)
}

#' @export
#' @importFrom dplyr arrange
arrange.cdm_table <- function(.data, ...) {
  if ("tbl_lazy" %in% class(.data)) {
    x <- dbplyr::window_order(.data, ...)
  } else {
    removeClass(.data) <- "cdm_table"
    x <- dplyr::arrange(.data, ...)
    addClass(.data) <- "cdm_table"
  }
  return(x)
}
