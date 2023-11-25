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
  return(tbl)
}

#' @importFrom dplyr compute
#' @export
compute.cdm_table <- function(x, name = NA_character_) {
  computeTable(x)
}
