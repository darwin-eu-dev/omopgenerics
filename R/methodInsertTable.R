#' Insert a table to a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The table in the cdm reference or the cdm reference.
#'
insertTable <- function(src, name, table) {
  assertCharacter(name, length = 1, minNumCharacter = 1, na = TRUE)
  assertClass(table, "data.frame")
  UseMethod("insertTable")
}

#' Insert a table to a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The cdm reference.
#'
insertTable.cdm_reference <- function(src, name, table) {
  table <- insertTable(src = getCdmSource(src), name = name, table = table)
  attr(table, "cdm_reference") <- src
  return(table)
}

#' Insert a table to a local cdm object.
#'
#' @param src The source of a local cdm.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
insertTable.local_cdm <- function(src, name, table) {
  attr(table, "tbl_name") <- name
  return(table)
}
