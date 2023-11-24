#' Insert a table to a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
insertTable <- function(src, name, table) {
  UseMethod("insertTable")
}

#' @export
insertTable.cdm_reference <- function(src, name, table) {
  insertTable(src = getCdmSource(src), name = name, table = table)
}

#' @export
insertTable.local_cdm <- function(src, name, table) {

}
