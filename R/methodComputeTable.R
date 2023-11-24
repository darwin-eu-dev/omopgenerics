#' Compute a cdm table.
#'
#' @param cdm A cdm reference.
#' @param name Name of the table to insert.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
computeCdmTable <- function(x, name) {
  UseMethod("insertCdmTable", cdmSource(x))
}

#' @export
computeCdmTable.local_cdm <- function(x, name) {

}
