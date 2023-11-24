#' Drop a table from a cdm object.
#'
#' @param cdm A cdm reference.
#' @param name Name(s) of the table(s) to insert. Tidyselect statments are
#' supported.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
dropCdmTable <- function(cdm, name) {
  UseMethod("insertCdmTable", cdmSource(cdm))
}

#' @export
dropCdmTable.local_cdm <- function(cdm, name) {

}
