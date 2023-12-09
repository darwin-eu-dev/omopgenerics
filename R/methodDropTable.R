#' Drop a table from a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name(s) of the table(s) to insert. Tidyselect statments are
#' supported.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
dropTable <- function(src, name) {
  UseMethod("dropTable")
}

#' @export
dropTable.cdm_reference <- function(src, name) {
  cdm <- src
  dropTable(src = getCdmSource(cdm), name = name)
  allTables <- names(cdm)
  names(allTables) <- names(cdm)
  toDrop <- names(tidyselect::eval_select(rlang::enquo(name), data = allTables))
  cdm[[toDrop]] <- NULL
  return(invisible(cdm))
}

#' @export
dropTable.local_cdm <- function(src, name) {
  return(invisible(TRUE))
}
