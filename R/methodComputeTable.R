#' Compute a cdm table.
#'
#' @param x A table.
#' @param cdm A cdm reference.
#' @param name Name of the table to insert.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
computeTable <- function(x, cdm, name = NA_character_) {
  assertCharacter(name, length = 1, minNumCharacter = 1, na = TRUE)
  assertClass(cdm, "cdm_reference")
  UseMethod("computeTable", getCdmSource(cdm))
}

#' @export
computeTable.local_cdm <- function(x, cdm, name) {
  attr(x, "tbl_name") <- name
  attr(x, "cdm_reference") <- cdm
  return(x)
}
