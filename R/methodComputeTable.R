#' Compute a cdm table.
#'
#' @param cdm A cdm reference.
#' @param name Name of the table to insert.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
computeTable <- function(x, name = NA_character_) {
  assertCharacter(name, length = 1, minNumCharacter = 1, na = TRUE)
  x <- UseMethod("computeTable", getCdmSource(x))
  attr(x, "tbl_name") <- name
  return(x)
}

#' @export
computeTable.local_cdm <- function(x, name) {
  return(x)
}
