#' Read a table from a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name of the table to read.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
readTable <- function(src, name) {
  UseMethod("readTable")
}

#' @export
readTable.cdm_reference <- function(src, name) {
  cdm <- src
  if (name %in% names(cdm)) {
    cli::cli_warn(
      "There is an object with same name ({name}) in the cdm, it will be
      overwriten."
    )
  }
  cdm[[name]] <- readTable(src = getCdmSource(cdm), name = name)
  return(cdm)
}

#' @export
readTable.local_source <- function(src, name) {
  cli::cli_warn("You can't read tables in a local source.")
  return(NULL)
}
