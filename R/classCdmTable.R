#' Extract a table from a cdm reference.
#'
#' @param cdm A cdm reference object.
#' @param name The name of the element that you want to extract.
#'
#' @return A table from the cdm reference
#'
#' @export
#'
cdmTable <- function(cdm, name) {
  # initial checks
  assertClass(cdm, "cdm_reference")
  assertCharacter(name, length = 1, minNumCharacter = 1)

  if (!name %in% names(cdm)) {
    cli::cli_warn("{name} is not present in the cdm reference.")
    return(NULL)
  }

  # extract table
  x_raw <- unclass(x)
  tbl <- x_raw[[i]]
  attr(tbl, "cdm_reference") <- cdm
  tbl <- addClass(tbl, "cdm_table")

  return(tbl)
}

#' @importFrom dplyr compute
#' @export
compute.cdm_table <- function(x, name = NA_character_) {
  computeTable(x)
}
