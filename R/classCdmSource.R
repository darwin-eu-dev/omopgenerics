#' Create a cdm source object.
#'
#' @param src Source to a cdm object
#'
#' @export
#'
#' @return A validated cdm source object.
#'
cdmSource <- function(src) {
  # toy data
  name <- paste0(c(sample(letters, 5, replace = TRUE), "_test_table"), collapse = "")
  value <- cars

  # insert table
  x <- insertTable(src = src, name = name, value = value)

  # check inserted table
  if (!identical(dplyr::collect(x), value)) {
    cli::cli_abort("The inserted table was not the same than the provided one.")
  }

  # compute inserted table
  x <- x |> computeTable(x)

  # drop table
  dropTable(src = src, name = name)

  # assign class
  class(src) <- addClass(src, "cdm_source")

  return(src)
}

#' Source of a local cdm.
#'
#' @param name Name of the local cdm
#'
localCdm <- function(name) {
  checkmate::checkCharacter(name, len = 1, any.missing = FALSE)
  class(name) <- "local_cdm"
  return(name)
}

#' @export
print.local_cdm <- function(x) {
  cli::cli_inform("This is a local cdm connection to: {unclass(x)}")
}
