#' Create a cdm source object.
#'
#' @param src Source to a cdm object.
#' @param sourceName Name of the source.
#' @param sourceType Type of the source object.
#'
#' @export
#'
#' @return A validated cdm source object.
#'
cdmSource <- function(src, sourceName, sourceType) {
  # check source
  {
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
  }
  assertCharacter(sourceName, length = 1, minNumCharacter = 1)
  assertCharacter(sourceType, length = 1, minNumCharacter = 1)

  # assign class
  class(src) <- addClass(src, "cdm_source")
  attr(src, "source_name") <- sourceName
  attr(src, "source_type") <- sourceType

  return(src)
}

#' @export
print.cdm_source <- function(x) {
  cli::cli_inform(
    "This is a {attr(x, 'source_type')} cdm connection to: {attr(x, 'source_name')}"
  )
}
