#' Create a source for a local cdm.
#'
#' @param name Name of the local cdm.
#'
localSource <- function(name) {
  assertCharacter(name, length = 1, minNumCharacter = 1)
  source <- NA
  class(source) <- "local_cdm"
  source <- cdmSource(src = source, sourceName = name, sourceType = "local")
  return(source)
}
