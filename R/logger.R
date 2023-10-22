
#' Write message to a logger file associated to a cdm object.
#'
#' @param cdm A cdm reference.
#' @param message Message to record in the logger.
#' @param warn Weather to throw a warning if the logger does not exist.
#'
#' @export
#'
#' @return Invisible cdm object
#'
writeLogger <- function(message, warn = FALSE) {
  logger <- getOption("logger")
  # initial checks
  checkInput(message = message, warn = warn)

  # add line break
  message <- paste0("[", Sys.time(), "]  ", message, "\n")

  # write message
  file <- attr(cdm, "logger")
  if (!is.null(file)) {
    message <- c(readLines(con = file), message)
    writeLines(text = message, con = file)
  } else if (isTRUE(warn)) {
    cli::cli_warn("Logger not found. Message couldn't be written.")
  }

  invisible(cdm)
}
