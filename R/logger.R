
#' Write message to a log file.
#'
#' @param message Message to record in the logger.
#' @param warn Weather to throw a warning if the logger does not exist.
#'
#' @export
#'
#' @return Invisible cdm object
#'
writeLog <- function(message, warn = TRUE) {
  logger <- getOption("logger")

  if (!is.null(logger)) {
    # initial checks
    checkInput(message = message, warn = warn, logger = logger)

    # add line break
    message <- paste0("[", Sys.time(), "]  ", message, "\n")

    # write message
    if (file.exists(logger)) {
      message <- c(readLines(con = logger), message)
    }
    writeLines(text = message, con = logger)
  } else if (isTRUE(warn)) {
    cli::cli_warn(c(
      "Logger not found. Message couldn't be written.",
      "To create a logger: options(logger = \"path\")"
    ))
  }

  invisible(TRUE)
}
