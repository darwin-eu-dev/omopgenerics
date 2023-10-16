
#' Link a logger file to a cdm object
#'
#' @param cdm A cdm reference.
#' @param file A path to an existing file or place to create it.
#'
#' @export
#'
#' @return The cdm object with the linked logger.
#'
linkLogger <- function(cdm, file) {
  # initial checks
  checkInput(cdm = cdm, file = file)

  # create file if it does not exist
  if (!file.exists(file)) {
    file.create(file)
  }

  # link the logger to the cdm
  attr(cdm, "logger") <- file

  return(cdm)
}

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
writeLogger <- function(cdm, message, warn = FALSE) {
  # initial checks
  checkInput(cdm = cdm, message = message, warn = warn)

  # add line break
  message <- paste0(message, "\n")

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
