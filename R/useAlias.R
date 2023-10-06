# find all functions
# detect the ones that need reexport
# create the new functions in the same file

#' Function to reexport other functions using camelCase or snake_case.
#'
#' @param originalCase Casing of functions to be reexported.
#' @param newCase Casing of the new created functions.
#'
useAlias <- function(originalCase = c("camelCase", "snake_case"),
                     newCase = c("camelCase", "snake_case")) {
  # initial checks
  checkInput(originalCase = originalCase, newCase = newCase)

  # locate existing functions
  funs <- locateFunctions()

  # detect functions
  functions <- existingFunctions(funs)

  # functions to create alias
  newAlias <- aliasToCreate(functions, funs)

  if (nrow(newAlias) > 0) {
    # inform alias
    informAlias(newAlias)
    askConfirmation()
    # create new alias
    createNewAlias(newAlias)
  } else {
    cli::cli_inform("No alias to create detected")
  }

  invisible(TRUE)
}

existingFunctions <- function() {
  x <- readLines("NAMESPACE")
  x <- x[grepl(pattern = "export", x = substr(x = x, start = 1, stop = 6))]
  x <- substr(x = x, start = 8, stop = nchar(x) - 1)
  return(x)
}
aliasToCreate <- function(functions) {
  dplyr::tibble(existing_function = functions) |>
    dplyr::mutate(
      snake_case = toSnakeCase(.data$existing_function),
      camelCase = toCamelCase(.data$existing_function)
    ) |>
    dplyr::filter(.data$snake_case != .data$camelCase) |>
    dplyr::mutate(original_case = dplyr::if_else(
      .data$existing_function == .data$snake_case, "snake_case", "camelCase"
    )) |>
    dplyr::filter(.data$original_case %in% .env$originalCase) |>
    dplyr::left_join(
      tidyr::expand_grid(original_case = originalCase, new_case = newCase),
      by = "original_case", relationship = "many-to-many"
    ) |>
    dplyr::mutate(new_name = dplyr::if_else(
      .data$new_case == "snake_case", .data$snake_case, .data$camelCase
    )) |>
    dplyr::filter(!(.data$new_name %in% .env$functions)) |>
    dplyr::select("existing_function", "original_case", "new_name", "new_case")
}
informAlias <- function(newAlias) {
  cli::cli_inform("A total of {nrow(newAlias)} new alias will be created:")
  for (k in seq_len(nrow(newAlias))) {
    cli::cli_inform(
      "+ {newAlias$existing_function[k]} ({newAlias$original_case[k]}) -->
      {newAlias$new_name[k]} ({newAlias$new_case[k]})"
    )
  }
}
askConfirmation <- function() {
  while(TRUE) {
    cli::cli_inform(c(
      "All these new alias will be created in alias.R.",
      "Are you sure you want to continue? Y/n"
    ))
    x <- readline()
    if (x == "Y") {
      cli::cli_inform("The files will be created")
      break
    } else if (x == "n") {
      cli::cli_abort("Files not created")
    }
  }
}
locateFunctions <- function() {
  details <- dplyr::tibble(
    file_name = character(), function_name = character(),
    line_start = numeric()
  )
  x <- list.files("R")
  for (k in seq_along(x)) {
    f <- readLines(paste0("R/", x[k]))
    idE <- which(grepl(pattern = "#' @export", x = f))
    idH <- which(grepl(pattern = "#'", x = f))
    idF <- which(grepl(pattern = "<- function", x = f))
    for (id in idE) {
      end <- min(idF[idF > id])
      start <- max(idH[idH < end]) + 1
      funName <- paste0(f[start:end], collapse = "")
      funName <- strsplit(x = funName, split = "<-")[[1]][1] |>
        gsub(pattern = " ", replacement = "")
      details <- details |>
        dplyr::union_all(dplyr::tibble(
          file_name = x[k], function_name = funName, line_start = end
        ))
    }
  }
  return(details)
}
createNewAlias <- function(newAlias, details) {
  aliasFile <- "R/alias.R"
  if (file.exists(aliasFile)) {
    unlink(aliasFile)
  }
  file.create(aliasFile)
  writeLines(
    text = c(
      "# File created using CDMUtilities::useAlias", ""
    ),
    con = aliasFile
  )
  for (k in seq_len(nrow(newAlias))) {
    newCase
    doc <- documentation(newName, oldName, newArguments, oldArguments, values)
    writeLines(text = c("", doc), con = aliasFile)
  }
}
documentation <- function(newName, oldName, newArguments, oldArguments, values) {
  x <- c(glue::glue("#' @rdname {oldName}"), "#' @export")
  if (length(newArguments) <= 1) {
    x <- c(x, paste0(newName, " <- function(", newArguments, values, ") {"))
  } else {
    for (k in seq_along(newArguments)) {
      spaces <- paste0(rep(" ", nchar(newName) + 13), collapse = "")
      if (k == 1) {
        x <- c(x, glue::glue("{newName} <- function({newArguments[1]}{values[1]},"))
      } else {
        xk <- glue::glue("{spaces}{newArguments[k]}{values[k]}")
        if (k == length(newArguments)) {
          xk <- paste0(xk, ") {")
        } else {
          xk <- paste0(xk, ",")
        }
        x <- c(x, xk)
      }
    }
  }
  if (length(newArguments) == 0) {
    x <- c(x, glue::glue("  {oldName}()"))
  } else if (length(newArguments) <= 1) {
    x <- c(x, glue::glue("  {oldName}({oldArguments} = {newArguments})"))
  } else {
    x <- c(x, glue::glue("  {oldName}("))
    for (k in seq_along(newArguments)) {
      xk <- glue::glue("    {oldArguments[k]} = {newArguments[k]}")
      if (k != length(newArguments)) {
        xk <- paste0(xk, ",")
      }
      x <- c(x, xk)
    }
    x <- c(x, "  )")
  }
  x <- c(x, "}")
}
