# find all functions
# detect the ones that need reexport
# create the new functions in the same file

#' Function to reexport other functions using camelCase or snake_case.
#'
#' @param originalCase Casing of functions to be reexported.
#' @param newCase Casing of the new created functions.
#'
#' @export
#'
useAlias <- function(originalCase = c("camelCase", "snake_case"),
                     newCase = c("camelCase", "snake_case")) {
  # initial checks
  checkInput(originalCase = originalCase, newCase = newCase)

  # check that you can load current
  call <- rlang::env_parent()
  tryCatch(
    {
      devtools::load_all()
      devtools::document()
    },
    error = function(cond) {
      cli::cli_abort(
        "package could not be loaded, please ensure you can run
        `devtools::load_all() before trying to create the alias.`",
        call = call
      )
    }
  )

  # functions to create alias
  newAlias <- aliasToCreate(originalCase = originalCase, newCase = newCase)

  if (nrow(newAlias) > 0) {
    # inform alias
    informAlias(newAlias)
    askConfirmation()
    # create new alias
    createNewAlias(newAlias)
    devtools::document()
  } else {
    cli::cli_inform("No alias to create detected")
  }

  invisible(TRUE)
}

aliasToCreate <- function(originalCase, newCase) {
  functions <- dplyr::tibble(original_name = readLines("NAMESPACE")) |>
    dplyr::filter(substr(.data$original_name, 1, 7) == "export(") |>
    dplyr::mutate(original_name = substr(
      .data$original_name, 8, nchar(.data$original_name) - 1
    )) |>
    dplyr::filter(!grepl("\\.", .data$original_name))
  functions |>
    dplyr::mutate(
      snake_case = toSnakeCase(.data$original_name),
      camelCase = toCamelCase(.data$original_name)
    ) |>
    dplyr::filter(.data$snake_case != .data$camelCase) |>
    dplyr::mutate(original_case = dplyr::if_else(
      .data$original_name == .data$snake_case, "snake_case", "camelCase"
    )) |>
    dplyr::filter(.data$original_case %in% .env$originalCase) |>
    dplyr::left_join(
      tidyr::expand_grid(original_case = originalCase, new_case = newCase),
      by = "original_case",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(new_name = dplyr::if_else(
      .data$new_case == "snake_case", .data$snake_case, .data$camelCase
    )) |>
    dplyr::filter(!(.data$new_name %in% .env$functions$original_name)) |>
    dplyr::select("original_name", "original_case", "new_name", "new_case")
}
informAlias <- function(newAlias) {
  cli::cli_inform("A total of {nrow(newAlias)} new alias will be created:")
  for (k in seq_len(nrow(newAlias))) {
    cli::cli_inform(
      "+ {newAlias$original_name[k]} ({newAlias$original_case[k]}) -->
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
      cli::cli_inform("File alias.R will be (re)created with all the alias.")
      break
    } else if (x == "n") {
      cli::cli_abort("Files not created")
    }
  }
}
createNewAlias <- function(newAlias) {
  aliasFile <- "R/alias.R"
  initialText <- "# File created using CDMUtilities::useAlias"
  if (!file.exists(aliasFile)) {
    file.create(aliasFile)
    text <- c(initialText, "")
  } else {
    text <- readLines(aliasFile)
    if (x[1] != initialText) {
      cli::cli_abort(
        "alias.R was not generated automatically by `CDMUtilities::useAlias`,
        please delete or rename this file."
      )
    }
  }
  newAlias <- newAlias %>% dplyr::select(-"original_case")
  for (k in seq_len(nrow(newAlias))) {
    newCase <- newAlias$new_case[k]
    newName <- newAlias$new_name[k]
    originalName <- newAlias$original_name[k]
    arguments <- eval(parse(text = glue::glue("formals({originalName})")))
    originalArguments <- names(arguments)
    if (is.null(originalArguments)) {
      newArguments <- originalArguments
    } else {
      if (newCase == "snake_case") {
        newArguments <- toSnakeCase(originalArguments)
      } else {
        newArguments <- toCamelCase(originalArguments)
      }
      newArguments[originalArguments == "..."] <- "..."
    }
    values <- getValues(arguments)
    doc <- documentation(
      newName = newName, originalName = originalName,
      newArguments = newArguments, originalArguments = originalArguments,
      values = values
    )
    text <- c(text, "", doc)
  }
  writeLines(text = text, con = aliasFile)
}
documentation <- function(newName, originalName, newArguments, originalArguments, values) {
  x <- c(glue::glue("#' @rdname {originalName}"), "#' @export")
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
    x <- c(x, glue::glue("  {originalName}()"))
  } else if (length(newArguments) <= 1) {
    x <- c(x, glue::glue("  {originalName}({originalArguments} = {newArguments})"))
  } else {
    x <- c(x, glue::glue("  {originalName}("))
    for (k in seq_along(newArguments)) {
      xk <- glue::glue("    {originalArguments[k]} = {newArguments[k]}")
      if (k != length(newArguments)) {
        xk <- paste0(xk, ",")
      }
      x <- c(x, xk)
    }
    x <- c(x, "  )")
  }
  x <- c(x, "}")
}
getValues <- function(arguments) {
  values <- list()
  for (nm in names(arguments)) {
    x <- arguments[[nm]]
    if (missing(x) | nm == "...") {
      values[[nm]] <- ""
    } else {
      if ("call" == class(x) | is.null(x)) {
        values[[nm]] <- paste0(" = ", capture.output(print(x)))
      } else {
        if (is.character(x)) {
          x <- paste0("\"", x, "\"")
        }
        values[[nm]] <- paste0(" = ", capture.output(cat(x)))
      }
    }
  }
  return(values)
}
