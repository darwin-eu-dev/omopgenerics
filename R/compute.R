# Copyright 2023 DARWIN EU (C)
#
# This file is part of omopgenerics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Store results in a table.
#'
#' @param x Table in the cdm.
#' @param name Name to store the table with.
#' @param temporary Whether to store table temporarily (TRUE) or permanently
#' (FALSE).
#' @param overwrite Whether to overwrite previously existing table with name
#' same.
#' @param ... For compatibility (not used).
#'
#' @return Reference to a table in the cdm
#'
#' @export
#' @importFrom dplyr compute
compute.cdm_table <- function(x,
                              name = NULL,
                              temporary = NULL,
                              overwrite = TRUE,
                              ...) {
  if (is.character(name) & is.null(temporary)) temporary <- FALSE
  if (is.null(name)) name <- uniqueTableName()
  if (is.null(temporary)) temporary <- TRUE
  src <- tableSource(x)
  cl <- class(src)[class(src) != "cdm_source"]
  cx <- class(x)

  logFile <- getOption("omopgenerics.log_sql_path")
  if(!is.null(logFile) && inherits(cdmSource(x), "db_cdm")){
    # log sql if option set
    # must have specified a directory that exists
    if (dir.exists(logFile)) {
    cli::cli_inform("SQL query saved to {logFile}")
    writeLines(utils::capture.output(dplyr::show_query(x)),
               here::here(logFile,
                          paste0("logged_query_ran_on_",
                          format(Sys.time(),
                                 format = "%Y_%m_%d_at_%H_%M_%S"),
                          ".sql"
                          )))
    } else {
      cli::cli_inform("SQL query not saved as '{logFile}' not an existing directory")
    }
    }

  res <- x |>
    keepClass() |>
    addClass(cl) |>
    dplyr::compute(name = name, temporary = temporary, overwrite = overwrite)
  if (temporary) name <- NA_character_
  res <- res |>
    removeClass(cl) |>
    newCdmTable(src = src, name = name) |>
    restoreClass(cx) |>
    restoreAttributes(keepAttributes(x, cx))
  return(res)
}

#' @export
compute.local_cdm <- function(x, ...) {
  return(x)
}

#' @export
compute.cohort_table <- function(x,
                                 name = NULL,
                                 temporary = NULL,
                                 uncohort = FALSE,
                                 ...) {
  # initial checks
  if (is.character(name) & is.null(temporary)) temporary <- FALSE
  if (is.null(name)) name <- uniqueTableName()
  if (is.null(temporary)) temporary <- TRUE
  assertLogical(uncohort, length = 1)
  assertLogical(temporary, length = 1)
  assertCharacter(name, length = 1, na = temporary)

  # warn if uncohort and not explicitly asked for it
  oldName <- tableName(x)
  if (temporary == TRUE && uncohort == FALSE) {
    if (!missing(uncohort)) {
      cli::cli_inform(c("!" = "`uncohort` = {.pkg FALSE} not compatible with
                        `temporary` = {.pkg TRUE} -> uncohort was changed to {.pkg TRUE}"))
    }
    uncohort <- TRUE
    cli::cli_warn(c("!" = "The cohort {.strong {oldName}} is computed to
                    temporary table and it will lose the class `cohort_table`"))
  }

  # inform new tables if not asked explicitly to create
  if (temporary == TRUE && oldName != name && uncohort == FALSE && !missing(uncohort)) {
    newNames <- paste0(name, c("_set", "_attrition", "_codelist"))
    cli::cli_inform(c(
      "!" = "copying cohort from {.strong {oldName}} to {.strong {name}},
      attributes will be copied to: {.pkg {newNames}}.",
      "i" = "You can switch off this message setting the option: `uncohort` = FALSE"
    ))
  }

  if (!uncohort) {
    set <- settings(x)
    atr <- attrition(x)
    cod <- attr(x, "cohort_codelist") |>
      dplyr::collect()
  }

  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  attr(x, "cohort_codelist") <- NULL

  x <- x |>
    removeClass("cohort_table") |>
    dplyr::compute(name = name, temporary = temporary, ...)

  if (!uncohort) {
    x <- x|>
      omopgenerics::newCohortTable(
        cohortSetRef = set,
        cohortAttritionRef = atr,
        cohortCodelistRef = cod,
        .softValidation = TRUE)
  }

  return(x)
}

#' Create a unique table name
#'
#' @param prefix Prefix for the table names.
#'
#' @return A string that can be used as a dbplyr temp table name
#' @export
#'
#' @examples
#' library(omopgenerics)
#' uniqueTableName()
uniqueTableName <- function(prefix = "") {
  assertCharacter(x = prefix, length = 1)
  i <- getOption("og_table_name", 0) + 1
  options(og_table_name = i)
  value <- paste0(sprintf("og_%03i", i), "_", round(as.numeric(Sys.time())))
  paste0(prefix, value)
}

#' Create a temporary prefix for tables, that contains a unique prefix that
#' starts with tmp.
#'
#' @return A temporary prefix.
#' @export
#'
#' @examples
#' library(omopgenerics)
#' tmpPrefix()
tmpPrefix <- function() {
  i <- getOption("tmp_prefix_number", 0) + 1
  options(tmp_prefix_number = i)
  sprintf("tmp_%03i_", i)
}
