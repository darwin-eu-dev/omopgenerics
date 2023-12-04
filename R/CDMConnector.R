
# define a cdm connection class
#' Create a source for a OMOP Common Data Model reference in a database.
#'
#' @param con Connection to a database.
#' @param writeSchema Schema, catalog and prefix of the database that allow us
#' to write permanent tables.
#'
dbSource <- function(con, writeSchema) {
  if (!inherits(con, "DBIConnection")) {
    cli::cli_abort(paste0(
      cli::style_bold("con"), " must be inherited from a DBI connection."
    ))
  }
  assertCharacter(writeSchema, minNumCharacter = 1, named = TRUE)
  assertChoice(x = names(writeSchema), choices = c("schema", "catalog", "prefix"))
  source <- list(con = con, write_schema = writeSchema)
  addClass(source) <- "db_source"
  source <- cdmSource(source, sourceType = dbms(con))
  return(source)
}

insertTable.db_source <- function() {

}
computeTable.db_source <- function() {

}
dropTable.db_source <- function() {

}

