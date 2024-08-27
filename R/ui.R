# function that builds and aggregates ts
generateTableShell <- function(tableShell, executionSettings, buildOptions = NULL) {

  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  # user print specifying job info
  tableShell$printJobDetails()

  # insert time windows
  tableShell$insertTimeWindows(executionSettings, buildOptions)

  # make sql file for table shell run
  sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)

  # Execute them on dbms
  DatabaseConnector::executeSql(connection = executionSettings$getConnection(), sql = sql)

  # keep dat Temp table for future use
  if (buildOptions$keepDatTable) {
    # TODO create ctas to save temp table
  }

}




# function that shows sql used to make table shell
reviewTableShellSql <- function(tableShell, executionSettings, buildOptions = NULL){

  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  # make sql file for table shell run
  sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)

  cli::cat_bullet(
    glue::glue("Opening Table Shell Query Sql in Monaco widget"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  mnc <- monaco::monaco(
    contents = sql,
    language = "sql",
    theme = "vs"
  )

  return(mnc)
}


# function that creates text summary of how table shells were created
buildTableShellReport <- function(tableShell) {

}

# function to save table shell results
saveTableShell <- function(result, outputPath) {

}

# creates reactable output of the tables shells
previewTableShell <- function(results) {

}
