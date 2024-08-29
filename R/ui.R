# function that builds and aggregates ts
generateTableShell <- function(tableShell, executionSettings, buildOptions = NULL) {

  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  # Step 0: user print specifying job info
  tableShell$printJobDetails()

  # Step 1: insert time windows
  tableShell$insertTimeWindows(executionSettings, buildOptions)

  # Step 2: make sql file for table shell run
  sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)

  # Step 3: Execute them on dbms
  cli::cat_bullet(
    glue::glue_col("{yellow Executing shell sql}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  DatabaseConnector::executeSql(connection = executionSettings$getConnection(), sql = sql)

  # Step 4: Transform results (continuous => categorical; categorical => continuous)
  # tableShell$categorizeItems(executionSettings)
  # tableShell$scoreItems(executionSettings)

  # Step 5: Aggregate Results Table
  # step 5a: aggregate categorical results first
  categoricalResultsRaw <- tableShell$aggregateTableShell(executionSettings, type = "categorical")
  #categoricalResultsFormatted <- .formatCategoricalResult(categoricalResultsRaw)

  #step 5b: aggregate continuous results second
  continuousResultsRaw <- tableShell$aggregateContinuous(executionSettings)
  #continuousResultsFormatted <- .formatCategoricalResult(continuousResultsRaw)

  # keep dat Temp table for future use
  if (buildOptions$keepDatTable) {
    # TODO create ctas to save temp table
  }

  tableShellResults <- list(
    'categorical' = categoricalResultsFormatted,
    'continuous' = continuousResultsFormatted
  )

  return(tableShellResults)
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
