# function that builds and aggregates ts
generateTableShell <- function(tableShell, executionSettings, keepDat) {

  # user print specifying job info
  tableShell$printJobDetails()

  # insert time windows
  tableShell$insertTimeWindows()

  # make sql file for table shell run
  sql <- tableShell$buildTableShellSql()

  # Execute them on dbms
  DatabaseConnector::executeSql(connection = executionSettings$getConnection(), sql = sql)

  # keep dat Temp table for future use
  if (keepDat) {
    # TODO create ctas to save temp table
  }

}

# function that shows sql used to make table shell
checkTableShellSql <- function(tableShell, executionSettings){

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
