#' @title
#' Create an empty TableShell object and set its title
#'
#' @param title The title of the TableShell
#'
#' @return A TableShell object
#'
#' @export
createTableShell <- function(title) {
    tableShell <- TableShell$new()
    tableShell$setTitle(title)
    return(tableShell)
}

#' @title
#' Add TargetCohorts to a TableShell object
#'
#' @param tableShell The TableShell object
#' @param targetCohorts A list of TargetCohort objects
#'
#' @return A TableShell object
#'
#' @export
addTargetCohorts <- function(tableShell, targetCohorts) {
    tableShell$setTargetCohorts(targetCohorts)
    invisible(tableShell)
}

#' @title
#' Add target cohorts from a data frame
#'
#' @param tableShell The table shell object to which the target cohorts will be added.
#' @param df The data frame containing the information for the target cohorts.
#'
#' @return The updated table shell object with the target cohorts added.
#'
#' @export
addTargetCohortsFromDf <- function(tableShell, df) {
    targetCohorts <- purrr::pmap(df, function(id, name) {
        createTargetCohort(id, name)
    })
    tableShell$setTargetCohorts(targetCohorts)
    invisible(tableShell)
}

#' @title
#' Add target cohorts from a CSV file
#'
#' @param tableShell The table shell object to which the target cohorts will be added.
#' @param file The path to the CSV file containing the target cohorts data.
#'
#' @return The modified table shell object.
#'
#' @export
addTargetCohortsFromCsv <- function(tableShell, file) {
    df <- read.csv(file)
    addTargetCohortsFromDf(tableShell, df)
    invisible(tableShell)
}

#' @title
#' Add one or more Sections to a TableShell object
#'
#' @param tableShell The TableShell object
#' @param section A Section object
#' @param ... Additional Section objects to be added
#'
#' @return A TableShell object
#'
#' @export
addSections <- function(tableShell, section, ...) {
    if (!inherits(section, "Section")) {
        stop("section must be a Section object")
    }

    additionalSections <- list(...)
    if (!all(sapply(additionalSections, function(x) inherits(x, "Section")))) {
        stop("All additional arguments must be Section objects")
    }

    allSections <- c(list(section), additionalSections)
    tableShell$setSections(allSections)
    invisible(tableShell)
}

#' @title
#' Set the ExecutionSettings of a TableShell object
#'
#' @param tableShell The TableShell object
#' @param executionSettings The ExecutionSettings object
#'
#' @return A TableShell object
#'
#' @export
setExecutionSettings <- function(tableShell, executionSettings) {
    tableShell$setExecutionSettings(executionSettings)
    return(tableShell)
}

#' @title
#' Create a TargetCohort object and set its attributes
#'
#' @param id The ID of the TargetCohort
#' @param name The name of the TargetCohort
#'
#' @return A TargetCohort object
#'
#' @export
createTargetCohort <- function(id, name) {
  targetCohort <- TargetCohort$new()
  targetCohort$setId(id)
  targetCohort$setName(name)
  return(targetCohort)
}

#' @title
#' Create an ExecutionSettings object and set its attributes
#'
#' @param connectionDetails A DatabaseConnector connectionDetails object (optional if connection is specified)
#' @param connection A DatabaseConnector connection object (optional if connectionDetails is specified)
#' @param cdmDatabaseSchema The schema of the OMOP CDM database
#' @param workDatabaseSchema The schema to which results will be written
#' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
#' @param targetCohortTable The name of the table where the target cohort(s) are stored
#' @param cdmSourceName A human-readable name for the OMOP CDM source
#' @param numThreads (OPTIONAL) The number of threads to use for parallel processing
#'
#' @return An ExecutionSettings object
#' @export
createExecutionSettings <- function(connectionDetails,
                                    connection,
                                    cdmDatabaseSchema,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    targetCohortTable,
                                    cdmSourceName,
                                    numThreads) {
  executionSettings <- ExecutionSettings$new(connectionDetails = connectionDetails,
                                             connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             workDatabaseSchema = workDatabaseSchema,
                                             tempEmulationSchema = tempEmulationSchema,
                                             targetCohortTable = targetCohortTable,
                                             cdmSourceName = cdmSourceName,
                                             numThreads = numThreads)
  return(executionSettings)
}

#' @title
#' Create a Section object and set its title and ordinal
#' @param title The title of the Section
#' @param ordinal The ordinal of the Section
#'
#' @return A Section object
#'
#' @export
createSection <- function(title, ordinal) {
  section <- Section$new()
  section$setTitle(title)
  section$setOrdinal(ordinal)
  return(section)
}

#' @title
#' Add one or more Line Items to a Section object
#'
#' @param section The Section object
#' @param lineItem A LineItem object
#' @param ... Additional LineItem objects to be added
#'
#' @return A Section object
#'
#' @export
addLineItems <- function(section, lineItem, ...) {
  if (!inherits(lineItem, "LineItem")) {
    stop("lineItem must be a LineItem object")
  }

  additionalLineItems <- list(...)
  if (!all(sapply(additionalLineItems, function(x) inherits(x, "LineItem")))) {
    stop("All additional arguments must be LineItem objects")
  }

  allLineItems <- c(list(lineItem), additionalLineItems)
  section$setLineItems(allLineItems)
  invisible(section)
}

#' @title
#' Create a LineItem object and set its attributes
#'
#' @param ordinal The ordinal of the LineItem
#' @param label The label of the LineItem
#' @param showMissing Whether to show missing values in the LineItem
#' @param statisticType The statistic type of the LineItem
#' @param limit The limit of the LineItem
#'
#' @return A LineItem object
#'
#' @export
createLineItem <- function(label, ordinal) {
  lineItem <- LineItem$new()
  lineItem$setOrdinal(ordinal)
  lineItem$setLabel(label)
  #lineItem$setShowMissing(showMissing)
  #lineItem$setStatisticType(statisticType)
  #lineItem$setLimit(limit)
  return(lineItem)
}
