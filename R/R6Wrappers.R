#' @title
#' Create an empty TableShell object and set its title
#'
#' @param title The title of the TableShell
#' @param sections A list of Section objects
#' @param targetCohorts A list of TargetCohort objects
#' @param executionSettings An ExecutionSettings object
#'
#' @return A TableShell object
#'
#' @export
createTableShell <- function(name,
                            sections,
                            targetCohorts,
                            executionSettings) {
    tableShell <- TableShell$new(name = name,
                                 sections = sections,
                                 targetCohorts = targetCohorts,
                                 executionSettings = executionSettings)
    return(tableShell)
}

#' @title
#' Parse target cohorts from a data frame
#'
#' @param df The data frame containing the information for the target cohorts (id and name)
#'
#' @return A list of TargetCohort objects
#'
#' @export
parseTargetCohortsFromDf <- function(df) {
    targetCohorts <- purrr::pmap(df, function(id, name) {
        createTargetCohort(id, name)
    })
    return(targetCohorts)
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
  targetCohort <- TargetCohort$new(id, name)
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


timeInterval <- function(lb, rb) {
  ti <- TimeInterval$new(lb = lb, rb = rb)
  return(ti)
}

createTimeWindows <- function(...) {
  windows <- list(...)
  tw <- TimeWindow$new(windows = windows)
}

#' @title
#' Create a Section object and set its attributes
#' @param title The title of the Section
#' @param ordinal The ordinal of the Section
#' @param lineItems A list of LineItem objects
#'
#' @return A Section object
#'
#' @export
createSection <- function(name, ordinal, lineItems) {
  section <- Section$new(name, ordinal, lineItems)
  return(section)
}

#' @title
#' Create a concept set line item and set its attributes
#'
#' @param name (OPTIONAL) The name of the line item (if not provided, the name will be set to the Capr concept set name)
#' @param ordinal The ordinal of the line item within a section
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param conceptSet The Capr concept set object
#' @param domain The domain of the concept set (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param sourceConceptSet (OPTIONAL) A Capr concept set of source concept IDs to use to limit the concept set
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A ConceptSetDefinition object
#'
#' @export
createConceptSetLineItem <- function(name,
                                     ordinal,
                                     statistic,
                                     conceptSet,
                                     domain,
                                     sourceConceptSet = NULL,
                                     typeConceptIds = c(),
                                     visitOccurrenceConceptIds = c()) {
  csDefinition <- ConceptSetDefinition$new(name,
                                           ordinal,
                                           statistic,
                                           conceptSet,
                                           domain,
                                           sourceConceptSet = sourceConceptSet,
                                           typeConceptIds = typeConceptIds,
                                           visitOccurrenceConceptIds = visitOccurrenceConceptIds)
  return(csDefinition)
}

#' @title
#' Create a batch of concept set line items from a list of Capr concept sets.
#'
#' @description
#' The name of each line item will be set to the name of its Capr concept set, and the ordinal will be set to the index of the Capr concept set in the list. All line items will use the same statistic, domain, type concepts, and visit concepts. It is not possible to specify source concept IDs.
#'
#' @param statistic The Statistic object to be used to evaluate the line items
#' @param conceptSets A list of concept set Capr objects
#' @param domain The domain of the concept sets (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A list of ConceptSetDefinition objects
#'
#' @export
createConceptSetLineItemBatch <- function(statistic,
                                          conceptSets,
                                          domain,
                                          typeConceptIds = c(),
                                          visitOccurrenceConceptIds = c()) {
  checkmate::assert_list(x = conceptSets, types = c("ConceptSet"), null.ok = FALSE, min.len = 1)

  csDefs <- list()
  n <- 0
  for (cs in conceptSets) {
    n <- n + 1
    csDefinition <- ConceptSetDefinition$new(name = cs@Name,
                                             ordinal = n,
                                             statistic = statistic,
                                             conceptSet = cs,
                                             domain = domain,
                                             typeConceptIds = typeConceptIds,
                                             visitOccurrenceConceptIds = visitOccurrenceConceptIds)
    csDefs <- append(csDefs, list(csDefinition))
  }
  return(csDefs)
}


# addGenderItem <- function(genderConceptIds = c(),
#                           inputType) {
#   gender <- GenderDefinition$new(inputType = "Explicit", genderConceptIds = genderConceptIds)

# }
