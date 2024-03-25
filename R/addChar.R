set_order_id <- function(clinChar) {
  order_id <- as.integer(length(clinChar@extractSettings) + 1)
  return(order_id)
}

#' Add an age characteristic
#' @description
#' This function adds an age characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categoization summary is done
#' @return adds a ageChar object into the clinChar extractSettings slot
#' @export
addAgeChar <- function(clinChar, categorize = NULL) {
  char <- new("ageChar", orderId = set_order_id(clinChar))
  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}

#' Add a gender characteristic
#' @description
#' This function adds a gender characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @return adds a demoConceptChar object into the clinChar extractSettings slot
#' @export
addGenderChar <- function(clinChar) {
  char <- new("demoConceptChar", domain = "gender", orderId = set_order_id(clinChar))
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}

#' Add a race characteristic
#' @description
#' This function adds a race characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @return adds a demoConceptChar object into the clinChar extractSettings slot
#' @export
addRaceChar <- function(clinChar) {
  char <- new("demoConceptChar", domain = "race", orderId = set_order_id(clinChar))
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}

#' Add a ethnicity characteristic
#' @description
#' This function adds a ethnicity characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @return adds a demoConceptChar object into the clinChar extractSettings slot
#' @export
addEthnicityChar <- function(clinChar) {
  char <- new("demoConceptChar", domain = "ethnicity", orderId = set_order_id(clinChar))
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}

#' Add a year characteristic
#' @description
#' This function adds a year characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @return adds a yearChar object into the clinChar extractSettings slot
#' @export
addYearChar <- function(clinChar, categorize = NULL) {
  char <- new("yearChar", domain = "year", orderId = set_order_id(clinChar))
  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}

#' Add a location characteristic
#' @description
#' This function adds alocation characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @return adds a locationChar object into the clinChar extractSettings slot
#' @export
addLocationChar <- function(clinChar) {
  char <- new("locationChar", orderId = set_order_id(clinChar))
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}


#' Add a lab characteristic
#' @description
#' This function adds a lab characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param labIds OMOP concept ids for labs
#' @param unitIds OMOP concept ids for units
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categoization summary is done
#' @return adds a labChar object into the clinChar extractSettings slot
#' @export
addLabChar <- function(clinChar, labIds, unitIds, timeWindows, limit = c("last", "first", "all"),
                       categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_lab <- glue::glue("{tempSchema}.lab_domain_tmp")
  } else {
    tbl_lab <- "#lab_domain"
  }

  labChar <- new("labChar", orderId = set_order_id(clinChar))
  labChar@labIds <- as.integer(labIds)
  labChar@unitIds <- as.integer(unitIds)
  labChar@time <- timeWindows
  labChar@limit <- limit
  labChar@tempTables <- list(
    'lab' = tbl_lab
  )

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, labChar)
  return(clinChar)
}

#' Add a visit occurrence presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a visit occurence.
#' A presence characteristic summarizes whether a person had the event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @return adds a presenceChar object of visit_occurrence into the clinChar extractSettings slot
#' @export
addVisitPresence <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all")) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.condition_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.condition_domain_tmp")
  } else {
    #tbl_codeset <- "#visit_codeset"
    tbl_domain <- "#visit_domain"
  }

  visitChar <- new("presenceChar", domain = "visit_occurrence", orderId = set_order_id(clinChar))
  visitChar@conceptSets <- conceptSets
  visitChar@time <- timeWindows
  visitChar@limit <- limit
  visitChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' = c()
  )

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a condition occurrence presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a condition occurence.
#' A presence characteristic summarizes whether a person had the event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @return adds a presenceChar object of condition_occurrence into the clinChar extractSettings slot
#' @export
addConditionPresence <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all")) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.condition_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.condition_domain_tmp")
  } else {
    #tbl_codeset <- "#condition_codeset"
    tbl_domain <- "#condition_domain"
  }

  conditionChar <- new("presenceChar", domain = "condition_occurrence", orderId = set_order_id(clinChar))
  conditionChar@conceptSets <- conceptSets
  conditionChar@time <- timeWindows
  conditionChar@limit <- limit
  conditionChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' = c()
  )

  clinChar@extractSettings <- append(clinChar@extractSettings, conditionChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a drug exposure presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a drug exposure.
#' A presence characteristic summarizes whether a person had the event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @return adds a presenceChar object of drug exposure into the clinChar extractSettings slot
#' @export
addDrugPresence <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all")) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.drug_domain_tmp")
  } else {
    tbl_domain <- "#drug_domain"
  }

  drugChar <- new("presenceChar", domain = "drug_exposure", orderId = set_order_id(clinChar))
  drugChar@conceptSets <- conceptSets
  drugChar@limit <- limit
  drugChar@time <- timeWindows
  drugChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )
  clinChar@extractSettings <- append(clinChar@extractSettings, drugChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a observation presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a observation.
#' A presence characteristic summarizes whether a person had the event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @return adds a presenceChar object of observation into the clinChar extractSettings slot
#' @export
addObservationPresence <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all")) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.obs_domain_tmp")
  } else {
    tbl_domain <- "#obs_domain"
  }

  obsChar <- new("presenceChar", domain = "observation", orderId = set_order_id(clinChar))
  obsChar@conceptSets <- conceptSets
  obsChar@limit <- limit
  obsChar@time <- timeWindows
  obsChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )
  clinChar@extractSettings <- append(clinChar@extractSettings, obsChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a Procedure presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a Procedure.
#' A presence characteristic summarizes whether a person had the event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @return adds a presenceChar object of Procedure into the clinChar extractSettings slot
#' @export
addProcedurePresence <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all")) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.proc_domain_tmp")
  } else {
    tbl_domain <- "#proc_domain"
  }

  procChar <- new("presenceChar", domain = "procedure_occurrence", orderId = set_order_id(clinChar))
  procChar@conceptSets <- conceptSets
  procChar@limit <- limit
  procChar@time <- timeWindows
  procChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )
  clinChar@extractSettings <- append(clinChar@extractSettings, procChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a drug exposure count characteristic
#' @description
#' This function adds a count characteristic to the clinChar object for a drug exposure.
#' A count characteristics enumerates the number of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categoization summary is done
#' @return adds a countChar object of drug exposure into the clinChar extractSettings slot
#' @export
addDrugCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = c(32810, 32869)) {

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.drug_count_tmp")
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
  } else {
    #tbl_codeset <- "#drug_codeset"
    tbl_count <- "#drug_count"
  }

  drugChar <- new("countChar", domain = "drug_exposure", orderId = set_order_id(clinChar))
  drugChar@time <- timeWindows
  drugChar@conceptSets <- conceptSets
  drugChar@conceptType <- as.integer(conceptType)
  drugChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, drugChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a visit count characteristic
#' @description
#' This function adds a count characteristic to the clinChar object for a visit.
#' A count characteristics enumerates the number of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a countChar object of visit into the clinChar extractSettings slot
#' @export
addVisitCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = c(32810, 32869)) {

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.visit_count_tmp")
    #tbl_codeset <- glue::glue("{tempSchema}.visit_codeset_tmp")
  } else {
    #tbl_codeset <- "#visit_codeset"
    tbl_count <- "#visit_count"
  }

  visitChar <- new("countChar", domain = "visit_occurrence", orderId = set_order_id(clinChar))
  visitChar@conceptSets <- conceptSets
  visitChar@conceptType <- as.integer(conceptType)
  visitChar@time <- timeWindows
  visitChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a drug exposure cost characteristic
#' @description
#' This function adds a cost characteristic to the clinChar object for a drug exposure.
#' A cost characteristics summarizes the total cost of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param costType the column in the cost table to summarize
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categoization summary is done
#' @return adds a costChar object of drug exposure into the clinChar extractSettings slot
#' @export
addDrugCost <- function(clinChar, timeWindows, conceptSets = NULL,
                        costType = "amount_allowed", categorize = NULL,
                        conceptType = c(32810, 32869)) {

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.drug_count_tmp")
  } else {
    tbl_cost <- "#drug_cost"
  }

  drugChar <- new("costChar", domain = "drug_exposure", orderId = set_order_id(clinChar))
  drugChar@costType <- costType
  drugChar@conceptSets <- conceptSets
  drugChar@conceptType <- as.integer(conceptType)
  drugChar@time <- timeWindows
  drugChar@tempTables <- list(
    'cost' = tbl_cost,
    'codeset' = c()
  )
  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, drugChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


addVisitCost <- function(clinChar, timeWindows,
                         conceptSets = NULL,
                         costType = "amount_allowed", categorize = NULL,
                         conceptType = c(32810, 32869)) {

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.visit_count_tmp")
  } else {
    tbl_cost <- "#visit_cost"
  }

  visitChar <- new("costChar", domain = "visit_occurrence", orderId = set_order_id(clinChar))
  visitChar@costType <- costType
  visitChar@conceptSets <- conceptSets
  visitChar@conceptType <- as.integer(conceptType)
  visitChar@time <- timeWindows
  visitChar@tempTables <- list(
    'cost' = tbl_cost,
    'codeset' = c()
  )
  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


addTimeInCohort <- function(clinChar, categorize = NULL) {

  cohortChar <- new("timeInChar", domain = "cohort", orderId = set_order_id(clinChar))

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, cohortChar)
  return(clinChar)
}


addTimeInInpatient <- function(clinChar, categorize = NULL) {

  cohortChar <- new("timeInChar", domain = "inpatient", orderId = set_order_id(clinChar))

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, cohortChar)
  return(clinChar)
}

#' Add a drug exposure timeTo characteristic
#' @description
#' This function adds a timeTo characteristic to the clinChar object for a drug exposure.
#' A timeTo characteristic summarizes the time to an event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categoization summary is done
#' @return adds a timeToChar object of drug exposure into the clinChar extractSettings slot
#' @export
addTimeToDrug <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                          categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_duration <- glue::glue("{tempSchema}.drug_duration_tmp")
  } else {
    #tbl_codeset <- "#drug_codeset"
    tbl_duration <- "#drug_duration"
  }

  drugChar <- new("timeToChar", domain = "drug_exposure", orderId = set_order_id(clinChar))
  drugChar@conceptSets <- conceptSets
  drugChar@time <- timeWindows
  drugChar@limit <- limit
  drugChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    drugChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, drugChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a condition timeTo characteristic
#' @description
#' This function adds a timeTo characteristic to the clinChar object for a condition.
#' A timeTo characteristic summarizes the time to an event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categoization summary is done
#' @return adds a timeToChar object of condition into the clinChar extractSettings slot
#' @export
addTimeToCondition <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                          categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_duration <- glue::glue("{tempSchema}.cond_duration_tmp")
  } else {
    #tbl_codeset <- "#drug_codeset"
    tbl_duration <- "#cond_duration"
  }

  condChar <- new("timeToChar", domain = "condition_occurrence", orderId = set_order_id(clinChar))
  condChar@conceptSets <- conceptSets
  condChar@time <- timeWindows
  condChar@limit <- limit
  condChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    condChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, condChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a visit timeTo characteristic
#' @description
#' This function adds a timeTo characteristic to the clinChar object for a visit.
#' A timeTo characteristic summarizes the time to an event of
#' interest as described by a set of codes during a window of time.
#' We use an CIRCE concept set to specify the set of codes to use to determine the presence of an event
#' in a domain table.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a timeToChar object of visit into the clinChar extractSettings slot
#' @export
addTimeToVisit <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                          categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snwoflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.visit_codeset_tmp")
    tbl_duration <- glue::glue("{tempSchema}.visit_duration_tmp")
  } else {
    #tbl_codeset <- "#visit_codeset"
    tbl_duration <- "#visit_duration"
  }

  visitChar <- new("timeToChar", domain = "visit_occurrence", orderId = set_order_id(clinChar))
  visitChar@conceptSets <- conceptSets
  visitChar@time <- timeWindows
  visitChar@limit <- limit
  visitChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (class(categorize) != "breaksStrategy") {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}
