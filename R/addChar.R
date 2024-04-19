set_order_id <- function(clinChar) {
  order_id <- as.integer(length(clinChar@extractSettings) + 1)
  return(order_id)
}

# Demographics ----------------------------------

#' Add an age characteristic
#' @description
#' This function adds an age characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a ageChar object into the clinChar extractSettings slot
#' @export
addAgeChar <- function(clinChar, categorize = NULL) {
  char <- new("ageChar", orderId = set_order_id(clinChar))
  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
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
  char <- new("demoConceptChar", domain = "gender", orderId = set_order_id(clinChar),
              categoryId = 1002L)
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
  char <- new("demoConceptChar", domain = "race", orderId = set_order_id(clinChar),
              categoryId = 1003L)
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
  char <- new("demoConceptChar", domain = "ethnicity", orderId = set_order_id(clinChar),
              categoryId = 1004L)
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
    if (!methods::is(categorize, "breaksStrategy")) {
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
#' @param locationTable a locationTable object set using the the makeLocTable fn
#' @return adds a locationChar object into the clinChar extractSettings slot
#' @export
addLocationChar <- function(clinChar, locationTable) {
  char <- new("locationChar", orderId = set_order_id(clinChar),
              locationTable = locationTable)
  clinChar@extractSettings <- append(clinChar@extractSettings, char)
  return(clinChar)
}

# Visit Detail ------------------------

#' Add a specialty characteristic
#' @description
#' This function adds alocation characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param visitDetailTable a visitDetailTable object set using the the makeLocTable fn
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @return adds a visitDetailChar object into the clinChar extractSettings slot
#' @export
addSpecialtyChar <- function(clinChar, visitDetailTable, timeWindows) {

  specChar <- new("visitDetailChar", orderId = set_order_id(clinChar),
                  domain = "provider",
              visitDetailTable = visitDetailTable,
              categoryId = 9001L)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_detail <- glue::glue("{tempSchema}.vd_tmp")
  } else {
    tbl_detail <- "#vd"
  }

  specChar@time <- timeWindows
  specChar@tempTables <- list(
    'detail' = tbl_detail
  )

  clinChar@extractSettings <- append(clinChar@extractSettings, specChar)
  return(clinChar)
}

# Lab -------------------------

#' Add a lab characteristic
#' @description
#' This function adds a lab characteristic to the clinChar object.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param labUnitTable a labUnitTable object specifying the lab-unit combos to search
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param limit specify which values to use in the characteristic. The last variable will pull the last value in the
#' time window, the first variable will pull the first value in the time window and the
#' all vairable will pull all values in the time window
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a labChar object into the clinChar extractSettings slot
#' @export
addLabChar <- function(clinChar, labUnitTable, timeWindows, limit = c("last", "first", "all"),
                       categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_lab <- glue::glue("{tempSchema}.lab_domain_tmp")
  } else {
    tbl_lab <- "#lab_domain"
  }

  labChar <- new("labChar", orderId = set_order_id(clinChar),
                 categoryId = 5005L)
  labChar@labUnitTable <- labUnitTable
  labChar@time <- timeWindows
  labChar@limit <- limit
  labChar@tempTables <- list(
    'lab' = tbl_lab
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, labChar)
  return(clinChar)
}

# Presence -------------------------

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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of visit_occurrence into the clinChar extractSettings slot
#' @export
addVisitPresence <- function(clinChar, conceptSets, timeWindows,
                             limit = c("first", "last", "all"),
                             score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.condition_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.condition_domain_tmp")
  } else {
    #tbl_codeset <- "#visit_codeset"
    tbl_domain <- "#visit_domain"
  }

  visitChar <- new("presenceChar", domain = "visit_occurrence", orderId = set_order_id(clinChar),
                   categoryId = 8001L)
  visitChar@conceptSets <- conceptSets
  visitChar@time <- timeWindows
  visitChar@limit <- limit
  visitChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' = c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    visitChar@score <- score
  }


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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of condition_occurrence into the clinChar extractSettings slot
#' @export
addConditionPresence <- function(clinChar, conceptSets, timeWindows,
                                 limit = c("first", "last", "all"),
                                 score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.condition_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.condition_domain_tmp")
  } else {
    #tbl_codeset <- "#condition_codeset"
    tbl_domain <- "#condition_domain"
  }

  conditionChar <- new("presenceChar", domain = "condition_occurrence", orderId = set_order_id(clinChar),
                       categoryId = 2001L)
  conditionChar@conceptSets <- conceptSets
  conditionChar@time <- timeWindows
  conditionChar@limit <- limit
  conditionChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' = c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    conditionChar@score <- score
  }

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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of drug exposure into the clinChar extractSettings slot
#' @export
addDrugPresence <- function(clinChar, conceptSets, timeWindows,
                            limit = c("first", "last", "all"),
                            score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.drug_domain_tmp")
  } else {
    tbl_domain <- "#drug_domain"
  }

  drugChar <- new("presenceChar", domain = "drug_exposure", orderId = set_order_id(clinChar),
                  categoryId = 3001L)
  drugChar@conceptSets <- conceptSets
  drugChar@limit <- limit
  drugChar@time <- timeWindows
  drugChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    drugChar@score <- score
  }

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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of observation into the clinChar extractSettings slot
#' @export
addObservationPresence <- function(clinChar, conceptSets, timeWindows,
                                   limit = c("first", "last", "all"),
                                   score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.obs_domain_tmp")
  } else {
    tbl_domain <- "#obs_domain"
  }

  obsChar <- new("presenceChar", domain = "observation", orderId = set_order_id(clinChar),
                 categoryId = 6001L)
  obsChar@conceptSets <- conceptSets
  obsChar@limit <- limit
  obsChar@time <- timeWindows
  obsChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    obsChar@score <- score
  }

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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of Procedure into the clinChar extractSettings slot
#' @export
addProcedurePresence <- function(clinChar, conceptSets, timeWindows,
                                 limit = c("first", "last", "all"),
                                 score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.proc_domain_tmp")
  } else {
    tbl_domain <- "#proc_domain"
  }

  procChar <- new("presenceChar", domain = "procedure_occurrence", orderId = set_order_id(clinChar),
                  categoryId = 4001L)
  procChar@conceptSets <- conceptSets
  procChar@limit <- limit
  procChar@time <- timeWindows
  procChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    procChar@score <- score
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, procChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a measurement presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a measurement.
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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of measurement into the clinChar extractSettings slot
#' @export
addMeasurementPresence <- function(clinChar, conceptSets, timeWindows,
                                   limit = c("first", "last", "all"),
                                   score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_domain <- glue::glue("{tempSchema}.meas_domain_tmp")
  } else {
    tbl_domain <- "#meas_domain"
  }

  measChar <- new("presenceChar", domain = "measurement", orderId = set_order_id(clinChar),
                  categoryId = 5001L)
  measChar@conceptSets <- conceptSets
  measChar@limit <- limit
  measChar@time <- timeWindows
  measChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    measChar@score <- score
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, measChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a device presence characteristic
#' @description
#' This function adds a presence characteristic to the clinChar object for a device.
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
#' @param score describes how the categorical value should be converted to a continuous score.
#' This function takes a scoreStrategy object to describe the scoring ow it is left NULL.
#' If the parameter is NULL then no continuous summary is done
#' @return adds a presenceChar object of device into the clinChar extractSettings slot
#' @export
addDevicePresence <- function(clinChar, conceptSets, timeWindows,
                              limit = c("first", "last", "all"),
                              score = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_domain <- glue::glue("{tempSchema}.dev_domain_tmp")
  } else {
    tbl_domain <- "#dev_domain"
  }

  devChar <- new("presenceChar", domain = "device_exposure", orderId = set_order_id(clinChar),
                 categoryId = 7001L)
  devChar@conceptSets <- conceptSets
  devChar@limit <- limit
  devChar@time <- timeWindows
  devChar@tempTables <- list(
    'domain' = tbl_domain,
    'codeset' =  c()
  )

  if (!is.null(score)) {
    if (!methods::is(score, "scoreStrategy")) {
      stop("Score needs to be a scoreStrategy object")
    }
    devChar@score <- score
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, devChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

# Count ----------------------------------

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
#' If the parameter is NULL then no categorization summary is done
#' @param conceptType the drug type concept ids to use to limit the count
#' @return adds a countChar object of drug exposure into the clinChar extractSettings slot
#' @export
addDrugCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = c(32810, 32869)) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.drug_count_tmp")
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
  } else {
    #tbl_codeset <- "#drug_codeset"
    tbl_count <- "#drug_count"
  }

  drugChar <- new("countChar", domain = "drug_exposure", orderId = set_order_id(clinChar),
                  categoryId = 3002L)
  drugChar@time <- timeWindows
  drugChar@conceptSets <- conceptSets
  if (!is.null(conceptType)) {
    drugChar@conceptType <- as.integer(conceptType)
  }
  drugChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    drugChar@categorize <- categorize
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
#' @param conceptType the visit type concept ids to use to limit the count
#' @return adds a countChar object of visit into the clinChar extractSettings slot
#' @export
addVisitCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = NULL) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.visit_count_tmp")
    #tbl_codeset <- glue::glue("{tempSchema}.visit_codeset_tmp")
  } else {
    #tbl_codeset <- "#visit_codeset"
    tbl_count <- "#visit_count"
  }

  visitChar <- new("countChar", domain = "visit_occurrence", orderId = set_order_id(clinChar),
                   categoryId = 8002L)
  visitChar@conceptSets <- conceptSets
  if (!is.null(conceptType)) {
    visitChar@conceptType <- as.integer(conceptType)
  }
  visitChar@time <- timeWindows
  visitChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    visitChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a condition count characteristic
#' @description
#' This function adds a count characteristic to the clinChar object for a condition.
#' A count characteristics enumerates the number of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @param conceptType the condition type concept ids to use to limit the count
#' @return adds a countChar object of condition into the clinChar extractSettings slot
#' @export
addConditionCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = NULL) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.cond_count_tmp")
  } else {
    tbl_count <- "#cond_count"
  }

  condChar <- new("countChar", domain = "condition_occurrence", orderId = set_order_id(clinChar),
                  categoryId = 2002L)
  condChar@time <- timeWindows
  condChar@conceptSets <- conceptSets
  if (!is.null(conceptType)) {
    condChar@conceptType <- as.integer(conceptType)
  }
  condChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    condChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, condChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a procedure count characteristic
#' @description
#' This function adds a count characteristic to the clinChar object for a procedure.
#' A count characteristics enumerates the number of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @param conceptType the procedure type concept ids to use to limit the count
#' @return adds a countChar object of procedure into the clinChar extractSettings slot
#' @export
addProcedureCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = NULL) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.proc_count_tmp")
  } else {
    tbl_count <- "#proc_count"
  }

  procChar <- new("countChar", domain = "procedure_occurrence", orderId = set_order_id(clinChar),
                  categoryId = 4002L)
  procChar@conceptSets <- conceptSets
  if (!is.null(conceptType)) {
    procChar@conceptType <- as.integer(conceptType)
  }
  procChar@time <- timeWindows
  procChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    procChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, procChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a measurement count characteristic
#' @description
#' This function adds a count characteristic to the clinChar object for a measurement .
#' A count characteristics enumerates the number of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @param conceptType the measurement type concept ids to use to limit the count
#' @return adds a countChar object of measurement  into the clinChar extractSettings slot
#' @export
addMeasurementCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = NULL) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.meas_count_tmp")
  } else {
    tbl_count <- "#meas_count"
  }

  measChar <- new("countChar", domain = "measurement", orderId = set_order_id(clinChar),
                  categoryId = 5002L)
  measChar@time <- timeWindows
  measChar@conceptSets <- conceptSets
  if (!is.null(conceptType)) {
    measChar@conceptType <- as.integer(conceptType)
  }
  measChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    measChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, measChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a observation count characteristic
#' @description
#' This function adds a count characteristic to the clinChar object for a observation.
#' A count characteristics enumerates the number of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param conceptSets a list of concept sets that specify the codes to search within the domain
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @param conceptType the observation type concept ids to use to limit the count
#' @return adds a countChar object of observation into the clinChar extractSettings slot
#' @export
addObservationCount <- function(clinChar, timeWindows, conceptSets = NULL, categorize = NULL, conceptType = NULL) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_count <- glue::glue("{tempSchema}.obs_count_tmp")
  } else {
    tbl_count <- "#obs_count"
  }

  obsChar <- new("countChar", domain = "observation", orderId = set_order_id(clinChar),
                 categoryId = 6002L)
  obsChar@conceptSets <- conceptSets
  if (!is.null(conceptType)) {
    obsChar@conceptType <- as.integer(conceptType)
  }
  obsChar@time <- timeWindows
  obsChar@tempTables <- list(
    'count' = tbl_count,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    obsChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, obsChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

# Cost -------------------------------

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
#' If the parameter is NULL then no categorization summary is done
#' @return adds a costChar object of drug exposure into the clinChar extractSettings slot
#' @export
addDrugCost <- function(clinChar, timeWindows, conceptSets = NULL,
                        costType = "amount_allowed", categorize = NULL,
                        conceptType = c(32810, 32869)) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_cost <- glue::glue("{tempSchema}.drug_cost_tmp")
  } else {
    tbl_cost <- "#drug_cost"
  }

  drugChar <- new("costChar", domain = "drug_exposure", orderId = set_order_id(clinChar),
                  categoryId = 3003L)
  drugChar@costType <- costType
  drugChar@conceptSets <- conceptSets
  drugChar@conceptType <- as.integer(conceptType)
  drugChar@time <- timeWindows
  drugChar@tempTables <- list(
    'cost' = tbl_cost,
    'codeset' = c()
  )
  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    drugChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, drugChar)
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
#' If the parameter is NULL then no categorization summary is done
#' @return adds a costChar object of drug exposure into the clinChar extractSettings slot
#' @export
addProcedureCost <- function(clinChar, timeWindows, conceptSets = NULL,
                        costType = "amount_allowed", categorize = NULL,
                        conceptType = c(32810, 32869)) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_cost <- glue::glue("{tempSchema}.proc_cost_tmp")
  } else {
    tbl_cost <- "#drug_cost"
  }

  procChar <- new("costChar", domain = "procedure_occurrence", orderId = set_order_id(clinChar),
                  categoryId = 4003L)
  procChar@costType <- costType
  procChar@conceptSets <- conceptSets
  procChar@conceptType <- as.integer(conceptType)
  procChar@time <- timeWindows
  procChar@tempTables <- list(
    'cost' = tbl_cost,
    'codeset' = c()
  )
  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    procChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, procChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a vist cost characteristic
#' @description
#' This function adds a cost characteristic to the clinChar object for a visit.
#' A cost characteristics summarizes the total cost of events observed during a window of time.
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param timeWindows a timeWindow object that specifies the boundaries relative to the target start date
#' on when to search for the presence of a value. use `makeTimeTable` function
#' @param costType the column in the cost table to summarize
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a costChar object of visit into the clinChar extractSettings slot
#' @export
addVisitCost <- function(clinChar, timeWindows,
                         conceptSets = NULL,
                         costType = "amount_allowed",
                         categorize = NULL,
                         conceptType = c(32810, 32869)) {

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_cost <- glue::glue("{tempSchema}.visit_cost_tmp")
  } else {
    tbl_cost <- "#visit_cost"
  }

  visitChar <- new("costChar", domain = "visit_occurrence", orderId = set_order_id(clinChar),
                   categoryId = 8003L)
  visitChar@costType <- costType
  visitChar@conceptSets <- conceptSets
  visitChar@conceptType <- as.integer(conceptType)
  visitChar@time <- timeWindows
  visitChar@tempTables <- list(
    'cost' = tbl_cost,
    'codeset' = c()
  )
  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    visitChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

# Time In -----------------------------

#' Add a time in cohort characteristic
#' @description
#' This function finds the time spent in a cohort per patient. This takes
#' the difference between the cohort start date and end date
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a timeInChar object into the clinChar extractSettings slot
#' @export
addTimeInCohort <- function(clinChar, categorize = NULL) {

  cohortChar <- new("timeInChar", domain = "cohort", orderId = set_order_id(clinChar),
                    categoryId = 1007L)

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, cohortChar)
  return(clinChar)
}

#' Add a time in inpatient characteristic
#' @description
#' This function finds the time spent in inpatient hospitalization per patient. This takes
#' the difference between the visit start date and end date
#' @param clinChar a clinChar object maintaining the components of the characterization
#' @param categorize describes how the continuous value should be categorized.
#' This function takes a breaksStrategy object to describe the categories ow it is left NULL.
#' If the parameter is NULL then no categorization summary is done
#' @return adds a timeInChar object into the clinChar extractSettings slot
#' @export
addTimeInInpatient <- function(clinChar, categorize = NULL) {

  cohortChar <- new("timeInChar", domain = "inpatient", orderId = set_order_id(clinChar),
                    categoryId = 8005L)

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    char@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, cohortChar)
  return(clinChar)
}

# Time To -------------------------------

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
#' If the parameter is NULL then no categorization summary is done
#' @return adds a timeToChar object of drug exposure into the clinChar extractSettings slot
#' @export
addTimeToDrug <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                          categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_duration <- glue::glue("{tempSchema}.drug_duration_tmp")
  } else {
    #tbl_codeset <- "#drug_codeset"
    tbl_duration <- "#drug_duration"
  }

  drugChar <- new("timeToChar", domain = "drug_exposure", orderId = set_order_id(clinChar),
                  categoryId = 3004L)
  drugChar@conceptSets <- conceptSets
  drugChar@time <- timeWindows
  drugChar@limit <- limit
  drugChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
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
#' If the parameter is NULL then no categorization summary is done
#' @return adds a timeToChar object of condition into the clinChar extractSettings slot
#' @export
addTimeToCondition <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                          categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.drug_codeset_tmp")
    tbl_duration <- glue::glue("{tempSchema}.cond_duration_tmp")
  } else {
    #tbl_codeset <- "#drug_codeset"
    tbl_duration <- "#cond_duration"
  }

  condChar <- new("timeToChar", domain = "condition_occurrence", orderId = set_order_id(clinChar),
                  categoryId = 2004L)
  condChar@conceptSets <- conceptSets
  condChar@time <- timeWindows
  condChar@limit <- limit
  condChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    condChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, condChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}

#' Add a procedure timeTo characteristic
#' @description
#' This function adds a timeTo characteristic to the clinChar object for a procedure.
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
#' @return adds a timeToChar object of procedure into the clinChar extractSettings slot
#' @export
addTimeToProcedure <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                               categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_duration <- glue::glue("{tempSchema}.proc_duration_tmp")
  } else {
    tbl_duration <- "#proc_duration"
  }

  procChar <- new("timeToChar", domain = "procedure_occurrence", orderId = set_order_id(clinChar),
                  categoryId = 4004L)
  procChar@conceptSets <- conceptSets
  procChar@time <- timeWindows
  procChar@limit <- limit
  procChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    procChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, procChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a measurement timeTo characteristic
#' @description
#' This function adds a timeTo characteristic to the clinChar object for a measurement.
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
#' @return adds a timeToChar object of measurement into the clinChar extractSettings slot
#' @export
addTimeToMeasurement <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                               categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_duration <- glue::glue("{tempSchema}.meas_duration_tmp")
  } else {
    tbl_duration <- "#meas_duration"
  }

  measChar <- new("timeToChar", domain = "measurement", orderId = set_order_id(clinChar),
                  categoryId = 5004L)
  measChar@conceptSets <- conceptSets
  measChar@time <- timeWindows
  measChar@limit <- limit
  measChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    measChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, measChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}


#' Add a observation timeTo characteristic
#' @description
#' This function adds a timeTo characteristic to the clinChar object for a observation.
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
#' @return adds a timeToChar object of observation into the clinChar extractSettings slot
#' @export
addTimeToObservation <- function(clinChar, conceptSets, timeWindows, limit = c("first", "last", "all"),
                                 categorize = NULL) {

  limit <- match.arg(limit)

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    tbl_duration <- glue::glue("{tempSchema}.obs_duration_tmp")
  } else {
    tbl_duration <- "#obs_duration"
  }

  obsChar <- new("timeToChar", domain = "observation", orderId = set_order_id(clinChar),
                 categoryId = 6004L)
  obsChar@conceptSets <- conceptSets
  obsChar@time <- timeWindows
  obsChar@limit <- limit
  obsChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    obsChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, obsChar)
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

  # check if clinChar is snowflake and use temp schema
  if (check_dbms(clinChar) == "snowflake") {
    tempSchema <-clinChar@executionSettings@workDatabaseSchema
    #tbl_codeset <- glue::glue("{tempSchema}.visit_codeset_tmp")
    tbl_duration <- glue::glue("{tempSchema}.visit_duration_tmp")
  } else {
    #tbl_codeset <- "#visit_codeset"
    tbl_duration <- "#visit_duration"
  }

  visitChar <- new("timeToChar", domain = "visit_occurrence", orderId = set_order_id(clinChar),
                   categoryId = 8004L)
  visitChar@conceptSets <- conceptSets
  visitChar@time <- timeWindows
  visitChar@limit <- limit
  visitChar@tempTables <- list(
    'duration' = tbl_duration,
    'codeset' = c()
  )

  if (!is.null(categorize)) {
    if (!methods::is(categorize, "breaksStrategy")) {
      stop("categorize needs to be a breaksStrategy object")
    }
    visitChar@categorize <- categorize
  }

  clinChar@extractSettings <- append(clinChar@extractSettings, visitChar)
  clinChar <- infuse_codset_id(clinChar)
  return(clinChar)
}
