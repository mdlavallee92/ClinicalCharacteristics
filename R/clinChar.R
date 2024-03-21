# ClinChar --------------------------

setClass("ClinChar",
         slots = c(targetCohort = "targetCohort",
                   executionSettings = "executionSettings",
                   stowSettings = "stowSettings",
                   extractSettings = "list"),
         prototype = list(
           targetCohort = new("targetCohort"),
           executionSettings = new("executionSettings"),
           stowSettings = new("stowSettings"),
           extractSettings = list()
         )
)

setGeneric("check_dbms", function(x)  standardGeneric("check_dbms"))
setMethod("check_dbms", "ClinChar", function(x){
  x@executionSettings@dbms
})


#' Create a clinChar object
#' @description
#' This function creates a clinChar object used to run the characterization
#' @param targetCohortIds the cohort ids from the cohort table needed for description
#' @param targetCohortNames the names of the cohorts corresponding to these ids
#' @param dbms the dbms where the OMOP data sits
#' @param cdmDatabaseSchema the database schema string specifying where the cdm sits
#' @param vocabularyDatabaseSchema the database schema string specifying where the vocabulary sits
#' @param workDatabaseSchema the database schema string specifying where the cohor table sits
#' @param cohortTable the table where the cohorts are
#' @return makes a clinChar object
#' @export
makeClinChar <- function(targetCohortIds,
                         targetCohortNames = NULL,
                         dbms,
                         cdmDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         workDatabaseSchema,
                         cohortTable) {
  # make new clin char object
  clinChar <- new("ClinChar")
  # add target ids
  clinChar@targetCohort@id <- as.integer(targetCohortIds)

  # add target cohort names
  if (is.null(targetCohortNames)) {
    clinChar@targetCohort@name <- glue::glue("cohort_{targetCohortIds}")
  } else {
    clinChar@targetCohort@name <- targetCohortNames
  }

  if (dbms == "snowflake") {
    clinChar@targetCohort@tempTable <- glue::glue("{workDatabaseSchema}.target_tmp")
    clinChar@stowSettings@dataTable <- glue::glue("{workDatabaseSchema}.dat_tmp")
    clinChar@executionSettings@timeWindowTable <- glue::glue("{workDatabaseSchema}.tw_tmp")

  }

  # add execution settings
  clinChar@executionSettings@dbms <- dbms
  clinChar@executionSettings@cdmDatabaseSchema <- cdmDatabaseSchema
  clinChar@executionSettings@vocabularyDatabaseSchema <- vocabularyDatabaseSchema
  clinChar@executionSettings@workDatabaseSchema <- workDatabaseSchema
  clinChar@executionSettings@cohortTable <- cohortTable


  return(clinChar)
}

make_dat_table <- function() {
  sql <- glue::glue("CREATE TABLE {{dataTable}} (
  cohort_id int NOT NULL,
  subject_id bigint NOT NULL,
  category_id int NOT NULL,
  time_id int NOT NULL,
  value_id bigint NOT NULL,
  value int NOT NULL
)
;")
return(sql)
}


setGeneric("build_query", function(x)  standardGeneric("build_query"))
setMethod("build_query", "ClinChar", function(x){

  # make params to paste into sql
  cdmDatabaseSchema <- x@executionSettings@cdmDatabaseSchema
  workDatabaseSchema <- x@executionSettings@workDatabaseSchema
  vocabularyDatabaseSchema <- x@executionSettings@vocabularyDatabaseSchema
  cohortTable <- x@executionSettings@cohortTable
  dataTable <- x@stowSettings@dataTable
  targetTable <- x@targetCohort@tempTable
  timeWindowTable <- x@executionSettings@timeWindowTable

  # collect all sql for char run
  collect_sql <- glue::glue(
    make_dat_table(), # make the dat table
    "\n\n",
    as_sql(x@targetCohort), # create target cohort
    "\n\n",
    paste(purrr::map_chr(x@extractSettings, ~as_sql(.x)), collapse = "\n\n"), # run covars
    "\n\n-- Drop Temp Tables\n",
    drop_temp_tables(x@targetCohort),
    "\n\n",
    drop_temp_tables(x@executionSettings),
    "\n\n",
    drop_domain_temp(x)# drop tables
  ) |>
    SqlRender::translate(
      targetDialect = x@executionSettings@dbms
    )

  return(collect_sql)

})


#' Runs the characterization and extracts data into an arrow object
#' @description
#' This runs the characterization specified by the clinChar object
#' @param connection the DatabaseConnector connection linking to the dbms with OMOP data
#' @param clinChar the clinChar object describing the study
#' @return runs database query described in extractSettings and uploads them to the stow object
#' @export
runClinicalCharacteristics <- function(connection, clinChar) {

  # build sql
  sql <- build_query(clinChar)

  insert_time_table(connection = connection, clinChar = clinChar)

  # execute on db
  DatabaseConnector::executeSql(connection = connection, sql = sql)

  stowTable(connection = connection, clinChar = clinChar)

  stowCount(connection = connection, clinChar = clinChar)

  invisible(sql)
}
