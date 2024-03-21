# verbose version of execute
verboseExecute <- function(connection, sql, task, printSql = FALSE) {

  cli::cat_bullet(
    glue::glue("Executing Task: {crayon::magenta(task)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  DatabaseConnector::executeSql(connection = connection, sql = sql)

  if (printSql) {
    cli::cat_bullet(
      glue::glue("Task {crayon::magenta(task)} done using:"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    cli::cat_line(sql, col = "#FFC000")
  }

  invisible(sql)
}

pluck_domain_char <- function(clinChar) {
  es <- clinChar@extractSettings
  clin_class <- purrr::map_chr(es, ~class(.x))
  idx <- which(clin_class %in% c("presenceChar", "labChar", "countChar", "costChar", "timeToChar"))
  dd <- es[idx]
  return(dd)
}


domain_translate <- function(domain) {
  tt <- switch(domain,
               "condition_occurrence" = list(
                 'record_id' = "condition_occurrence_id",
                 'concept_id' ="condition_concept_id",
                 'concept_type_id' = "condition_type_concept_id",
                 'event_date' = "condition_start_date"
               ),
               "drug_exposure" = list(
                 'record_id' = "drug_exposure_id",
                 'concept_id' = "drug_concept_id",
                 'concept_type_id' = "drug_type_concept_id",
                 'event_date' = "drug_exposure_start_date"
               ),
               "procedure_occurrence" = list(
                 'record_id' = "procedure_occurrence_id",
                 'concept_id' = "procedure_concept_id",
                 'concept_type_id' = "procedure_type_concept_id",
                 'event_date' = "procedure_date"
               ),
               "measurement" = list(
                 'record_id' = "measurement_id",
                 'concept_id' = "measurement_concept_id",
                 'concept_type_id' = "measurement_type_concept_id",
                 'event_date' = "measurement_date"
               ),
               "gender" = list('concept_id' ="gender_concept_id"),
               "race" = list('concept_id' = "race_concept_id"),
               "ethnicity" = list('concept_id' = "ethnicity_concept_id")
  )
  return(tt)
}


grab_concept <- function(clinChar, connection, ids) {

  ids <- paste(ids, collapse = ", ")
  cdmDatabaseSchema <- clinChar@executionSettings@cdmDatabaseSchema
  sql <- glue::glue("SELECT c.concept_id, c.concept_name
  FROM {cdmDatabaseSchema}.concept c WHERE c.concept_id IN ({ids})") |>
    SqlRender::translate(targetDialect = clinChar@executionSettings@dbms)

  cli::cat_bullet(
    glue::glue("Database Query: {crayon::green('Grab concept names from CDM')}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  concept_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql)
  names(concept_tbl) <- c("concept_id", "concept_name")

  return(concept_tbl)
}

grab_locations <- function(clinChar, connection) {

  cdmDatabaseSchema <- clinChar@executionSettings@cdmDatabaseSchema
  sql <- glue::glue("SELECT l.location_id, l.location_source_value
  FROM {cdmDatabaseSchema}.location l") |>
    SqlRender::translate(targetDialect = clinChar@executionSettings@dbms)

  cli::cat_bullet(
    glue::glue("Database Query: {crayon::green('Grab unique locations from CDM')}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  loc_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql)
  names(loc_tbl) <- c("value_id", "value_name")

  return(loc_tbl)
}

race_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    8527, "White",
    38003599, "African American",
    8516, "Black or African American",
    8515, "Asian",
    0, "Not Identified"
  )
}

gender_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    8532, "Female",
    8507, "Male",
    0, "Not Identified"
  )
}

ethnicity_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    38003563, "Hispanic or Latino",
    38003564, "Not Hispanic or Latino",
    0, "Not Identified"
  )
}

cost_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    44818668, "USD",
    44818568, "EUR",
    44818571, "GBP"
  )
}


count_label <- function(domain) {
  lb <- switch(domain,
               'drug_exposure' = "medication count",
               'procedure_occurrence' = "procedure count",
               'measurement' = "measurement count",
               'condition_occurrence' = "condition count")
  return(lb)
}
