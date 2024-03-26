# sql generic


# Classes --------------------

setClass("breaksStrategy",
         slots = c(
           breaks = "data.frame"
         ),
         prototype = list(
           breaks = data.frame(
             value = c(),
             grp = c()
           )
         )
)

## Target Cohort --------------------
setClass("targetCohort",
         slots = c(
           id = "integer",
           name = "character",
           tempTable = "character"
         ),
         prototype = list(
           id = NA_integer_,
           name = NA_character_,
           tempTable = "#target"
         )
)

## Execution Settings --------------------------
setClass("executionSettings",
         slots = c(
           dbms = "character",
           cdmDatabaseSchema = "character",
           vocabularyDatabaseSchema = "character",
           workDatabaseSchema = "character",
           cohortTable = "character",
           timeWindowTable = "character",
           codesetTable = "character"
         ),
         prototype = list(
           dbms = NA_character_,
           cdmDatabaseSchema = NA_character_,
           vocabularyDatabaseSchema = NA_character_,
           workDatabaseSchema = NA_character_,
           cohortTable = NA_character_,
           timeWindowTable = "#tw",
           codesetTable = "#Codesets"
         )
)

## Stow Settings ----------------------------
setClass("stowSettings",
         slots = c(
           dataTable = "character",
           dataLoc = "character",
           countLoc = "character",
           format = "character"
         ),
         prototype = list(
           dataTable = "#dat",
           dataLoc = fs::file_temp(pattern = "cov", ext = "parquet"),
           countLoc = fs::file_temp(pattern = "count", ext = "parquet"),
           format = "parquet"
         )
)

## Age Char --------------------

setClass("ageChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           categorize = "ANY"
         ),
         prototype = list(
           domain = "age",
           orderId = NA_integer_,
           categorize = NULL
         )
)

setClass("yearChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           categorize = "ANY"
         ),
         prototype = list(
           domain = "year",
           orderId = NA_integer_,
           categorize = NULL
         )
)


## Location Char -------------------------------------

setClass("locationTable",
         slots = c(
           column = "character",
           key = "data.frame"
         ),
         prototype = list(
           column = "location_source_value",
           key = tibble::tibble(
             value_id = c(),
             value_name = c()
           )
         )
)

setClass("locationChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           locationTable = "locationTable"
         ),
         prototype = list(
           domain = "location",
           orderId = NA_integer_,
           locationTable = new("locationTable")
         )
)


## Demo Concept Char --------------------------
setClass("demoConceptChar",
         slots = c(
           domain = "character",
           orderId = "integer"
         ),
         prototype = list(
           domain = NA_character_,
           orderId = NA_integer_
         )
)

## Lab Char ------------------------
setClass("labUnitTable",
         slots = c(
            key = "data.frame"
         ),
         prototype = list(
           key = tibble::tibble(
             measurement_concept_id = c(),
             measurement_name = c(),
             unit_concept_id = c(),
             unit_name = c(),
             lab_unit_code = c()
           )
         )
)


setClass("labChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           labUnitTable = "labUnitTable",
           limit = "character",
           time = "data.frame",
           tempTables = "list",
           categorize = "ANY"
         ),
         prototype = list(
           domain = "labs",
           orderId = NA_integer_,
           labUnitTable = new("labUnitTable"),
           limit = "first",
           time = data.frame('time_id' = 1, 'time_a' = -365, 'time_b' = -1),
           tempTables = list(),
           categorize = NULL
         )
)

## Presence Char -------------------

setClass("presenceChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           conceptSets = "list",
           limit = "character",
           time = "data.frame",
           tempTables = "list"
         ),
         prototype = list(
           domain = NA_character_,
           orderId = NA_integer_,
           conceptSets = list(),
           limit = "last",
           time = data.frame('time_id' = 1, 'time_a' = -365, 'time_b' = -1),
           tempTables = list()
         )
)

## timeIn Char -------------------

setClass("timeInChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           categorize = "ANY"
         ),
         prototype = list(
           domain = NA_character_,
           orderId = NA_integer_,
           categorize = NULL
         )
)

## timeTo Char -------------------

setClass("timeToChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           conceptSets = "list",
           limit = "character",
           time = "data.frame",
           tempTables = "list",
           categorize = "ANY"
         ),
         prototype = list(
           domain = NA_character_,
           orderId = NA_integer_,
           conceptSets = list(),
           limit = "first",
           time = data.frame('time_id' = 1, 'time_a' = -365, 'time_b' = -1),
           tempTables = list(),
           categorize = NULL
         )
)

## Count Char ----------------
setClass("countChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           conceptSets = "ANY",
           conceptType = "integer",
           time = "data.frame",
           tempTables = "list",
           categorize = "ANY"
         ),
         prototype = list(
           domain = NA_character_,
           orderId = NA_integer_,
           conceptType = 32869L,
           conceptSets = NULL,
           time = data.frame('time_id' = 1, 'time_a' = -365, 'time_b' = -1),
           tempTables = list(),
           categorize = NULL
         )
)

## Cost Char ----------------------
setClass("costChar",
         slots = c(
           domain = "character",
           orderId = "integer",
           conceptSets = "ANY",
           conceptType = "integer",
           costType = "character",
           time = "data.frame",
           tempTables = "list",
           categorize = "ANY"
         ),
         prototype = list(
           domain = NA_character_,
           orderId = NA_integer_,
           costType = "amount_allowed",
           conceptType = 32869L,
           time = data.frame('time_id' = 1, 'time_a' = -365, 'time_b' = -1),
           tempTables = list(),
           categorize = NULL
         )
)

# limit_sql --------------------
setGeneric("limit_sql", function(x)  standardGeneric("limit_sql"))

setMethod("limit_sql", "labChar", function(x) {

  domain <- x@domain
  domain_trans <- domain_translate(domain)

  limit_type <- x@limit
  if (limit_type == "last") {
    sql <- glue::glue(
      "SELECT a.cohort_id, a.subject_id, a.time_id, a.value_id, a.value
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.time_id, l.value_id order by l.measurement_date DESC) as ordinal
        FROM {x@tempTables$lab} l
      ) a
      WHERE ordinal = 1
      ")
  }

  if (limit_type == "first") {
    sql <- glue::glue(
      "SELECT a.cohort_id, a.subject_id, a.time_id, a.value_id, a.value
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.time_id, l.value_id order by l.measurement_date ASC) as ordinal
        FROM {x@tempTables$lab} l
      ) a
      WHERE ordinal = 1
      ")
  }

  if (limit_type == "all") {
    sql <- glue::glue(
      "SELECT l.cohort_id, l.subject_id, l.category_id, l.time_id, l.value_id, l.value
      FROM {x@tempTables$lab} l")
  }
  return(sql)

})

setMethod("limit_sql", "presenceChar", function(x) {

  domain <- x@domain
  domain_trans <- domain_translate(domain)

  limit_type <- x@limit
  if (limit_type == "last") {
    sql <- glue::glue(
      "SELECT a.cohort_id, a.subject_id, a.time_id, a.value_id
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.time_id, l.value_id order by l.{domain_trans$event_date} DESC) as ordinal
        FROM {x@tempTables$domain} l
      ) a
      WHERE ordinal = 1
      ")
  }

  if (limit_type == "first") {
    sql <- glue::glue(
      "SELECT a.cohort_id, a.subject_id, a.time_id, a.value_id
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.time_id, l.value_id order by l.{domain_trans$event_date} ASC) as ordinal
        FROM {x@tempTables$domain} l
      ) a
      WHERE ordinal = 1
      ")
  }

  if (limit_type == "all") {
    sql <- glue::glue(
      "SELECT l.cohort_id, l.subject_id, l.category_id, l.time_id, l.value_id
      FROM {x@tempTables$domain} l")
  }
  return(sql)

})

setMethod("limit_sql", "timeToChar", function(x) {

  domain <- x@domain
  domain_trans <- domain_translate(domain)

  limit_type <- x@limit
  if (limit_type == "last") {
    sql <- glue::glue(
      "SELECT a.cohort_id, a.subject_id, a.time_id, a.value_id, a.value
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.time_id, l.value_id order by l.{domain_trans$event_date} DESC) as ordinal
        FROM {x@tempTables$duration} l
      ) a
      WHERE ordinal = 1
      ")
  }

  if (limit_type == "first") {
    sql <- glue::glue(
      "SELECT a.cohort_id, a.subject_id, a.time_id, a.value_id, a.value
      FROM (
        SELECT l.*,
        ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.time_id, l.value_id order by l.{domain_trans$event_date} ASC) as ordinal
        FROM {x@tempTables$duration} l
      ) a
      WHERE ordinal = 1
      ")
  }

  if (limit_type == "all") {
    sql <- glue::glue(
      "SELECT l.cohort_id, l.subject_id, l.category_id, l.time_id, l.value_id, l.value
      FROM {x@tempTables$duration} l")
  }
  return(sql)

})
# as_sql ----------------------------

## generic---------
setGeneric("as_sql", function(x)  standardGeneric("as_sql"))

## target cohort-------------
setMethod("as_sql", "targetCohort", function(x){

  ids <- paste(x@id, collapse = ", ")
  sql <- glue::glue(
    "-- Make target cohort temp table
     SELECT *
     INTO {{targetTable}}
     FROM {{workDatabaseSchema}}.{{cohortTable}}
     WHERE cohort_definition_id IN ({ids});"
  )
  return(sql)

})


## age --------------

setMethod("as_sql", "ageChar", function(x){

  domain <- x@domain
  sql <- glue::glue(
    "-- Make {domain} query
     INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
     SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     {x@orderId} AS category_id,
     -999 AS time_id,
     -999 AS value_id,
     YEAR(t.cohort_start_date) - d.year_of_birth AS value
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.person d
     ON t.subject_id = d.person_id;"
  )
  return(sql)

})


## year --------------

setMethod("as_sql", "yearChar", function(x){

  domain <- x@domain
  sql <- glue::glue(
    "-- Make {domain} query
     INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
     SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     {x@orderId} AS category_id,
     -999 AS time_id,
     YEAR(t.cohort_start_date) AS value_id,
     1 AS value
     FROM {{targetTable}} t
     ;"
  )
  return(sql)

})

## demographic --------------
setMethod("as_sql", "demoConceptChar", function(x){


  domain <- x@domain
  demo_trans <- domain_translate(domain)

  sql <- glue::glue(
    "-- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     {x@orderId} AS category_id,
     -999 AS time_id,
     d.{demo_trans$concept_id} AS value_id,
     1 AS value
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.person d
     ON t.subject_id = d.person_id;"
  )

  return(sql)
})


## location -------------
setMethod("as_sql", "locationChar", function(x){

  domain <- x@domain
  sql <- glue::glue(
    "-- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     {x@orderId} AS category_id,
     -999 AS time_id,
     d.location_id AS value_id,
     1 AS value
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.person d
     ON t.subject_id = d.person_id;"
  )

  return(sql)
})


## lab vlaues ------------------
setMethod("as_sql", "labChar", function(x){

  labIds <- unique(x@labUnitTable@key$measurement_concept_id) |>
    paste(collapse = ", ")
  unitIds <- unique(x@labUnitTable@key$unit_concept_id) |>
    paste(collapse = ", ")
  query_sql <- glue::glue(
    "
    -- Find matching lab values
    WITH labs AS (
        SELECT M.person_id, M.measurement_concept_id, M.measurement_date,
        M.value_as_number, M.unit_concept_id,
        CAST((CAST(M.measurement_concept_id AS BIGINT) * 1000000) + (M.unit_concept_id - (FLOOR(M.unit_concept_id / 1000) * 1000)) AS BIGINT) AS value_id
        FROM {{cdmDatabaseSchema}}.measurement M
        WHERE M.measurement_concept_id IN ({labIds})
        AND M.value_as_number is not null
        AND M.unit_concept_id IN ({unitIds})
    )
    SELECT
      t.cohort_definition_id AS cohort_id,
      t.subject_id,
      t.cohort_start_date,
      tw.time_id,
      l.value_id,
      l.value_as_number AS value,
      l.measurement_date
     INTO {x@tempTables$lab}
     FROM {{targetTable}} t
     JOIN labs l ON t.subject_id = l.person_id
     INNER JOIN {{timeWindowTable}} tw
          ON DATEADD(day, tw.time_a, t.cohort_start_date) <= l.measurement_date
          AND DATEADD(day, tw.time_b, t.cohort_start_date) >= l.measurement_date
     ;
   ")


  pull_sql <- glue::glue(
    " -- Make Lab query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT i.cohort_id, i.subject_id,
    {x@orderId} AS category_id,
    i.time_id,
    i.value_id,
    i.value
    FROM (
    {limit_sql(x)}
    ) i
    ;
    "
  )
  sql <- paste(query_sql, pull_sql, sep = "\n\n")
  return(sql)

})

## presence --------------------
setMethod("as_sql", "presenceChar", function(x){

  domain <- x@domain
  domain_trans <- domain_translate(domain)
  time_a <- paste(x@time$time_a, collapse = ", ")
  time_b <- paste(x@time$time_b, collapse = ", ")
  codesetIds <- paste(x@tempTables$codeset, collapse = ", ")

  #codesetSql <- bind_codeset_queries(x@conceptSets, codesetTable = x@tempTables$codeset)
  querySql <- glue::glue(
    "
    WITH T1 AS (
      SELECT * FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    ),
    T2 AS (
      SELECT * FROM {{codesetTable}} cs
      WHERE codeset_id IN ({codesetIds})
    )
    -- Find matching {domain} covariates
    SELECT
      t.cohort_definition_id AS cohort_id,
      t.subject_id, t.cohort_start_date,
      tw.time_id,
      cs.codeset_id AS value_id, d.{domain_trans$event_date}
     INTO {x@tempTables$domain}
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.{domain} d ON t.subject_id = d.person_id
     JOIN T2 cs on (d.{domain_trans$concept_id} = cs.concept_id)
     INNER JOIN T1 tw
          ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
     ;")

  insertSql <- glue::glue(
    " -- Insert into data table
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT i.cohort_id, i.subject_id,
    {x@orderId} AS category_id,
    i.time_id,
    i.value_id,
    1 AS value
    FROM (
    {limit_sql(x)}
    ) i
    ;
    ")

  sql <- paste(querySql, insertSql, sep = "\n\n")
  return(sql)

})


## count --------------------
setMethod("as_sql", "countChar", function(x){

  domain <- x@domain
  domain_trans <- domain_translate(domain)
  time_a <- paste(x@time$time_a, collapse = ", ")
  time_b <- paste(x@time$time_b, collapse = ", ")
  conceptType <- paste(x@conceptType, collapse = ", ")

  if (!is.null(x@conceptSets)) {
    codesetIds <- paste(x@tempTables$codeset, collapse = ", ")
    querySql <- glue::glue(
      "
    WITH T0 AS (
      SELECT * FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    ),
    T1 AS (
      SELECT * FROM {{codesetTable}} cs
      WHERE codeset_id IN ({codesetIds})
    ),
    T2 AS (
    -- Find matching {domain} covariates
    SELECT a.*,
    tw.time_id,
    cs.codeset_id AS value_id,
    d.{domain_trans$record_id}, d.{domain_trans$concept_id}, d.{domain_trans$event_date}
    FROM {{targetTable}} a
    JOIN {{cdmDatabaseSchema}}.{domain} d ON a.subject_id = d.person_id
    JOIN T1 cs on (d.{domain_trans$concept_id} = cs.concept_id)
    INNER JOIN T0 tw
          ON DATEADD(day, tw.time_a, a.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, a.cohort_start_date) >= d.{domain_trans$event_date}
    WHERE d.{domain_trans$concept_id} <> 0
    AND {domain_trans$concept_type_id} IN ({conceptType})
    )
    SELECT d.cohort_definition_id, d.subject_id, d.time_id, d.value_id, COUNT(d.{domain_trans$record_id}) AS value
    INTO {x@tempTables$count}
    FROM T2 d
    GROUP BY d.cohort_definition_id, d.subject_id, d.time_id, d.value_id
    ;

    -- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT cohort_definition_id AS cohort_id, subject_id,
    {x@orderId} AS category_id,
    time_id,
    value_id,
    value
    FROM {x@tempTables$count}
    ;")

  } else{

    querySql <- glue::glue(
      "
    WITH T0 AS (
      SELECT * FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    ),
    T1 AS (
    -- Find matching {domain} covariates
    SELECT a.*,
    tw.time_id,
    d.{domain_trans$record_id}, d.{domain_trans$concept_id}, d.{domain_trans$event_date}
    FROM {{targetTable}} a
    JOIN {{cdmDatabaseSchema}}.{domain} d ON a.subject_id = d.person_id
    INNER JOIN T0 tw
          ON DATEADD(day, tw.time_a, a.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, a.cohort_start_date) >= d.{domain_trans$event_date}
    WHERE d.{domain_trans$concept_id} <> 0
    AND {domain_trans$concept_type_id} IN ({conceptType})
    )
    SELECT d.cohort_definition_id, d.subject_id, d.time_id, COUNT(d.{domain_trans$record_id}) AS value
    INTO {x@tempTables$count}
    FROM T1 d
    GROUP BY d.cohort_definition_id, d.subject_id, d.time_id
    ;

    -- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT cohort_definition_id AS cohort_id, subject_id,
    {x@orderId} AS category_id,
    time_id,
    -999 AS value_id,
    value
    FROM {x@tempTables$count}
    ;")
  }
  #sql <- paste(codesetSql, querySql, sep = "\n\n")
  return(querySql)

})


## cost -----------------------
setMethod("as_sql", "costChar", function(x){

  domain <- x@domain
  domain_trans <- domain_translate(domain)
  time_a <- paste(x@time$time_a, collapse = ", ")
  time_b <- paste(x@time$time_b, collapse = ", ")
  conceptType <- paste(x@conceptType, collapse = ", ")

  if (!is.null(x@conceptSets)) {
    codesetIds <- paste(x@tempTables$codeset, collapse = ", ")
    querySql <- glue::glue(
      "
    WITH T0 AS (
      SELECT * FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    ),
    T1 AS (
      SELECT * FROM {{codesetTable}} cs
      WHERE codeset_id IN ({codesetIds})
    ),
    T2 AS (
    -- Find matching {domain} covariates
    SELECT a.*,
    tw.time_id,
    cs.codeset_id AS value_id,
    d.{domain_trans$record_id}, d.{domain_trans$concept_id}, d.{domain_trans$event_date},
    cc.{x@costType}
    FROM {{targetTable}} a
    JOIN {{cdmDatabaseSchema}}.{domain} d ON a.subject_id = d.person_id
    JOIN T1 cs on (d.{domain_trans$concept_id} = cs.concept_id)
    JOIN {{cdmDatabaseSchema}}.cost cc ON d.{domain_trans$record_id} = cc.cost_event_id
    INNER JOIN T0 tw
          ON DATEADD(day, tw.time_a, a.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, a.cohort_start_date) >= d.{domain_trans$event_date}
    WHERE d.{domain_trans$concept_id} <> 0
    AND {domain_trans$concept_type_id} IN ({conceptType})
    )
    SELECT d.cohort_definition_id, d.subject_id, d.time_id, d.value_id, FLOOR(SUM(d.{x@costType})) AS value
    INTO {x@tempTables$cost}
    FROM T2 d
    GROUP BY d.cohort_definition_id, d.subject_id, d.time_id, d.value_id
    ;

    -- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT cohort_definition_id AS cohort_id, subject_id,
    {x@orderId} AS category_id,
    time_id,
    value_id,
    value
    FROM {x@tempTables$cost}
    ;")

  } else{

    querySql <- glue::glue(
      "
    WITH T0 AS (
      SELECT * FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    ),
    T1 AS (
    -- Find matching {domain} covariates
    SELECT a.*,
    tw.time_id,
    d.{domain_trans$record_id}, d.{domain_trans$concept_id}, d.{domain_trans$event_date},
    cc.{x@costType}, cc.currency_concept_id
    FROM {{targetTable}} a
    JOIN {{cdmDatabaseSchema}}.{domain} d ON a.subject_id = d.person_id
    JOIN {{cdmDatabaseSchema}}.cost cc ON d.{domain_trans$record_id} = cc.cost_event_id
    INNER JOIN T0 tw
          ON DATEADD(day, tw.time_a, a.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, a.cohort_start_date) >= d.{domain_trans$event_date}
    WHERE d.{domain_trans$concept_id} <> 0
    AND {domain_trans$concept_type_id} IN ({conceptType})
    )
    SELECT d.cohort_definition_id, d.subject_id, d.time_id, d.currency_concept_id, FLOOR(SUM(d.{x@costType})) AS value
    INTO {x@tempTables$cost}
    FROM T1 d
    GROUP BY d.cohort_definition_id, d.subject_id, d.time_id, d.currency_concept_id
    ;

    -- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT cohort_definition_id AS cohort_id, subject_id,
    {x@orderId} AS category_id,
    time_id,
    currency_concept_id AS value_id,
    value
    FROM {x@tempTables$cost}
    ;")
  }
  #sql <- paste(codesetSql, querySql, sep = "\n\n")
  return(querySql)

})


## timeIn --------------------------
setMethod("as_sql", "timeInChar", function(x){

  domain <- x@domain

  if (domain == "cohort") {
    querySql <- glue::glue(
      "-- Make time in {domain} query
     INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
     SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     {x@orderId} AS category_id,
     -999 AS time_id,
     1001 AS value_id,
     DATEDIFF(day, t.cohort_start_date, t.cohort_end_date) AS value
     FROM {{targetTable}} t;
    "
    )
  }

  if (domain == "inpatient") {
    querySql <- glue::glue(
      "-- Make time in {domain} query
      INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
     SELECT d.cohort_definition_id AS cohort_id, d.subject_id,
     {x@orderId} AS category_id,
     -999 AS time_id,
     9201000262 AS value_id,
     d.value
     FROM (
      SELECT t.*,
        DATEDIFF(day, vo.visit_start_date, vo.visit_end_date) AS value
        FROM {{targetTable}} t
        JOIN {{cdmDatabaseSchema}}.visit_occurrence vo
          ON t.subject_id = vo.person_id
            AND t.cohort_start_date <= vo.visit_start_date
            AND vo.visit_start_date <= t.cohort_end_date
        WHERE visit_concept_id IN (262, 9201)
     ) d;
    ")
  }

  return(querySql)
})

## timeTo --------------------------
setMethod("as_sql", "timeToChar", function(x){

  domain <- x@domain
  domain_trans <- domain_translate(domain)
  time_a <- paste(x@time$time_a, collapse = ", ")
  time_b <- paste(x@time$time_b, collapse = ", ")
  codesetIds <- paste(x@tempTables$codeset, collapse = ", ")

  querySql <- glue::glue(
    "
    WITH T1 AS (
      SELECT *
      FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    ),
    T2 AS (
      SELECT * FROM {{codesetTable}} cs
      WHERE codeset_id IN ({codesetIds})
    )
    -- Find time to {domain}
    SELECT
      t.cohort_definition_id AS cohort_id,
      t.subject_id, t.cohort_start_date,
      tw.time_id,
      cs.codeset_id AS value_id, d.{domain_trans$event_date},
      DATEDIFF(day, t.cohort_start_date, d.{domain_trans$event_date}) AS value
     INTO {x@tempTables$duration}
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.{domain} d ON t.subject_id = d.person_id
     JOIN T2 cs on (d.{domain_trans$concept_id} = cs.concept_id)
     INNER JOIN T1 tw
          ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
     ;")

  insertSql <- glue::glue(
    " -- Insert into data table
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT i.cohort_id, i.subject_id,
    {x@orderId} AS category_id,
    i.time_id,
    i.value_id,
    i.value
    FROM (
    {limit_sql(x)}
    ) i
    ;
    ")

  sql <- paste(querySql, insertSql, sep = "\n\n")
  return(sql)

})

# Drop temp tables ------------------------------

setGeneric("drop_temp_tables", function(x)  standardGeneric("drop_temp_tables"))

trunc_drop <- function(table) {
  sql <- glue::glue("TRUNCATE TABLE {table}; DROP TABLE {table};")
  return(sql)
}

drop_domain_temp <- function(clinChar) {
  dd <- pluck_domain_char(clinChar)
  sql <- purrr::map_chr(dd, ~drop_temp_tables(.x)) |>
    paste(collapse = "\n\n")
  return(sql)
}

setMethod("drop_temp_tables", "presenceChar", function(x){

  sql <- trunc_drop(x@tempTables$domain)
  return(sql)

})


setMethod("drop_temp_tables", "targetCohort", function(x){

  sql <- trunc_drop(x@tempTable)
  return(sql)

})

setMethod("drop_temp_tables", "executionSettings", function(x){

  sql <- paste(
    trunc_drop(x@timeWindowTable),
    trunc_drop(x@codesetTable),
    sep = "\n\n"
  )
  return(sql)

})


setMethod("drop_temp_tables", "costChar", function(x){
  sql <- trunc_drop(x@tempTables$cost)
  return(sql)

})

setMethod("drop_temp_tables", "labChar", function(x){

  sql <- trunc_drop(x@tempTables$lab)
  return(sql)

})


setMethod("drop_temp_tables", "timeToChar", function(x){

  sql <- trunc_drop(x@tempTables$duration)
  return(sql)

})

setMethod("drop_temp_tables", "countChar", function(x){

  sql <- trunc_drop(x@tempTables$count)
  return(sql)

})


