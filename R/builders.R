# Builders --------------


## Collect covariates from FE --------------------

### Categorical covariates
collect_cat <- function(cov) {

  tbl <- cov$covariates |>
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
    dplyr::select(
      cohortDefinitionId, analysisId,
      covariateId, covariateName,
      sumValue, averageValue) |>
    dplyr::collect() |>
    dplyr::mutate(
      covariateName = gsub(".*: ", "", covariateName)
    )

  return(tbl)

}

### Continuous covariates

collect_cts <- function(cov) {

  tbl <- cov$covariatesContinuous |>
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
    dplyr::select(
      cohortDefinitionId, analysisId,
      covariateId, covariateName, conceptId,
      countValue,
      averageValue, standardDeviation,
      minValue, p10Value, p25Value,
      medianValue, p75Value, p90Value, maxValue) |>
    dplyr::collect()

  return(tbl)
}




## Run FE ---------------------

# function to build clinical covariates based on domains
build_domain_covariates <- function(con,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    cdmDatabaseSchema,
                                    cohortIds,
                                    domain,
                                    includeConcepts = c(),
                                    excludeConcepts = c(),
                                    timeA,
                                    timeB,
                                    outputFolder) {

  consule_txt <- glue::glue("Build {domain} Covariates at time {timeA} : {timeB}")
  cli::cat_rule(consule_txt)

  # get cov_settings
  covSettings <- domain_settings_fn(domain = domain,
                                    timeA = timeA,
                                    timeB = timeB,
                                    include = includeConcepts,
                                    exclude = excludeConcepts)

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortIds,
                          covSettings = covSettings)

  if (domain %in% c("Drugs", "Conditions", "Procedures")) {
    tbl <- collect_cat(cov)
  } else{
    tbl <- collect_cts(cov)
  }

  # save output
  saveName <- glue::glue("{domain}_{abs(timeA)}_{abs(timeB)}")

  verboseSave(
    object = tbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(tbl)

}

# function to build clinical covariates based on demographics
build_demographic_covariates <- function(con,
                                         cohortDatabaseSchema,
                                         cohortTable,
                                         cdmDatabaseSchema,
                                         cohortIds,
                                         type,
                                         outputFolder) {

  consule_txt <- glue::glue("Build {type} Demographics")
  cli::cat_rule(consule_txt)

  # get cov_settings
  covSettings <- demographic_settings_fn(type = type)

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortIds,
                          covSettings = covSettings)

  if (type %in% c("Categorical")) {
    tbl <- collect_cat(cov)
  } else{
    tbl <- collect_cts(cov)
  }

  # save output
  saveName <- glue::glue("{type}_demographics")

  verboseSave(
    object = tbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(tbl)

}

# function to build clinical covariates based on scores
build_score_covariates <- function(con,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   cdmDatabaseSchema,
                                   cohortIds,
                                   scores,
                                   outputFolder) {

  consule_txt <- glue::glue("Build Covariate Scores")
  cli::cat_rule(consule_txt)

  # get cov_settings
  covSettings <- score_settings_fn(scores = scores)

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortIds,
                          covSettings = covSettings)

  tbl <- collect_cts(cov)

  # save output
  saveName <- glue::glue("covariate_scores")

  verboseSave(
    object = tbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(tbl)

}

# function to build clinical covariates based on cohorts
build_cohort_covariates <- function(con,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   cdmDatabaseSchema,
                                   targetCohortIds,
                                   covariateCohorts,
                                   timeA,
                                   timeB,
                                   outputFolder) {

  consule_txt <- glue::glue("Build Cohort Covariates at time {timeA} : {timeB}")
  cli::cat_rule(consule_txt)

  # get cov_settings
  covSettings <- FeatureExtraction::createCohortBasedCovariateSettings(
    analysisId = 999,
    covariateCohortDatabaseSchema = cohortDatabaseSchema,
    covariateCohortTable = cohortTable,
    covariateCohorts = covariateCohorts,
    valueType = "binary",
    startDay = timeA,
    endDay = timeB,
    includedCovariateIds = c(),
    warnOnAnalysisIdOverlap = TRUE
  )

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = targetCohortIds,
                          covSettings = covSettings)

  tbl <- collect_cat(cov)

  # save output
  saveName <- glue::glue("cohort_{abs(timeA)}_{abs(timeB)}")

  verboseSave(
    object = tbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(tbl)

}
