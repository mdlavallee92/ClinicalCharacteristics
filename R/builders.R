# Builders --------------


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
                                    minValue = 10,
                                    outputFolder) {
  ii <- seq_along(timeA)
  time_print <- glue::glue("t{ii}: {timeA}_{timeB}") |> paste(collapse = ", ")
  consule_txt <- glue::glue("Build {domain} Covariates")
  cli::cat_rule(consule_txt)
  cli::cat_bullet(
    glue::glue("At time points: {time_print}"),
    bullet = "info",
    bullet_col = "blue"
  )
  # get cov_settings
  covSettings <- domain_settings_fn(domain = domain,
                                    timeA = timeA,
                                    timeB = timeB,
                                    include = includeConcepts,
                                    exclude = excludeConcepts)

  #run FE
  cov <- FeatureExtraction::getDbCovariateData(connection = con,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortDatabaseSchema = cohortDatabaseSchema,
                                               cohortId = cohortIds,
                                               covariateSettings = covSettings,
                                               aggregated = TRUE)

  if (domain %in% c("Drugs", "Conditions", "Procedures", "Measurements", "Observations")) {
    # collect categorical covariates
    tbl <- cov$covariates |>
      dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
      dplyr::select(
        cohortDefinitionId, analysisId, timeId,
        covariateId, covariateName,
        sumValue, averageValue) |>
      dplyr::collect() |>
      dplyr::mutate(
        covariateName = gsub(".*: ", "", covariateName)
      ) |>
      dplyr::filter(
        sumValue >= minValue # remove observations with too small of count
      )
  } else{
    # collect continuous covariates
    tbl <- cov$covariatesContinuous |>
      dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
      dplyr::select(
        cohortDefinitionId, analysisId,
        covariateId, covariateName, conceptId,
        countValue,
        averageValue, standardDeviation,
        minValue, p10Value, p25Value,
        medianValue, p75Value, p90Value, maxValue) |>
      dplyr::collect() |>
      dplyr::mutate(
        covariateName = gsub(".*: ", "", covariateName) # remove the junk in covariate name
      ) |>
      dplyr::filter(
        countValue >= minValue # remove observations with too small of count
      )
  }

  # save output
  verboseSave(
    object = tbl,
    saveName = glue::glue("{domain}"),
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
  cov <- FeatureExtraction::getDbCovariateData(connection = con,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortDatabaseSchema = cohortDatabaseSchema,
                                               cohortId = cohortIds,
                                               covariateSettings = covSettings,
                                               aggregated = TRUE)

  if (type %in% c("Categorical")) {
    # collect categorical covariates
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

  } else{
    # collect continuous covariates
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

  }

  # save output
  verboseSave(
    object = tbl,
    saveName = glue::glue("{type}_demographics"),
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
  cov <- FeatureExtraction::getDbCovariateData(connection = con,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortDatabaseSchema = cohortDatabaseSchema,
                                               cohortId = cohortIds,
                                               covariateSettings = covSettings,
                                               aggregated = TRUE)
  # collect continuous covariates
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

  # save output
  verboseSave(
    object = tbl,
    saveName = glue::glue("covariate_scores"),
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


  ii <- seq_along(timeA)
  time_print <- glue::glue("t{ii}: {timeA}_{timeB}") |> paste(collapse = ", ")
  consule_txt <- glue::glue("Build Cohort Covariates")
  cli::cat_rule(consule_txt)
  cli::cat_bullet(
    glue::glue("At time points: {time_print}"),
    bullet = "info",
    bullet_col = "blue"
  )

  # get cov_settings
  covSettings <- FeatureExtraction::createCohortBasedTemporalCovariateSettings(
    analysisId = 999,
    covariateCohortDatabaseSchema = cohortDatabaseSchema,
    covariateCohortTable = cohortTable,
    covariateCohorts = covariateCohorts,
    valueType = "binary",
    temporalStartDays = timeA,
    temporalEndDays = timeB,
    includedCovariateIds = c(),
    warnOnAnalysisIdOverlap = TRUE
  )

  #run FE
  cov <- FeatureExtraction::getDbCohortBasedCovariatesData(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortId = targetCohortIds,
    covariateSettings = covSettings,
    aggregated = TRUE
  )

  tbl <- cov$covariates |>
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
    dplyr::select(
      cohortDefinitionId, analysisId, timeId,
      covariateId, covariateName,
      sumValue, averageValue) |>
    dplyr::collect() |>
    dplyr::mutate(
      covariateName = gsub(".*: ", "", covariateName)
    )

  # save output
  verboseSave(
    object = tbl,
    saveName = glue::glue("cohort_covariates"),
    saveLocation = outputFolder
  )

  invisible(tbl)

}
#
# build_count_covariates <- function(con,
#                                    cohortDatabaseSchema,
#                                    cohortTable,
#                                    cdmDatabaseSchema,
#                                    cohortIds,
#                                    countDomain,
#                                    includeConcepts = c(),
#                                    excludeConcepts = c(),
#                                    timeA,
#                                    timeB,
#                                    outputFolder) {
#
#   consule_txt <- glue::glue("Build {countDomain} Covariates at time {timeA} : {timeB}")
#   cli::cat_rule(consule_txt)
#
#   # get cov_settings
#   covSettings <- count_settings_fn(countDomain = countDomain,
#                                     timeA = timeA,
#                                     timeB = timeB,
#                                     include = includeConcepts,
#                                     exclude = excludeConcepts)
#
#   #run FE
#   cov <- silentCovariates(con = con,
#                           cdmDatabaseSchema = cdmDatabaseSchema,
#                           cohortTable = cohortTable,
#                           cohortDatabaseSchema = cohortDatabaseSchema,
#                           cohortId = cohortIds,
#                           covSettings = covSettings)
#
#
#   tbl <- collect_cts(cov)
#
#
#   # save output
#   saveName <- glue::glue("{countDomain}_count_{abs(timeA)}_{abs(timeB)}")
#
#   verboseSave(
#     object = tbl,
#     saveName = saveName,
#     saveLocation = outputFolder
#   )
#
#   invisible(tbl)
#
#
# }


## Collect covariates from FE --------------------

### Categorical covariates
# collect_cat <- function(cov) {
#
#   tbl <- cov$covariates |>
#     dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
#     dplyr::select(
#       cohortDefinitionId, analysisId, timeId,
#       covariateId, covariateName,
#       sumValue, averageValue) |>
#     dplyr::collect() |>
#     dplyr::mutate(
#       covariateName = gsub(".*: ", "", covariateName)
#     )
#
#   return(tbl)
#
# }
#
# ### Continuous covariates
#
# collect_cts <- function(cov) {
#
#   tbl <- cov$covariatesContinuous |>
#     dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
#     dplyr::select(
#       cohortDefinitionId, analysisId,
#       covariateId, covariateName, conceptId,
#       countValue,
#       averageValue, standardDeviation,
#       minValue, p10Value, p25Value,
#       medianValue, p75Value, p90Value, maxValue) |>
#     dplyr::collect()
#
#   return(tbl)
# }

