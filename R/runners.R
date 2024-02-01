# Runners ------------------

# functions that execute what we need done
#' Function that runs the clinical characterization analysis
#' @param con a connection to a dbms hosting OMOP data
#' @param executionSettings a list of settings used to specify aspects of the connections
#' @param analysisSettings a list of settings specifying the details of the analysis and how the
#' characterization should be done
#' @param outputFolder a path to a folder specifying where to save the results
#' @return no object return, this function will save csv files of the characterization results
#' @export
runClinicalCharacteristics <- function(con,
                                  executionSettings,
                                  analysisSettings,
                                  outputFolder) {

  cli::cat_boxx("RUN CLINICAL CHARACTERISTICS")

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable

  #get global analysis settings
  targetCohortIds <- analysisSettings$ClinicalCovariates$targetCohortIds
  settings <- analysisSettings$ClinicalCovariates$settings

  if (checkMode(settings, mode = "Demographics")) {
    cli::cat_bullet("Run Demographics", bullet = "cross", bullet_col = "yellow")

    #unpack demographics
    demographics <- settings$Demographics$type

    #run builder
    purrr::walk(
      demographics,
      ~build_demographic_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        type = .x,
        outputFolder = outputFolder)
    )
  }

  if (checkMode(settings, mode = "Scores")) {
    cli::cat_bullet("Run Scores", bullet = "cross", bullet_col = "yellow")

    #unpack demographics
    scores <- settings$Scores$scores

    #run builder
    build_score_covariates(
      con = con,
      cohortDatabaseSchema = workDatabaseSchema,
      cohortTable = cohortTable,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortIds = targetCohortIds,
      scores = scores,
      outputFolder = outputFolder)
  }

  if (checkMode(settings, mode = "Cohort")) {
    cli::cat_bullet("Run Cohort Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    covariateCohort <- tibble::tibble(
      cohortId = settings$Cohort$covariates$cohortId,
      cohortName = settings$Cohort$covariates$cohortName
    )
    timeA <- settings$Cohort$windows$timeA
    timeB <- settings$Cohort$windows$timeB

    #run builder
    purrr::walk2(
      timeA,
      timeB,
      ~build_cohort_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        targetCohortIds = targetCohortIds,
        covariateCohorts = covariateCohort,
        timeA = .x,
        timeB = .y,
        outputFolder = outputFolder)
    )
  }
  ###########################
  # Domains
  ############################
  # TODO can this be a bit more elegent
  if (checkMode(settings, mode = "Drugs")) {
    cli::cat_bullet("Run Drugs Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    domain <- "Drugs"
    includeConcepts <- settings$Drugs$include
    excludeConcepts <- settings$Drugs$exclude
    timeA <- settings$Drugs$windows$timeA
    timeB <- settings$Drugs$windows$timeB

    #run builder
    purrr::walk2(
      timeA,
      timeB,
      ~build_domain_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        domain = domain,
        includeConcepts = includeConcepts,
        excludeConcepts = excludeConcepts,
        timeA = .x,
        timeB = .y,
        outputFolder = outputFolder)
    )
  }

  if (checkMode(settings, mode = "Conditions")) {
    cli::cat_bullet("Run Conditions Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    domain <- "Conditions"
    includeConcepts <- settings$Conditions$include
    excludeConcepts <- settings$Conditions$exclude
    timeA <- settings$Conditions$windows$timeA
    timeB <- settings$Conditions$windows$timeB

    #run builder
    purrr::walk2(
      timeA,
      timeB,
      ~build_domain_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        domain = domain,
        includeConcepts = includeConcepts,
        excludeConcepts = excludeConcepts,
        timeA = .x,
        timeB = .y,
        outputFolder = outputFolder)
    )
  }

  if (checkMode(settings, mode = "Procedures")) {
    cli::cat_bullet("Run Procedures Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    domain <- "Procedures"
    includeConcepts <- settings$Procedures$include
    excludeConcepts <- settings$Procedures$exclude
    timeA <- settings$Procedures$windows$timeA
    timeB <- settings$Procedures$windows$timeB

    #run builder
    purrr::walk2(
      timeA,
      timeB,
      ~build_domain_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        domain = domain,
        includeConcepts = includeConcepts,
        excludeConcepts = excludeConcepts,
        timeA = .x,
        timeB = .y,
        outputFolder = outputFolder)
    )
  }

  if (checkMode(settings, mode = "Visits")) {
    cli::cat_bullet("Run Visits Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    domain <- "Visits"
    includeConcepts <- settings$Visits$include
    excludeConcepts <- settings$Visits$exclude
    timeA <- settings$Visits$windows$timeA
    timeB <- settings$Visits$windows$timeB

    #run builder
    purrr::walk2(
      timeA,
      timeB,
      ~build_domain_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        domain = domain,
        includeConcepts = includeConcepts,
        excludeConcepts = excludeConcepts,
        timeA = .x,
        timeB = .y,
        outputFolder = outputFolder)
    )
  }

  if (checkMode(settings, mode = "Measurements")) {
    cli::cat_bullet("Run Measurements Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    domain <- "Measurements"
    includeConcepts <- settings$Measurements$include
    excludeConcepts <- settings$Measurements$exclude
    timeA <- settings$Measurements$windows$timeA
    timeB <- settings$Measurements$windows$timeB

    #run builder
    purrr::walk2(
      timeA,
      timeB,
      ~build_domain_covariates(
        con = con,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        domain = domain,
        includeConcepts = includeConcepts,
        excludeConcepts = excludeConcepts,
        timeA = .x,
        timeB = .y,
        outputFolder = outputFolder)
    )
  }


  invisible(targetCohortIds)
}


