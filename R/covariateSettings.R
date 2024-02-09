# Settings Functions -----------------

# build demographic settings
demographic_settings_fn <- function(type) {

  if (type == "Categorical") {
    dm <- c("Gender", "AgeGroup", "Race", "Ethnicity", "IndexYear")
  }

  if (type == "Continuous") {
    dm <- c("Age", "PriorObservationTime", "PostObservationTime", "TimeInCohort")
  }

  # make settings entry name
  dm <- purrr::map_chr(dm, ~glue::glue("useDemographics{.x}"))

  # create options list
  opts <- rep(TRUE, length(dm)) |> as.list()
  names(opts) <- dm

  #build cov settings call
  cmd <- rlang::call2(
    .fn = "createCovariateSettings",
    !!!opts,
    .ns = "FeatureExtraction"
  )

  cov_settings <- eval(cmd)

  return(cov_settings)

}

# build score settings
score_settings_fn <- function(scores) {

  # make settings entry name
  dm <- purrr::map_chr(scores, ~glue::glue("use{.x}"))

  # create options list
  opts <- rep(TRUE, length(dm)) |> as.list()
  names(opts) <- dm

  #build cov settings call
  cmd <- rlang::call2(
    .fn = "createCovariateSettings",
    !!!opts,
    .ns = "FeatureExtraction"
  )

  cov_settings <- eval(cmd)

  return(cov_settings)
}

clinical_domains <- function(domain = c("Drugs",
                                        "Conditions",
                                        "Procedures",
                                        "Measurements",
                                        "Observations",
                                        "Visits",
                                        "Labs",
                                        "DrugCount",
                                        "ConditionCount",
                                        "ProcedureCount",
                                        "ObservationCount",
                                        "MeasurementCount")) {

  domain <- match.arg(domain)
  dm <- switch(domain,
               Drugs = "DrugGroupEra",
               Conditions = "ConditionGroupEra",
               Procedures = "ProcedureOccurrence",
               Measurements = "Measurement",
               Observations = "Observation",
               Labs = "MeasurementValue",
               Visits = "VisitConceptCount",
               DrugCount = "DistinctIngredientCount",
               ConditionCount = "DistinctConditionCount",
               ProcedureCount = "DistinctProcedureCount",
               ObservationCount = "DistinctObservationCount",
               MeasurementCount = "DistinctMeasurementCount"
  )
  fe <- glue::glue("use{dm}LongTerm")
  return(fe)
}

temporal_domains <- function(domain = c("Drugs",
                                        "Conditions",
                                        "Procedures",
                                        "Measurements",
                                        "Observations",
                                        "Visits",
                                        "Labs",
                                        "DrugCount",
                                        "ConditionCount",
                                        "ProcedureCount",
                                        "ObservationCount",
                                        "MeasurementCount")) {

  domain <- match.arg(domain)
  dm <- switch(domain,
               Drugs = "DrugEraGroupStart",
               Conditions = "ConditionOccurrence",
               Procedures = "ProcedureOccurrence",
               Measurements = "Measurement",
               Observations = "Observation",
               Labs = "MeasurementValue",
               Visits = "VisitConceptCount",
               DrugCount = "DistinctIngredientCount",
               ConditionCount = "DistinctConditionCount",
               ProcedureCount = "DistinctProcedureCount",
               ObservationCount = "DistinctObservationCount",
               MeasurementCount = "DistinctMeasurementCount"
  )
  fe <- glue::glue("use{dm}")
  return(fe)
}


# build concept domain settings
domain_settings_fn <- function(domain,
                               timeA,
                               timeB,
                               exclude = c(),
                               include = c()) {

  if (length(timeA) > 1) {
    dm <- temporal_domains(domain)
    opts <- rlang::list2(
      !!dm := TRUE,
      temporalStartDays = timeA,
      temporalEndDays = timeB,
      excludedCovariateConceptIds = exclude,
      includedCovariateConceptIds = include
    )

    #build cov settings call
    cmd <- rlang::call2(
      .fn = "createTemporalCovariateSettings",
      !!!opts,
      .ns = "FeatureExtraction"
    )
  } else{
    dm <- clinical_domains(domain)
    opts <- rlang::list2(
      !!dm := TRUE,
      longTermStartDays = timeA,
      endDays = timeB,
      excludedCovariateConceptIds = exclude,
      includedCovariateConceptIds = include
    )

    #build cov settings call
    cmd <- rlang::call2(
      .fn = "createCovariateSettings",
      !!!opts,
      .ns = "FeatureExtraction"
    )
  }
  # evaluate to get cov settings
  cov_settings <- eval(cmd)

  return(cov_settings)
}


# count_domains <- function(domain = c("Drugs", "Conditions", "Procedures",
#                                      "Observations", "Measurements")) {
#
#   domain <- match.arg(domain)
#   dm <- switch(domain,
#                Drugs = "Ingredient",
#                Conditions = "Condition",
#                Procedures = "Procedure",
#                Measurements = "Measurement",
#                Observations = "Observation"
#   )
#   fe <- glue::glue("useDistinct{dm}Count")
#   return(fe)
#
# }

# build concept count domain settings
# count_settings_fn <- function(countDomain,
#                               timeA,
#                               timeB,
#                               exclude = c(),
#                               include = c()) {
#
#   dm <- count_domains(domain)
#   opts <- rlang::list2(
#     !!dm := TRUE,
#     temporalStartDays = timeA,
#     temporalEndDays = timeB,
#     excludedCovariateConceptIds = exclude,
#     includedCovariateConceptIds = include
#   )
#
#   #build cov settings call
#   cmd <- rlang::call2(
#     .fn = "createTemporalCovariateSettings",
#     !!!opts,
#     .ns = "FeatureExtraction"
#   )
#
#   cov_settings <- eval(cmd)
#
#   return(cov_settings)
# }
#
# # build visit domain settings
# visit_settings_fn <- function(timeA,
#                               timeB,
#                               exclude = c(),
#                               include = c()) {
#
#   dm <- "useVisitConceptCount"
#   opts <- rlang::list2(
#     !!dm := TRUE,
#     temporalStartDays = timeA,
#     temporalEndDays = timeB,
#     excludedCovariateConceptIds = exclude,
#     includedCovariateConceptIds = include
#   )
#
#   #build cov settings call
#   cmd <- rlang::call2(
#     .fn = "createTemporalCovariateSettings",
#     !!!opts,
#     .ns = "FeatureExtraction"
#   )
#
#   cov_settings <- eval(cmd)
#
#   return(cov_settings)
# }
#
# # build labs domain settings
# labs_settings_fn <- function(timeA,
#                               timeB,
#                               exclude = c(),
#                               include = c()) {
#
#   dm <- "useMeasurementValue"
#   opts <- rlang::list2(
#     !!dm := TRUE,
#     temporalStartDays = timeA,
#     temporalEndDays = timeB,
#     excludedCovariateConceptIds = exclude,
#     includedCovariateConceptIds = include
#   )
#
#   #build cov settings call
#   cmd <- rlang::call2(
#     .fn = "createTemporalCovariateSettings",
#     !!!opts,
#     .ns = "FeatureExtraction"
#   )
#
#   cov_settings <- eval(cmd)
#
#   return(cov_settings)
# }



