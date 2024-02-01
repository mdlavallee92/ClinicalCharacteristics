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

clinical_domains <- function(domain = c("Drugs", "Conditions", "Procedures", "Measurements", "Visits")) {
  
  domain <- match.arg(domain)
  dm <- switch(domain,
               Drugs = "DrugGroupEra",
               Conditions = "ConditionGroupEra",
               Procedures = "ProcedureOccurrence",
               Measurements = "MeasurementValue",
               Visits = "VisitConceptCount"
  )
  fe <- glue::glue("use{dm}LongTerm")
  return(fe)
  
}

# build concept domain settings
domain_settings_fn <- function(domain, timeA, timeB,
                               exclude = c(),
                               include = c()) {
  
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