# Analysis Settings builders --------------

#' Make demographics settings
#' @param type the type of demographic covariates to produce, default Categorical and Continuous
#' @return list of settings
#' @export
demographicSettings <- function(type = c("Categorical", "Continuous")) {

  list(
    'Demographics' = list(
      'type' = type
    )
  )

}

#' Make score settings
#' @param scores the score covariates to produce, default CharlsonIndex, DCsi, and Chads2Vasc
#' @return list of settings
#' @export
scoreSettings <- function(scores = c("CharlsonIndex", "Dcsi", "Chads2Vasc")) {
  list(
    'Scores' = list(
      'scores' = scores
    )
  )
}

#' Make domain settings
#' @param domain the concept domain to use. Can be Drugs, Conditions, Procedures, Measurements or Visits
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings
#' @export
domainSettings <- function(domain, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {

  rlang::list2(
    !!domain := list(
      'windows' = list(
        'timeA' = timeA,
        'timeB' = timeB
      ),
      'include' = includeConcepts,
      'exclude' = excludeConcepts
    )
  )

}

#' Make cohort settings
#' @param cohortId the ids of the cohorts in the initialized cohort table to use as covariates
#' @param cohortName the names of the cohorts corresponding to their id
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @return list of settings
#' @export
cohortSettings <- function(cohortId, cohortName, timeA, timeB) {

  list(
    'Cohort' = list(
      'covariates' = list(
        'cohortId' = cohortId,
        'cohortName' = cohortName
        ),
      'windows' = list(
        'timeA' = timeA,
        'timeB' = timeB
      )
    )

  )
}

#' Define clinical characteristics settings
#' @param targetCohortIds the target cohorts to characterize
#' @param ... a set of settings to define
#' @return a list object containing the analysis settings
#' @export
defineClinicalCharacteristics <- function(targetCohortIds, ...) {

  ll <- list(
    'ClinicalCovariates' = list(
      'targetCohortIds' = targetCohortIds,
      'settings' = purrr::flatten(list(...))
    )
  )

  return(ll)

}



