
# ClinicalCharacteristics

<!-- badges: start -->
<!-- badges: end -->

The goal of `ClinicalCharacteristics` is to provide a module to improve the interface with the Feature Extraction package from OHDSI. This R package allows you to specify the clinical characteristics you choose to build at specified time intervals and generates them as csv files in an output folder. More features to come....

## Installation

To install `ClinicalCharacteristics`, follow these steps:

1) clone the repository from the BI Bitbucket RWECODE.
2) Open the `ClinicalCharacteristics.RProj` file in the repository
3) Navigate to the build tab in RStudio and select Install
4) Exit out of the `ClinicalCharacteristics.RProj` session
5) **Recommended** create a new `RProj` to test the package.

## Example

To run this package you need to instantiate cohorts in a dbms. A function is provided to facilitate this. 
You will need connection information and information about your schemas specifying save points for tables.


``` r
library(ClinicalCharacteristics)

# make connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'redshift',
  user = "<user>",
  password = "<password>",
  connectionString = "<jdbcString>"
)

# make execution Settings
executionSettings <- list(
  cdmDatabaseSchema = '<cdm>',
  workDatabaseSchema = '<scratch_user>',
  cohortTable = 'test_cc'
)

# make an output folder
outputFolder <- here::here() |>
  fs::path("output") |>
  fs::dir_create()

# use these analysis settings
targetCohortIds <- 1:3
covariateCohort <- tibble::tibble(
  cohortId = 4:14,
  cohortName = c("ace", "afib", "arb",
                 "dialysis", "hf", "hyperglycemia",
                 "hypertension", "nash",
                 "pad", "sglt2", "stroke")
)


analysisSettings <- defineClinicalCovariatesSettings(
  targetCohortIds = c(1:3),
  demographics = demographicSettings(),
  scores = scoreSettings(),
  drugs = domainSettings(
    domain = 'Drugs',
    timeA = c(-365, 0, 1, 1, 1),
    timeB = c(-1, 0, 183, 365, 730),
    excludeConcepts = c(21600001, 21600959, 21601237, # Remove ATC 1st class
                21601907, 21602359, 21602681,
                21602795, 21601386, 21603931,
                21604180, 21604847, 21605007,
                21603550, 21605212)
    ),
  conditions = domainSettings(
    domain = 'Conditions',
    timeA = c(-365),
    timeB = c(-1)
  ),
  measurements = domainSettings(
    domain = "Measurements",
    timeA = c(-365, 0, 1, 1, 1),
    timeB = c(-1, 0, 183, 365, 730),
    includeConcepts = c(
      4146380, 3006923, # ALT
      4263457, 3013721, # AST
      4004235, # CDT
      4182052, 4156660, #fasting glucose
      4197971, # HbA1c
      3038553, # BMI
      4154790, 3012888, # DBP
      4152194, 3004249, #SBP
      4012479, #LDL
      4101713, #HDL
      4008265 #total cholesterol
    )
  ),
  cohorts = cohortSettings(
    cohortId = covariateCohort$cohortId,
    cohortName = covariateCohort$cohortName,
    timeA = c(-365, 0, 1, 1, 1),
    timeB = c(-1, 0, 183, 365, 730)
  )
)

# Establish connection

con <- DatabaseConnector::connect(connectionDetails)

# Instantiate Scenario
instantiateExample(
  connectionDetails = connectionDetails,
  executionSettings = executionSettings,
  outputFolder
)

# run Clinical Characteristics
runClinicalCharacteristics(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings,
  outputFolder = outputFolder
)

DatabaseConnector::disconnect(con)


```

