
# ClinicalCharacteristics

<!-- badges: start -->
<!-- badges: end -->

The goal of `ClinicalCharacteristics` is to characterize a patient population using OMOP data. This approach uses
a table shell approach where characteristic extraction are limited to what is specified in the build object.

## Installation

To install `ClinicalCharacteristics`, follow these steps:

1) clone the repository from the BI Bitbucket RWECODE.
2) Open the `ClinicalCharacteristics.RProj` file in the repository
3) Navigate to the build tab in RStudio and select Install
4) Exit out of the `ClinicalCharacteristics.RProj` session
5) **Recommended** create a new `RProj` to test the package.

## Example

To run this example you need to install `Capr`

``` r
library(ClinicalCharacteristics)
library(Capr)

# make connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'redshift',
  user = "<user>",
  password = "<password>",
  connectionString = "<jdbcString>"
)

connection <- DatabaseConnector::connect(connectionDetails)

clinChar <- makeClinChar(
  targetCohortIds = 2,
  targetCohortNames = "Obesity",
  dbms = "redshift",
  cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
  workDatabaseSchema = executionSettings$workDatabaseSchema,
  cohortTable = executionSettings$cohortTable
) |>
  addAgeChar(categorize = age10yrGrp()) |>
  addGenderChar() |>
  addRaceChar() |>
  addYearChar(categorize = year5yrGrp()) |>
  addLocationChar(locationTable = makeLocationTable(connection, cdmDatabaseSchema = executionSettings$cdmDatabaseSchema)) |>
  addConditionPresence(
    conceptSets = charlsonConcepts(),
    timeWindows = makeTimeTable(time_a = -365, time_b = -1),
    limit = "first",
    score = charlsonIndexScore(ageId = 1)
  ) |>
  addDrugPresence(
    conceptSets = atcConcepts(),
    timeWindows = makeTimeTable(
      time_a = c(-365, 0, 0, 0),
      time_b = c(-1, 183, 365, 730)
      ),
    limit = "first"
  ) |>
  addDrugCount(
    timeWindows = makeTimeTable(
      time_a = c(-365, 0, 0, 0),
      time_b = c(-1, 183, 365, 730)
    )
  ) |>
  addVisitCount(
    conceptSets = standardVisitConcepts(),
    timeWindows = makeTimeTable(
      time_a = c(-365, 0, 0, 0),
      time_b = c(-1, 183, 365, 730)
    )
  )


res <- runClinicalCharacteristics(connection = connection, clinChar = clinChar)
previewClincalCharacteristics(res, type = "categorical")
previewClincalCharacteristics(res, type = "continuous")

DatabaseConnector::disconnect(con)


```

