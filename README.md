# ClinicalCharacteristicsOldOld

<!-- badges: start -->
<!-- badges: end -->

The goal of `ClinicalCharacteristicsOld` is to characterize a patient population using OMOP data. This approach uses
a table shell approach where characteristic extraction are limited to what is specified in the build object.

## Installation

To install `ClinicalCharacteristicsOld`, follow these steps:

1) clone the repository 
2) Open the `ClinicalCharacteristicsOld.RProj` file in the repository
3) Navigate to the build tab in RStudio and select Install
4) Exit out of the `ClinicalCharacteristicsOld.RProj` session
5) **Recommended** create a new `RProj` to test the package.
6) Use CohortGenerator to build a cohort def in scratch

## Example

To run this example you need to install `Capr`

``` r
library(ClinicalCharacteristicsOld)
library(Capr)

# make connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = '<dbms>',
  user = "<user>",
  password = "<password>",
  connectionString = "<jdbcString>"
)

executionSettings <- list(
  databaseName = "my_omop_data",
  cdmDatabaseSchema = "cdm",
  workDatabaseSchema = "scratch",
  cohortTable = "cohorts"
)

outputFolder <- here::here("my_output")

connection <- DatabaseConnector::connect(connectionDetails)

clinChar <- makeClinChar(
  targetCohortIds = 1,
  targetCohortNames = "Target",
  dbms = connection@dbms,
  database = executionSettings$databaseName,
  cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
  workDatabaseSchema = executionSettings$workDatabaseSchema,
  cohortTable = executionSettings$cohortTable
) |>
  addAgeChar(categorize = age10yrGrp()) |>
  addGenderChar() |>
  addRaceChar() |>
  addYearChar(categorize = year5yrGrp()) |>
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


# run the clinical characteristics
dt <- runClinicalCharacteristicsOld(connection = connection,
                                      clinChar = clinChar,
                                      saveName = "internals_test",
                                      savePath = outputFolder,
                                      dropDat = FALSE)

# preview the categorical results
previewClincalCharacteristics(dt, type = "categorical")
# preview the continuous results
previewClincalCharacteristics(dt, type = "continuous")
#build a report
createReport(clinChar, outputFolder = outputFolder)


```

