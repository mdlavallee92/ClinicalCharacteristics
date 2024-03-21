
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


# make concept sets
conceptSet <- list(cs(descendants(201826), name = "T2D"),
                   cs(descendants(313217,314665), name = "af")
)

cs2 <- list(
  cs(descendants(1503297), name = "metformin"),
  cs(descendants(1310149), name = "warfarin")
)

# build clinChar object
clinChar <- makeClinChar(
    targetCohortIds = 1,
    targetCohortNames = "Obesity",
    dbms = "redshift",
    cdmDatabaseSchema = "cdm_data",
    workDatabaseSchema = "my_scratch",
    cohortTable = "cohort_table"
  ) |>
  addAgeChar(categorize = age10yrGrp()) |>
  addRaceChar() |>
  addGenderChar() |>
  addLocationChar() |>
  addConditionPresence(
    conceptSets = conceptSet, 
    timeWindows = makeTimeTable(time_a = -365, time_b = -1), 
    limit = "first") |>
  addLabChar(
    labIds = c(3036277), 
    unitIds = c(8582), 
    timeWindows = makeTimeTable(time_a = -365, time_b = -1), 
    limit = "last") |>
  addDrugCount(timeWindows = makeTimeTable(time_a = -365, time_b = -1)) |>
  addDrugCost(timeWindows = makeTimeTable(time_a = -365, time_b = -1)) |>
  addTimeToDrug(conceptSets = cs2, timeWindows = makeTimeTable(time_a = c(0, 0), time_b = c(180, 365)))

runClinicalCharacteristics(connection = connection, clinChar = clinChar)


tb <- tabulateClinicalCharacteristics(clinChar = clinChar)

previewClincalCharacteristics(tb, type = "categorical")
previewClincalCharacteristics(tb, type = "continuous")

DatabaseConnector::disconnect(con)


```

