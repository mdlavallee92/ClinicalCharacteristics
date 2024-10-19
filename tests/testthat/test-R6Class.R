library(testthat)
testthat::local_edition(3)

# test creating a TableShell object
test_that("TableShell object is initialized correctly", {
  targetCohort <- CohortInfo$new(id = 1, name = "Cohort 1")
  stat <- Statistic$new(type = "test")
  lineItem <- LineItem$new(name = "test",
                           definitionType = "test",
                           statistic = stat)
  tableShell <- TableShell$new(name = "Table 1", 
                               targetCohorts = list(targetCohort), 
                               lineItems = list(lineItem))

  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getName(), "Table 1")
  expect_equal(length(tableShell$getTargetCohorts()), 1)
})

test_that("CohortInfo object creates", {
  # create a CohortInfo object
  ci <- CohortInfo$new(1, "Test Name")
  expect_true(inherits(ci, "CohortInfo"), info = "ci should be a CohortInfo object")
  expect_equal(ci$getId(), 1)
  expect_equal(ci$getName(), "Test Name")
})

test_that("ExecutionSettings object creates", {
  # create an ExecutionSettings object
  es <- ExecutionSettings$new(connectionDetails = connectionDetailsEunomia,
                              connection = NULL,
                              cdmDatabaseSchema = "fake_cdm_database_schema",
                              workDatabaseSchema = "fake_work_database_schema",
                              tempEmulationSchema = "fake_temp_emulation_schema",
                              targetCohortTable = "fake_target_cohort_table",
                              cdmSourceName = "fake_cdm_source_name")
  expect_true(inherits(es, "ExecutionSettings"), info = "es should be an ExecutionSettings object")
})

test_that("ConceptSetLineItem object initializes correctly", {
  conceptSet <- ConceptSetLineItem$new(name = "concept_set_name", 
                                       statistic = Statistic$new("statistic_type"), 
                                       domain = "domain",
                                       conceptSet = Capr::cs(1335471, name = "test"), 
                                       timeInterval = TimeInterval$new(0,365),
                                       sourceConceptSet = Capr::cs(1335471, name = "test_source"), 
                                       typeConceptIds = c(67890), 
                                       visitOccurrenceConceptIds = c(12345, 67890))
  expect_true(inherits(conceptSet, "ConceptSetLineItem"))
  expect_true(inherits(conceptSet, "LineItem"))
})

# Test CohortLineItem Initialization
test_that("CohortLineItem initializes correctly", {
  cohortInfo <- CohortInfo$new(id = 1, name = "Test Cohort")
  timeInterval <- TimeInterval$new(0, 365)
  statistic <- Statistic$new(type = "Test Stat")
  cohortLi <- CohortLineItem$new(name = "Test Line Item", 
                                 statistic = statistic, 
                                 cohort = cohortInfo, 
                                 timeInterval = timeInterval)
  
  expect_true(inherits(cohortLi, "CohortLineItem"))
  expect_true(inherits(cohortLi, "LineItem"))
  expect_equal(cohortLi$grabCohort(), cohortInfo)
  expect_equal(cohortLi$getTimeInterval(), timeInterval$getTimeInterval())
})

test_that("Presence object initializes correctly", {
  presence <- Presence$new("operator", 2)
  expect_true(inherits(presence, "Presence"))
  expect_true(inherits(presence, "Statistic"))
})

# Test TableShell Initialization
test_that("TableShell initializes correctly", {
  cohort_info <- CohortInfo$new(id = 1, name = "Test Cohort")
  line_item <- LineItem$new(name = "Test Line Item", definitionType = "Test Type", statistic = Statistic$new(type = "Test Stat"))
  table_shell <- TableShell$new(name = "Test Table", targetCohorts = list(cohort_info), lineItems = list(line_item))
  
  expect_equal(table_shell$getName(), "Test Table")
  expect_equal(length(table_shell$getTargetCohorts()), 1)
  expect_equal(length(table_shell$getLineItems()), 1)
})
