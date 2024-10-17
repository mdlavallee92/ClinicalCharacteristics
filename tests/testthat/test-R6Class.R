library(testthat)
testthat::local_edition(3)

# test creating a TableShell object
test_that("TableShell object is initialized correctly", {
  targetCohort <- TargetCohort$new(id = 1, name = "Cohort 1")
  section <- Section$new(name = "Section 1", ordinal = 1, lineItems = list(LineItem$new("Line",1,"def",Statistic$new("stat"))))
  es <- ExecutionSettings$new(connectionDetails = "fake_connection_details",
              connection = NULL,
              cdmDatabaseSchema = "fake_cdm_database_schema",
              workDatabaseSchema = "fake_work_database_schema",
              tempEmulationSchema = "fake_temp_emulation_schema",
              targetCohortTable = "fake_target_cohort_table",
              cdmSourceName = "fake_cdm_source_name",
              numThreads = 4)

  tableShell <- TableShell$new(name = "Table 1", sections = list(section), targetCohorts = list(targetCohort), executionSettings = es)

  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getName(), "Table 1")
  expect_equal(length(tableShell$getSections()), 1)
  expect_equal(length(tableShell$getTargetCohorts()), 1)
})

test_that("TargetCohort object creates", {
  # create a TargetCohort object
  tc <- TargetCohort$new(1, "Test Name")
  expect_true(inherits(tc, "TargetCohort"), info = "tc should be a TargetCohort object")
  expect_equal(tc$getId(), 1)
  expect_equal(tc$getName(), "Test Name")
})

test_that("Section object functions correctly", {
  # Create a Section object
  section <- Section$new(name = "Test Section", ordinal = 1, lineItems = list(LineItem$new("Line",1,"def",Statistic$new("stat"))))

  expect_true(inherits(section, "Section"))
  # Test the getName() function
  expect_equal(section$getName(), "Test Section")

  # Test the getOrdinal() function
  expect_equal(section$getOrdinal(), 1)

  # Test the getLineItems() function
  expect_length(section$getLineItems(), 1)
})

test_that("ExecutionSettings object creates", {
  # create an ExecutionSettings object
  es <- ExecutionSettings$new(connectionDetails = "fake_connection_details",
                              connection = NULL,
                              cdmDatabaseSchema = "fake_cdm_database_schema",
                              workDatabaseSchema = "fake_work_database_schema",
                              tempEmulationSchema = "fake_temp_emulation_schema",
                              targetCohortTable = "fake_target_cohort_table",
                              cdmSourceName = "fake_cdm_source_name",
                              numThreads = 4)
  expect_true(inherits(es, "ExecutionSettings"), info = "es should be an ExecutionSettings object")
})

test_that("ConceptSetLineItem object initializes correctly", {
  conceptSet <- ConceptSetLineItem$new("concept_set_name", 1, Statistic$new("statistic_type"), Capr::cs(1335471, name = "test"), "domain", Capr::cs(1335471, name = "test_source"), c(67890), c(12345, 67890))
  expect_true(inherits(conceptSet, "ConceptSetLineItem"))
  expect_true(inherits(conceptSet, "LineItem"))
})

test_that("Presence object initializes correctly", {
  presence <- Presence$new("operator", 2)
  expect_true(inherits(presence, "Presence"))
  expect_true(inherits(presence, "Statistic"))
})
