library(testthat)

test_that("createTableShell returns a TableShell object with the correct name", {
  name <- "Table 1"
  targetCohort <- createTargetCohort(id = 1, name = "TC")
  lineItem <- LineItem$new("Line",1,"def",Statistic$new("stat"))
  section <- createSection(name = "Section 1", ordinal = 1, lineItems = list(lineItem))
  es <- ExecutionSettings$new(connectionDetails = "fake_connection_details",
                              connection = NULL,
                              cdmDatabaseSchema = "fake_cdm_database_schema",
                              workDatabaseSchema = "fake_work_database_schema",
                              tempEmulationSchema = "fake_temp_emulation_schema",
                              targetCohortTable = "fake_target_cohort_table",
                              cdmSourceName = "fake_cdm_source_name",
                              numThreads = 4)
  tableShell <- createTableShell(name = "Table 1", sections = list(section), targetCohorts = list(targetCohort), executionSettings = es)

  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getName(), name)
})

test_that("addTargetCohortsFromDf adds target cohorts from a data frame", {
  df <- data.frame(id = c(1, 2), name = c("Cohort 1", "Cohort 2"))
  cohorts <- parseTargetCohortsFromDf(df)
  expect_true(all(sapply(cohorts, inherits, "TargetCohort")))
})

test_that("createTargetCohort returns a TargetCohort object with the correct attributes", {
  id <- 1
  name <- "Test Cohort"
  targetCohort <- createTargetCohort(id, name)
  expect_true(inherits(targetCohort, "TargetCohort"))
  expect_equal(targetCohort$getId(), id)
  expect_equal(targetCohort$getName(), name)
})

test_that("createExecutionSettings returns an ExecutionSettings object", {
  executionSettings <- createExecutionSettings(connectionDetails = "fake_connection_details",
                                              connection = NULL,
                                              cdmDatabaseSchema = "cdm_schema",
                                              workDatabaseSchema = "work_schema",
                                              tempEmulationSchema = "fake_temp_emulation_schema",
                                              targetCohortTable = "fake_target_cohort_table",
                                              cdmSourceName = "fake_cdm_source_name",
                                              numThreads = 4)
  expect_true(inherits(executionSettings, "ExecutionSettings"))
})

test_that("createSection returns a Section object with the correct attributes", {
  section <- createSection("Test Section", 1, list(LineItem$new("Line",1,"def",Statistic$new("stat"))))
  expect_true(inherits(section, "Section"))
  expect_equal(section$getName(), "Test Section")
  expect_equal(section$getOrdinal(), 1)
})

test_that("createConceptSetLineItem creates a ConceptSetLineItem object", {
  csDefinition <- createConceptSetLineItem(name = "Concept Set 1",
                                           ordinal = 1,
                                           statistic = Presence$new("equal", 1),
                                           conceptSet = Capr::cs(1335471, name = "test"),
                                           domain = "Drug")
  expect_true(inherits(csDefinition, "ConceptSetLineItem"))
  expect_true(inherits(csDefinition, "LineItem"))
  expect_equal(csDefinition$getDefinitionType(), "conceptSet")
})

test_that("createConceptSetLineItemBatch creates a list of ConceptSetLineItem objects", {
  conceptSets <- list(Capr::cs(1335471, name = "A"), Capr::cs(1340128, name = "B"), Capr::cs(1341927, name = "C"))
  csDefinitions <- createConceptSetLineItemBatch(statistic = Presence$new("equal", 1),
                                                 conceptSets = conceptSets,
                                                 domain = "Drug")
  expect_equal(length(csDefinitions), 3)
  expect_true(inherits(csDefinitions[[1]], "ConceptSetLineItem"))
  expect_true(inherits(csDefinitions[[1]], "LineItem"))
  expect_equal(csDefinitions[[1]]$getName(), "A")
  expect_equal(csDefinitions[[1]]$getOrdinal(), 1)
})
