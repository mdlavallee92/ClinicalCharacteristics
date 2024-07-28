library(testthat)

test_that("createTableShell returns a TableShell object with the correct title", {
  title <- "Test TableShell"
  tableShell <- createTableShell(title)
  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getTitle(), title)
})

test_that("addTargetCohorts sets the target cohorts correctly", {
  tableShell <- createTableShell("Test TableShell")
  targetCohorts <- list(
    createTargetCohort(1, "Cohort 1"),
    createTargetCohort(2, "Cohort 2")
  )
  tableShell <- addTargetCohorts(tableShell, targetCohorts)
  expect_equal(tableShell$getTargetCohorts(), targetCohorts)
})

test_that("addTargetCohortsFromDf adds target cohorts from a data frame", {
  tableShell <- createTableShell("Test TableShell")
  df <- data.frame(id = c(1, 2), name = c("Cohort 1", "Cohort 2"))
  tableShell <- addTargetCohortsFromDf(tableShell, df)
  expect_true(all(sapply(tableShell$getTargetCohorts(), inherits, "TargetCohort")))
})

# test_that("addTargetCohortsFromCsv adds target cohorts from a CSV file", {
#   tableShell <- createTableShell("Test TableShell")
#   file <- "path/to/file.csv"
#   tableShell <- addTargetCohortsFromCsv(tableShell, file)
#   expect_true(all(sapply(tableShell$getTargetCohorts(), inherits, "TargetCohort")))
# })

test_that("addSections adds 2 sections to a TableShell object", {
  tableShell <- createTableShell("Test TableShell")
  section1 <- Section$new()
  section2 <- Section$new()
  tableShell <- addSections(tableShell, section1, section2)
  expect_equal(tableShell$getSections(), list(section1, section2))
})

test_that("addSections adds 1 section to a TableShell object", {
  tableShell <- createTableShell("Test TableShell")
  section1 <- Section$new()
  tableShell <- addSections(tableShell, section1)
  expect_equal(tableShell$getSections(), list(section1))
})

test_that("setExecutionSettings sets the ExecutionSettings of a TableShell object", {
  tableShell <- createTableShell("Test TableShell")
  es <- ExecutionSettings$new(connectionDetails = "fake_connection_details",
                              connection = NULL,
                              cdmDatabaseSchema = "fake_cdm_database_schema",
                              workDatabaseSchema = "fake_work_database_schema",
                              tempEmulationSchema = "fake_temp_emulation_schema",
                              targetCohortTable = "fake_target_cohort_table",
                              cdmSourceName = "fake_cdm_source_name",
                              numThreads = 4)
  tableShell <- setExecutionSettings(tableShell, es)
  expect_equal(tableShell$getExecutionSettings(), es)
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
  section <- createSection("Test Section", 1)
  expect_true(inherits(section, "Section"))
  expect_equal(section$getTitle(), "Test Section")
  expect_equal(section$getOrdinal(), 1)
})

test_that("addLineItems adds line items to a Section object", {
  section <- createSection("Test Section", 1)
  lineItem1 <- createLineItem("Line Item 1", 1)
  lineItem2 <- createLineItem("Line Item 2", 2)
  section <- addLineItems(section, lineItem1, lineItem2)
  expect_equal(section$getLineItems(), list(lineItem1, lineItem2))
})

test_that("addLineItems adds 1 line item to a Section object", {
  section <- createSection("Test Section", 1)
  lineItem1 <- createLineItem("Line Item 1", 1)
  section <- addLineItems(section, lineItem1)
  expect_equal(section$getLineItems(), list(lineItem1))
})

test_that("createLineItem returns a LineItem object with the correct attributes", {
  ordinal <- 1
  label <- "Test LineItem"

  lineItem <- createLineItem(label, ordinal)

  expect_true(inherits(lineItem, "LineItem"))
  expect_equal(lineItem$getOrdinal(), ordinal)
  expect_equal(lineItem$getLabel(), label)
})
