library(testthat)

test_that("createTableShell returns a TableShell object with the correct name", {
  targetCohort <- CohortInfo$new(id = 1, name = "Cohort 1")
  stat <- Statistic$new(label = "test", type = "test")
  lineItem <- LineItem$new(
    sectionLabel = "sectionLabel",
    domain = "Hello",
    lineItemClass = "lineItemClass",
    statistic = stat
  )
  tableShell <- createTableShell(title = "Table 1",
                                 targetCohorts = list(targetCohort),
                                 lineItems = list(lineItem))
  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getName(), "Table 1")
})

test_that("parseCohortInfoFromDf parses cohort info from a data frame", {
  df <- data.frame(id = c(1, 2), name = c("Cohort 1", "Cohort 2"))
  cohorts <- parseCohortInfoFromDf(df)
  expect_true(all(sapply(cohorts, inherits, "CohortInfo")))
})

test_that("createCohortInfo returns a CohortInfo object with the correct attributes", {
  id <- 1
  name <- "Test Cohort"
  cohortInfo <- createCohortInfo(id, name)
  expect_true(inherits(cohortInfo, "CohortInfo"))
  expect_equal(cohortInfo$getId(), id)
  expect_equal(cohortInfo$getName(), name)
})

test_that("createExecutionSettings returns an ExecutionSettings object", {
  executionSettings <- createExecutionSettings(connectionDetails = connectionDetailsEunomia,
                                              connection = NULL,
                                              cdmDatabaseSchema = "cdm_schema",
                                              workDatabaseSchema = "work_schema",
                                              tempEmulationSchema = "fake_temp_emulation_schema",
                                              targetCohortTable = "fake_target_cohort_table",
                                              cdmSourceName = "fake_cdm_source_name")
  expect_true(inherits(executionSettings, "ExecutionSettings"))
})

test_that("createConceptSetLineItem creates a ConceptSetLineItem object", {
  csLi <- createConceptSetLineItem(sectionLabel = "Concept Set 1",
                                   statistic = CategoricalPresence$new(">=", 1),
                                   domain = "drug_exposure",
                                   conceptSet = Capr::cs(1335471, name = "test"),
                                   timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(csLi, "ConceptSetLineItem"))
  expect_true(inherits(csLi, "LineItem"))
  expect_equal(csLi$lineItemClass, "ConceptSet")
})

test_that("createConceptSetLineItem creates a ConceptSetLineItem object - no name specified", {
  csLi <- createConceptSetLineItem(statistic = CategoricalPresence$new(">=", 1),
                                   domain = "Drug",
                                   conceptSet = Capr::cs(1335471, name = "test"),
                                   timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(csLi, "ConceptSetLineItem"))
  expect_true(inherits(csLi, "LineItem"))
  expect_equal(csLi$lineItemClass, "ConceptSet")
  expect_equal(csLi$lineItemLabel, "test")
})

test_that("createCohortLineItem creates a CohortLineItem object", {
  cohortInfo <- CohortInfo$new(id = 1, name = "Test Cohort")
  cLi <- createCohortLineItem(sectionLabel = "Cohort 1",
                              statistic = CategoricalPresence$new(">=", 1),
                              covariateCohort = cohortInfo,
                              cohortTable = "test_cohort_table",
                              timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(cLi, "CohortLineItem"))
  expect_true(inherits(cLi, "LineItem"))
  expect_equal(cLi$lineItemClass, "Cohort")
})

test_that("createCohortLineItem creates a CohortLineItem object - no name specified", {
  cohortInfo <- CohortInfo$new(id = 1, name = "Test Cohort")
  cLi <- createCohortLineItem(statistic = CategoricalPresence$new(">=", 1),
                              covariateCohort = cohortInfo,
                              cohortTable = "test_cohort_table",
                              timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(cLi, "CohortLineItem"))
  expect_true(inherits(cLi, "LineItem"))
  expect_equal(cLi$lineItemClass, "Cohort")
  expect_equal(cLi$lineItemLabel, "Test Cohort")
})

test_that("createConceptSetLineItemBatch creates a list of ConceptSetLineItem objects", {
  conceptSets <- list(Capr::cs(1335471, name = "A"), Capr::cs(1340128, name = "B"), Capr::cs(1341927, name = "C"))
  csList <- createConceptSetLineItemBatch(sectionLabel = "Drug Exposures",
                                          statistic = CategoricalPresence$new(">=", 1),
                                          domain = "drug_exposure",
                                          conceptSets = conceptSets,
                                          timeIntervals = list(TimeInterval$new(0,365)))
  expect_equal(length(csList), 3)
  expect_true(inherits(csList[[1]], "ConceptSetLineItem"))
  expect_true(inherits(csList[[1]], "LineItem"))
  expect_equal(csList[[1]]$lineItemLabel, "A")
})

test_that("createCohorttLineItemBatch creates a list of CohortLineItem objects", {
  cohorts <- list(CohortInfo$new(id = 1, name = "A"), CohortInfo$new(id = 2, name = "B"))
  cohortList <- createCohortLineItemBatch(sectionLabel = "Cohort Batch",
                                          statistic = CategoricalPresence$new(">=", 1),
                                          covariateCohorts = cohorts,
                                          cohortTable = "test_cohort_table",
                                          timeIntervals = list(TimeInterval$new(0,365)))
  expect_equal(length(cohortList), 2)
  expect_true(inherits(cohortList[[1]], "CohortLineItem"))
  expect_true(inherits(cohortList[[1]], "LineItem"))
  expect_equal(cohortList[[1]]$lineItemLabel, "A")
})
