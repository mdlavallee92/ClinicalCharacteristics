targetCohorts <- list(TargetCohort$new(id = 1, name = "someCondition"))
lineItems <- list(
  GenderDefinition$new(name = "Female", genderConceptIds = c(8532)),
  GenderDefinition$new(name = "Male", genderConceptIds = c(8507)),
  ConceptSetGroupDefinition$new(ConceptSetLineItems =
                                  list(ConceptSetLineItem$new(name = "Tobacco", domainIds = c("Condition", "Observation"), conceptSetId = 99,
                                                                caprConceptSet = Capr::cs(12345, name = "Tobacco")),
                                       ConceptSetLineItem$new(name = "Alcohol", domainIds = c("Condition", "Observation"), conceptSetId = 100,
                                                                caprConceptSet = Capr::cs(99999, name = "Alcohol")))
                                ),
  CohortDefinition$new(name = "Major bleeding", cohortDefinitionId = 123,
                       cohortDatabaseSchema = "eunomia_results", cohortTable = "cohort")
)
sections <- list(Section$new(name = "Demos", ordinal = 1, lineItems = lineItems))
tableShell <- TableShell$new(name = "test", sections = sections, targetCohorts = targetCohorts)



