targetCohorts <- list(TargetCohort$new(id = 1, name = "someCondition"))
lineItems <- list(
  GenderDefinition$new(name = "Female", genderConceptIds = c(8532)),
  GenderDefinition$new(name = "Male", genderConceptIds = c(8507))
)
sections <- list(Section$new(name = "Demos", ordinal = 1, lineItems = lineItems))
tableShell <- TableShell$new(name = "test", sections = sections, targetCohorts = targetCohorts)
