# ExecutionSettings ----

#' @description
#' An R6 class to define an ExecutionSettings object
#'
#' @export
ExecutionSettings <- R6::R6Class("ExecutionSettings",
 public = list(
   connectionDetails = NULL,
   connection = NULL,
   cdmDatabaseSchema = NULL,
   workDatabaseSchema = NULL,
   tempEmulationSchema = NULL,
   targetCohortTable = NULL,
   cdmSourceName = NULL,
   numThreads = NULL,
   initialize = function(connectionDetails = NULL,
                         connection = NULL,
                         cdmDatabaseSchema = NULL,
                         workDatabaseSchema = NULL,
                         tempEmulationSchema = NULL,
                         targetCohortTable = NULL,
                         cdmSourceName = NULL,
                         numThreads = NULL) {
     stopifnot(is.null(connectionDetails) || is.null(connection))
     checkmate::assert_string(x = connectionDetails, na.ok = TRUE, null.ok = TRUE)
     checkmate::assert_string(x = connection, na.ok = TRUE, null.ok = TRUE)
     checkmate::assert_string(x = cdmDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
     checkmate::assert_string(x = workDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
     checkmate::assert_string(x = tempEmulationSchema, na.ok = TRUE, null.ok = TRUE)
     checkmate::assert_string(x = targetCohortTable, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
     checkmate::assert_string(x = cdmSourceName, na.ok = TRUE, null.ok = TRUE)
     checkmate::assert_number(x = numThreads, na.ok = TRUE, null.ok = TRUE)

     if (!is.null(connection)) {
       self$connection <- connection
       self$numThreads <- 1
     } else {
       self$connectionDetails <- connectionDetails
       self$numThreads <- numThreads
     }

     self$cdmDatabaseSchema <- cdmDatabaseSchema
     self$workDatabaseSchema <- workDatabaseSchema
     self$tempEmulationSchema <- tempEmulationSchema
     self$targetCohortTable <- targetCohortTable
     self$cdmSourceName <- cdmSourceName
   }
 )
)


# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    initialize = function(name,
                          sections,
                          targetCohorts) {
      .setString(private = private, key = "name", value = name)
      .setListofClasses(private = private, key = "targetCohorts", value = targetCohorts, classes = c("TargetCohort"))
      .setListofClasses(private = private, key = "sections", value = sections, classes = c("Section"))
    },
    getTitle = function() {
      tsTitle <- private$name
      return(tsTitle)
    },
    getTargetCohorts = function() {
      tsTargetCohorts <- private$targetCohorts
      return(tsTargetCohorts)
    },
    getSections = function() {
      tsSections <- private$sections
      return(tsSections)
    }
  ),
  private = list(
    name = NULL,
    sections = NULL,
    targetCohorts = NULL
  )
)

# Target Cohort -----

#' @description
#' An R6 class to define a Target Cohort object
#' TargetCohort objects do not maintain any execution settings, just the id and name
#'
#' @export
TargetCohort <- R6::R6Class("TargetCohort",
  public = list(
    initialize = function(id, name) {
      .setNumber(private = private, key = "id", value = id)
      .setString(private = private, key = "name", value = name)
    },
    getId = function() {
      tcId <- private$id
      return(tcId)
    },
    getName = function(name) {
      tcName <- private$name
      return(tcName)
    }
  ),
  private = list(
    id = NULL,
    name = NULL
  )
)

# Section ------

#' @description
#' An R6 class to define a Section object
#' A Section object is a logical encapsulation of LineItems
#' Sections have names and ordinals for the final output
#'
#' @export
Section <- R6::R6Class("Section",
  public = list(
    initialize = function(name,
                          ordinal,
                          lineItems) {
      .setString(private = private, key = "name", value = name)
      .setNumber(private = private, key = "ordinal", value = ordinal)
      .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
    },
    getName = function() {
      name <- private$name
      return(name)
    },
    getOrdinal = function() {
      sectionOrdinal <- private$ordinal
      return(sectionOrdinal)
    },
    getLineItems = function() {
      sectionLineItems <- private$lineItems
      return(sectionLineItems)
    }
  ),
  private = list(
    name = NULL,
    ordinal = NA,
    lineItems = NULL
  )
)

# LineItem -----

#' @description
#' An R6 class to define a LineItem object
#' A LineItem is an explicitly defined statistic to appear in a Section
#' LineItems have names, ordinals, StatisticTypes, DomainIds, Limits
#' Derived classes exist off of LineItems
#'
#' @export
LineItem <- R6::R6Class("LineItem",
  public = list(
    setName = function(name) {
      .setString(private = private, key = "name", value = name)
      invisible(self)
    },
    getName = function() {
      name <- private$name
      return(name)
    },
    setOrdinal = function(ordinal) {
      .setNumber(private = private, key = "ordinal", value = ordinal)
      invisible(self)
    },
    setDomainIds = function(domainIds) {
      theseChoices <- .getAssertChoices(category = "DomainId")
      .setChoiceList(private = private, key = "domainIds", value = domainIds, choices = theseChoices)
      invisible(self)
    },
    getOrdinal = function() {
      liOrdinal <- private$ordinal
      return(liOrdinal)
    },
    setStatisticType = function(statisticType) {
      theseChoices <- .getAssertChoices(category = "StatisticType")
      .setChoice(private = private, key = "statisticType", value = statisticType, choices = theseChoices)
      invisible(self)
    },
    getStatisticType = function() {
      liStatType <- private$statisticType
      return(liStatType)
    },
    getDomainIds = function() {
      domainIds <- private$domainIds
      return(domainIds)
    },
    setLimit = function(limit) {
      theseChoices <- .getAssertChoices(category = "Limit")
      .setChoice(private = private, key = "limit", value = limit, choices = theseChoices)
      invisible(self)
    },
    getLimit = function() {
      liLimit <- private$limit
      return(liLimit)
    },
    setTemplateSql = function(templateSql) {
      .setString(private = private, key = "templateSql", value = templateSql)
      invisible(self)
    },
    getTemplateSql = function() {
      templateSql <- private$templateSql
      return(templateSql)
    },
    setConceptIds = function(conceptIds) {
      .setNumber(private = private, key = "conceptIds", value = conceptIds)
      invisible(self)
    },
    getConceptIds = function() {
      conceptIds <- private$conceptIds
      return(conceptIds)
    },
    setMinThreshold = function(minThreshold) {
      .setNumber(private = private, key = "minThreshold", value = minThreshold)
      invisible(self)
    },
    getMinThreshold = function() {
      minThreshold <- private$minThreshold
      return(minThreshold)
    },
    setMaxThreshold = function(maxThreshold) {
      .setNumber(private = private, key = "maxThreshold", value = maxThreshold)
      invisible(self)
    },
    getMaxThreshold = function() {
      maxThreshold <- private$maxThreshold
      return(maxThreshold)
    },
    setAssetId = function(assetId) {
      .setNumber(private = private, key = "assetId", value = assetId)
      invisible(self)
    },
    getAssetId = function() {
      assetId <- private$assetId
      return(assetId)
    }
  ),
  private = list(
    name = NULL,
    assetId = NA,
    ordinal = NA,
    statisticType = NULL,
    domainIds = NULL,
    limit = NULL,
    templateSql = NULL,
    conceptIds = c(),
    minThreshold = NA,
    maxThreshold = NA
  )
)

# GenderDefinition -----

#' @description
#' An R6 class to define a GenderDefinition object.
#'
#' @export
GenderDefinition <- R6::R6Class("GenderDefinition",
  inherit = LineItem,
  public = list(
    initialize = function(name,
                          genderConceptIds) {
      super$setDomainIds(domainIds = "Gender")
      super$setName(name = name)
      super$setConceptIds(conceptIds = genderConceptIds)

      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
      super$setTemplateSql(templateSql = templateSql)
    },
    getGenderConceptIds = function() {
      genderConceptIds <- private$genderConceptIds
      return(genderConceptIds)
    }
  )
)

# AgeDefinition -----

#' @description
#' An R6 class to handle the ...
#'
#' @export
AgeDefinition <- R6::R6Class("AgeDefinition",
   inherit = LineItem,
   public = list(
     initialize = function(name,
                           minAge = NULL,
                           maxAge = NULL) {
       super$setName(name = name)
       super$setDomainIds(domainIds = "Age")
       super$setMinThreshold(minThreshold = minAge)
       super$setMaxThreshold(maxThreshold = maxAge)

       templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
       super$setTemplateSql(templateSql = templateSql)
     }
   )
)


# IndexYearDefinition -----

#' @description
#' An R6 class to define a IndexYearDefinition object.
#'
#' @export
IndexYearDefinition <- R6::R6Class("IndexYearDefinition",
  inherit = LineItem,
  public = list(
    initialize = function(name = indexYear,
                          indexYear) {
      super$setName(name = name)
      super$setDomainIds(domainIds = "IndexYear")
      super$setMinThreshold(minThreshold = indexYear)
      super$setMaxThreshold(maxThreshold = indexYear) # is it fine to just stuff this in min and max thresholds?

      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
      super$setTemplateSql(templateSql = templateSql)
    }
  )
)


# ConceptSetDefinition ----

#' @description
#' An R6 class to define a ConceptSetDefinition
#' A line item that uses a Concept Set JSON
#'
#' @export
ConceptSetDefinition <- R6::R6Class("ConceptSetDefinition", list(
  inherit = LineItem,
  public = list(
    initialize = function(name,
                          domainIds,
                          conceptSetId) {
      super$setName(name = name)
      super$setDomainIds(domainIds = domainIds)
      super$setAssetId(assetId = conceptSetId)

      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
      super$setTemplateSql(templateSql = templateSql)
    }
  )
))

# ConceptSetGroupDefinition ----

#' @description
#' An R6 class to define a ConceptSetGroupDefinition
#' A line item that uses a Concept Set Group
#'
#' @export
ConceptSetGroupDefinition <- R6::R6Class("ConceptSetGroupDefinition", list(
  inherit = LineItem,
  public = list(
    initialize = function(conceptSetDefinitions) {
      .setListofClasses(private = private, key = "conceptSetDefinitions", value = conceptSetDefinitions, classes = c("ConceptSet"))
    },
    getConceptSetDefinitions = function() {
      conceptSetDefinitions <- private$conceptSetDefinitions
      return(conceptSetDefinitions)
    }
  ),
  private = list(
    conceptSetDefinitions = c()
  )
))

# CohortDefinition ----

#' @description
#' An R6 class to define a CohortDefinition
#'
#' @export
CohortDefinition <- R6::R6Class("CohortDefinition",
  public = list(
    initialize = function(name,
                          cohortDefinitionId) {
      super$setName(name = name)
      super$setDomainIds(domainIds = "Cohort")
      super$setAssetId(assetId = cohortDefinitionId)

      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
      super$setTemplateSql(templateSql = templateSql)
    }
  )
)


# RaceDefinition ------

#' @description
#' An R6 class to define a RaceDefinition object.
#'
#' @export
RaceDefinition <- R6::R6Class("RaceDefinition", list(
  inherit = LineItem,
  public = list(
    initialize = function(name,
                          raceConceptIds) {
      super$setDomainIds(domainIds = "Race")
      super$setName(name = name)
      super$setConceptIds(conceptIds = raceConceptIds)

      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
      super$setTemplateSql(templateSql = templateSql)
    },
    getGenderConceptIds = function() {
      genderConceptIds <- private$genderConceptIds
      return(genderConceptIds)
    }
  )
))

######## MORE TO-DO #####

# ValueDefinition ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
ValueDefinition <- R6::R6Class("ValueDefinition", list(
  inherit = LineItem,

  domainIds = c(),
  thresholdMin = NA,
  thresholdMax = NA,
  unitConceptIds = c(),
  unitConversions = c()
))

# UnitConversion ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
UnitConversion <- R6::R6Class("UnitConversion", list(
  originalUnitConceptId = NA,
  targetUnitConceptId = NA,
  multiplierToOriginal = NA
))

# BreaksStrategy ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
BreaksStrategy <- R6::R6Class("BreaksStrategy", list(
  name = NULL,
  breaks = NULL,
  initialize = function(name,
                        breaks) {
    self$name <- name
    self$breaks <- breaks
  }
))

