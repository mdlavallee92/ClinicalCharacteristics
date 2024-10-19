LineItem <- R6::R6Class(
  classname = "LineItem",
  public = list(
    initialize = function(
      sectionLabel,
      lineItemLabel,
      domainTable,
      lineItemClass,
      valueDescription,
      statistic,
      timeInterval
    ) {
      .setString(private = private, key = "sectionLabel", value = sectionLabel)
      .setString(private = private, key = "lineItemLabel", value = lineItemLabel)
      .setString(private = private, key = "domainTable", value = domainTable)
      .setString(private = private, key = "lineItemClass", value = lineItemClass)
      .setString(private = private, key = "valueDescription", value = valueDescription)
      .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")
      .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval", nullable = TRUE)
    },

    getLineItemMeta = function() {

      tw <- private$TimeInterval
      if (is.null(tw)) {
        timeLabel <- "Static at Index"
      } else {
        timeLabel <- "-365d to -1d" #tw$getTimeLabels()
      }

      tb <- tibble::tibble(
        ordinalId = private$ordinalId,
        sectionLabel = private$sectionLabel,
        linetItemLabel = private$lineItemLabel,
        valueId = private$valueId,
        valueDescription = private$valueDescription,
        timeLabel = timeLabel,
        statisticType = class(private$statistic)[[1]],
        domainTable = private$domainTable,
        lineItemClass = private$lineItemClass
      )

      return(tb)
    }

  ),
  private = list(
    ordinalId = NA_integer_,
    sectionLabel = NA_character_,
    lineItemLabel = NA_character_,
    valueId = NA_integer_,
    valueDescription = NA_character_,
    timeInterval = NULL,
    statistic = NULL,
    domainTable = NA_character_,
    lineItemClass = NA_character_
  ),
  active = list()
)


ConceptSetLineItem <- R6::R6Class(
  classname = "ConceptSetLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(
      sectionLabel,
      domainTable,
      conceptSet,
      timeInterval,
      statistic,
      sourceConceptSet = NULL,
      typeConceptIds = c(),
      visitOccurrenceConceptIds = c()
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "ConceptSet",
        valueDescription = "codeset_id",
        statistic = statistic,
        lineItemLabel = conceptSet@Name,
        timeInterval = timeInterval
        )

      .setClass(private = private, key = "conceptSet", value = conceptSet, class = "ConceptSet")
      #.setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval", nullable = TRUE)
      # TODO change this to enforce domain from choice list
      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
      .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
      .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)

    }
  ),
  private = list(
    conceptSet = NULL,
    sourceConceptSet = NULL,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()
  ),
  active = list()
)
