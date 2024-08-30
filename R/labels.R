# function to make an empty labels
.emptyLabels <- function() {
  tb <- tibble::tibble(
    categoryId = NA_integer_,
    categoryLabel = NA_character_,
    timeId = NA_integer_,
    twLabel = NA_character_,
    valueId = NA_integer_,
    name = NA_character_
  )
  return(tb)
}


.makeTargetCohortLabels <- function(targetCohort) {
  targetCohortKey <- tibble::tibble(
    cohort_id = purrr::map_int(targetCohort, ~.x$getId()),
    cohort_name = purrr::map_chr(targetCohort, ~.x$getName())
  )
  return(targetCohortKey)
}

# function to make concept set labels
.makeConceptSetLabels <- function(categoryKey, conceptSetMeta) {
  if (!is.null(conceptSetMeta)) {
    csLabels <- conceptSetMeta |>
      dplyr::left_join(
        categoryKey, by = c("categoryId")
      ) |>
      dplyr::mutate(
        categoryLabel = glue::glue("{snakecase::to_title_case(domain)}: {statType}"),
        valueId = csId,
        timeId = twId
      ) |>
      dplyr::select(
        categoryId, categoryLabel, timeId, twLabel, valueId, name
      )
  } else{
    csLabels <- .emptyLabels()
  }

  return(csLabels)
}
