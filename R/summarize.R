# Merge Keys ----------------------------
cohort_key <- function(clinChar) {
  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )
  return(cohortKey)
}

cat_name <- function(slot) {

  cl <- class(slot)
  dm <- slot@domain
  if (cl %in% c("presenceChar", "labChar", "countChar", "costChar", "timeToChar")) {
    catName <- glue::glue("{gsub('Char', '', cl)}_{dm}")
  } else{
    catName <- dm
  }

  return(catName)
}

category_key <- function(clinChar){

  catName <- purrr::map_chr(clinChar@extractSettings, ~cat_name(.x))
  catId <- purrr::map_int(clinChar@extractSettings, ~.x@orderId)

  categoryKey <- tibble::tibble(
    category_id = catId,
    category_name = catName
  )
  return(categoryKey)
}

# Categorize Value ----------------------

categorize_value <- function(tbl, breaksKey) {
  tb <- tbl |>
    dplyr::left_join(breaksKey, by = "value") |>
    dplyr::mutate(
     value = 1,
     value_id = grp
    ) |>
    dplyr::select(-c(grp))
  return(tb)
}

#

# Summary --------------------

summarize_continuous <- function(tbl) {

  tb <- tbl |>
    dplyr::group_by(cohort_id, category_id, time_id, value_id) |>
    dplyr::summarize(
      n = dplyr::n_distinct(subject_id),
      mean = mean(value),
      sd = sd(value),
      min_value = min(value),
      p25_value = quantile(value, probs = c(0.25)),
      median_value = median(value),
      p75_value = quantile(value, probs = c(0.75)),
      max_value = max(value)
    ) |>
    dplyr::ungroup() |>
    dplyr::collect()

  return(tb)
}

summarize_categorical <- function(tbl, clinChar) {

  tb <- tbl |>
    dplyr::group_by(cohort_id, category_id, time_id, value_id) |>
    dplyr::summarize(
      sum_value = sum(value)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(retrieveCount(clinChar), by = "cohort_id") |>
    dplyr::mutate(
      sum_value = as.numeric(sum_value),
      average_value = sum_value / total
    ) |>
    dplyr::select(-c(total)) |>
    dplyr::collect()

  return(tb)
}

# Labels ------------------------

set_time_labels <- function(clinChar) {

  timeKey <- time_key(clinChar)

  if (nrow(timeKey) == 0) {
    timeKey <- tibble::tibble(
      time_id = -999,
      time_name = "Static from Index"
    )
  } else {
    timeKey <- timeKey |>
      dplyr::mutate(
        time_name = glue::glue("{time_a}d:{time_b}d")
      ) |>
      dplyr::select(
        -c(time_a, time_b)
      ) |>
      tibble::add_row(
        time_id = -999,
        time_name = "Static from Index"
      )
  }
  return(timeKey)
}


label_table <- function(tbl, clinChar) {

  timeKey <- set_time_labels(clinChar)

  lbl_tbl <- tbl |>
    dplyr::left_join(
      cohort_key(clinChar), by = "cohort_id"
    ) |>
    dplyr::left_join(
      category_key(clinChar), by = "category_id"
    ) |>
    dplyr::left_join(
      timeKey, by = "time_id"
    ) |>
    dplyr::relocate(
      cohort_name, .after = cohort_id
    ) |>
    dplyr::relocate(
      category_name, .after = category_id
    ) |>
    dplyr::relocate(
      time_name, .after = time_id
    )

  if (is.factor(lbl_tbl$value_id)) {
    lbl_tbl <- lbl_tbl |>
      dplyr::mutate(
        value_name = as.character(value_id),
        value_id = as.numeric(value_id)
      ) |>
      dplyr::relocate(
        value_name, .after = value_id
      )
  }


  return(lbl_tbl)
}

# Sum Char -------------------------

setGeneric("sum_char", function(x, clinChar)  standardGeneric("sum_char"))

setMethod("sum_char", "demoConceptChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  demo_labels <- rlang::call2(glue::glue("{domain}_categories")) |>
    eval()

  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_categorical(clinChar) |>
    label_table(clinChar) |>
    dplyr::left_join(
      demo_labels, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )
  return(tb)

})


setMethod("sum_char", "ageChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::mutate(
      value_name = "age"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = 1) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

setMethod("sum_char", "locationChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_categorical(clinChar) |>
    label_table(clinChar)
  return(tb)

})


setMethod("sum_char", "labChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar)
  return(tb)

})


setMethod("sum_char", "presenceChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  codesetKey <- tibble::tibble(
    value_id = x@tempTables$codeset,
    value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
  )

  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_categorical(clinChar) |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )
  return(tb)

})




setMethod("sum_char", "costChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )


  if (!is.null(x@conceptSets)) {
    codesetKey <- tibble::tibble(
      value_id = x@tempTables$codeset,
      value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
    )
  } else {
    codesetKey <- cost_categories()
  }

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = 1) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})


setMethod("sum_char", "countChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  if (!is.null(x@conceptSets)) {
    codesetKey <- tibble::tibble(
      value_id = x@tempTables$codeset,
      value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
    )
  } else {
    codesetKey <- tibble::tibble(
      value_id = -999,
      value_name = count_label(domain)
    )
  }

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = 1) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

setMethod("sum_char", "timeInChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain


  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      time_in_label(), by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = 1) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})


setMethod("sum_char", "timeToChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain

  codesetKey <- tibble::tibble(
    value_id = x@tempTables$codeset,
    value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
  )

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = 1) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})


#' Summarizes the characterization and pulls into R
#' @description
#' This summarizes the characterization specified by the clinChar object
#' @param clinChar the clinChar object describing the study
#' @return pulls the aggregated data into R
#' @export
tabulateClinicalCharacteristics <- function(clinChar) {

  es <- clinChar@extractSettings
  tb <- purrr::map(es, ~sum_char(.x, clinChar = clinChar)) |>
    purrr::flatten() |>
    purrr::compact()

  # get indicies for continuous and categorical
  cts_idx <- which(names(tb) == "continuous")
  cat_idx <- which(names(tb) == "categorical")

  cts_tb <- tb[cts_idx] |> purrr::list_rbind()
  cat_tb <- tb[cat_idx] |>
    purrr::list_rbind()

  tb <- list(
    'continuous' = cts_tb,
    'categorical' = cat_tb
  )

  return(tb)
}


#' Preview the characterization
#' @description
#' This shows the characterization tables using reactable
#' @param tb the table created from the tabulateClincalCharacteristics
#' @param type which table to preview
#' @return presents the reactable summarizing the characterization
#' @export
previewClincalCharacteristics <- function(tb, type = c("categorical", "continuous")) {


  if (type == "categorical") {
    cat_dat <- tb$categorical |>
      dplyr::mutate(
        cohort_name = snakecase::to_title_case(cohort_name),
        category_name = snakecase::to_title_case(category_name)
      )

    res_tb <- reactable::reactable(
      data = cat_dat,
      columns = list(
        'cohort_id' = reactable::colDef(name = "Cohort Id"),
        'cohort_name' = reactable::colDef(name = "Cohort Name"),
        'category_id' = reactable::colDef(name = "Category Id"),
        'category_name' = reactable::colDef(name = "Category Name"),
        'time_id' = reactable::colDef(name = "Time Id"),
        'time_name' = reactable::colDef(name = "Time Name"),
        'value_id' = reactable::colDef(name = "Value Id"),
        'value_name' = reactable::colDef(name = "Value Name"),
        'sum_value' = reactable::colDef(
          name = "N", format = reactable::colFormat(separators = TRUE)
        ),
        'average_value' = reactable::colDef(
          name = "pct", format = reactable::colFormat(percent = TRUE, digits = 2)
        )
      ),
      highlight = TRUE,
      bordered = TRUE,
      outlined = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE
    )
  }

  if (type == "continuous") {

    cts_dat <- tb$continuous |>
      dplyr::mutate(
        cohort_name = snakecase::to_title_case(cohort_name),
        category_name = snakecase::to_title_case(category_name)
      )

    res_tb <- reactable::reactable(
      data = cts_dat,
      columns = list(
        'cohort_id' = reactable::colDef(name = "Cohort Id"),
        'cohort_name' = reactable::colDef(name = "Cohort Name"),
        'category_id' = reactable::colDef(name = "Category Id"),
        'category_name' = reactable::colDef(name = "Category Name"),
        'time_id' = reactable::colDef(name = "Time Id"),
        'time_name' = reactable::colDef(name = "Time Name"),
        'value_id' = reactable::colDef(name = "Value Id"),
        'value_name' = reactable::colDef(name = "Value Name"),
        'n' = reactable::colDef(
          name = "N", format = reactable::colFormat(separators = TRUE)
        ),
        'mean' = reactable::colDef(
          name = "Mean", format = reactable::colFormat(separators = TRUE, digits = 2)
        ),
        'sd' = reactable::colDef(
          name = "SD", format = reactable::colFormat(separators = TRUE, digits = 2)
        ),
        'min_value' = reactable::colDef(
          name = "Min", format = reactable::colFormat(separators = TRUE)
        ),
        'p25_value' = reactable::colDef(
          name = "25th", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'median_value' = reactable::colDef(
          name = "Median", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'p75_value' = reactable::colDef(
          name = "75th", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'max_value' = reactable::colDef(
          name = "Max", format = reactable::colFormat(separators = TRUE)
        )
      ),
      highlight = TRUE,
      bordered = TRUE,
      outlined = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE
    )
  }

  return(res_tb)
}
