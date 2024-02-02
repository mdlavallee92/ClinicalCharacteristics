domain_to_id <- function(domain = c("Drugs", "Conditions")) {

  #TODO add domains
  domain <- match.arg(domain)
  switch(domain,
         Drugs = "410",
         Conditions = "210")

}

pluck_characteristic <- function(folder, domain, timeWindow = NULL, ids) {

  if (!is.null(timeWindow)) {
    file <- glue::glue("{domain}_{timeWindow}.csv")
  } else{
    file <- glue::glue("{domain}.csv")
  }

  # make file

  # set path
  filePath <- fs::path(folder, file)
  #read in csv
  dt <- readr::read_csv(
    file = filePath,
    show_col_types = FALSE
  )


  #make the covariate id
  covariateIds <- paste0(conceptIds, domain_to_id(domain)) |> as.integer()

  #build snippet
  snippet <- dt |>
    dplyr::filter(
      covariateId %in% covariateIds
    ) |>
    dplyr::mutate(
      averageValue = averageValue * 100
    ) |>
    dplyr::select(
      -c(analysisId, covariateId)
    )

  return(snippet)

}
